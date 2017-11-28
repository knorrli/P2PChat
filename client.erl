-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").

run() ->
  {ok, [Cookie|Enodes]} = file:consult('./enodes.conf'),
  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  Username = get_username(),

  io:format("init net_kernel~n", []),
  net_kernel:start([Username, longnames]),

  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  io:format("set cookie to ~p~n", [ACookie]),
  erlang:set_cookie(node(), ACookie),
  io:format("current cookie: ~p~n", [erlang:get_cookie()]),

  ConnectedNode = choose_node(Enodes),
  connect_client(Username, ConnectedNode),
  % we can only access the global information after connecting
  spawn(ui, start, [self(), get_available_clients(ConnectedNode)]),

  maintain_connection(ConnectedNode).

maintain_connection(ConnectedNode) ->
  receive
    ping -> ping();
    {start_chat, PeerName} -> start_chat(PeerName);
    {outgoing_msg, Msg, To} -> send_chat_msg(Msg, ConnectedNode, To);
    {incoming_msg, Msg, From} -> ui:render_msg(self(), Msg, From);
    end_chat -> end_chat();
    quit -> quit(ConnectedNode);
    list_users ->
      ui:render_peers(self(), get_available_clients(ConnectedNode))
  end,
  maintain_connection(ConnectedNode).


connect_client(Username, Node) ->
  io:format("Connecting to ~p...~n", [Node]),
  net_kernel:connect_node(Node),
  % sync global state (although this should happen automatically?)
  io:format("Syncing global state...~n"),
  global:sync(),
  io:format("Global after sync: ~p~n", [global:registered_names()]),
  io:format("Calling global:register_name(~p, ~p)~n", [Username, self()]),
  global:register_name(node(), self()),
  io:format("Sending {connect_client} Msg...~n"),
  global:send(Node, {connect_client, node()}),
  io:format("Done.~n").

ping() ->
  io:format("PING~n").

start_chat(PeerName) ->
  ui:render_chat(self(), PeerName).

send_chat_msg(Msg, ConnectedNode, PeerName) ->
  io:format("~p: sending chat msg ~p to ~p~n", [self(), Msg, PeerName]),
  try
    global:send(ConnectedNode, {chat_msg, node(), PeerName, Msg})
  catch
    {badarg, _} ->
      % TODO: Error handling
      io:format("ERROR: The chat message could not be sent.")
  end.

end_chat() ->
  ui:prompt(self()).

quit(ConnectedNode) ->
  global:send(ConnectedNode, {disconnect_client, self()}),
  receive
    {disconnect_successful, Node} -> io:format("Disconnected from ~p~n", [Node])
  end,
  global:unregister_name(node()),
  init:stop().

get_available_clients(ConnectedNode) ->
  global:send(ConnectedNode, { request_available_clients, self() }),
  receive
    {available_clients, AvailableClients} -> AvailableClients
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPERS

% TODO: improve error handling
choose_node(Enodes) ->
  io:format("There are currently ~p Nodes in the network. ", [length(Enodes)]),
  io:format("Which one do you want to connect to?~n"),
  EnodeList = lists:zip(lists:seq(1, length(Enodes)), Enodes),
  [ io:format("~2.. B. ~p~n", [I, Enode]) || {I, Enode} <- EnodeList ],
  case io:fread("> ", "~d") of
    {ok, [Int]} ->
      {_, Choice} = lists:keyfind(Int, 1, EnodeList),
      Choice;
    {error, _} ->
      io:format("ERROR: please enter a number between 1 and ~p~n", [length(Enodes)]),
      choose_node(Enodes)
  end.

get_username() ->
  case io:fread("Please enter your username (atomic): ", "~a") of
    {ok, [Username]} -> Username;
    {error, _} ->
      io:format("ERROR: non-atomic username detected."),
      get_username()
  end.
