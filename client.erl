-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").

-define(HELP, "!h").
-define(START_CHAT, "!c").
-define(END_CHAT, "!e").
-define(DISCONNECT_CLIENT, "!d").
-define(LIST_USERS, "!l").

run() ->
  {ok, [Cookie|Enodes]} = file:consult('./enodes.conf'),
  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  io:format("init net_kernel~n"),
  net_kernel:start([client, longnames]),

  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  io:format("set cookie to ~p~n", [Cookie]),
  erlang:set_cookie(node(), ACookie),
  io:format("current cookie: ~p~n", [erlang:get_cookie()]),

  ConnectedNode = choose_node(Enodes),
  connect_client(ConnectedNode),
  % we can only access the global information after connecting
  io:format("Welcome to P2PChat. enter !h for help.~n"),
  display_ui(ConnectedNode).

display_ui(ConnectedNode) ->
  prompt(ConnectedNode).

connect_client(Node) ->
  io:format("Connecting to ~p... ", [Node]),
  net_kernel:connect_node(Node),
  % sync global state (although this should happen automatically?)
  global:sync(),
  global:send(Node, {connect_client, self()}),
  io:format("Done.~n").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%

prompt(ConnectedNode) ->
  case io:fread("> ", "~s") of
    {ok, [Input]} -> input_action(ConnectedNode, Input);
    {error, _} ->
      io:format("USER INPUT ERROR! Help!!!")
  end.

input_action(ConnectedNode, Input) ->
  case Input of
    [?START_CHAT|Username] -> start_chat(Username);
    ?DISCONNECT_CLIENT -> disconnect_client(ConnectedNode);
    ?END_CHAT -> end_chat();
    ?HELP -> display_help(ConnectedNode);
    ?LIST_USERS -> list_connected_clients(ConnectedNode);
    _ -> ok
  end.

start_chat(Username) ->
  io:format("You are now chatting with ~p. Type !h for help.~n", [Username]).

end_chat() ->
  ok.

display_help(ConnectedNode) ->
  io:format("HELP~n"),
  io:format("The following commands are available:~n"),
  io:format("~p <username> | start chat with the selected user.~n", [?START_CHAT]),
  io:format("~p | disconnect from the network.~n", [?DISCONNECT_CLIENT]),
  io:format("~p | end chat.~n", [?END_CHAT]),
  io:format("~p | display this help message.~n", [?HELP]),
  io:format("~p | list users.~n", [?LIST_USERS]),
  prompt(ConnectedNode).

disconnect_client(ConnectedNode) ->
  io:format("Node: ~p~n", [ConnectedNode]),
  global:send(ConnectedNode, {disconnect_client, self()}),
  receive
    {disconnect_successful, Node} -> io:format("Disconnected from ~p~n", [Node])
  end,
  ok.

list_connected_clients(ConnectedNode) ->
  ConnectedNode ! { request_available_clients, self() },
  receive
    {available_clients, AvailableClients} ->
      [ io:format("~p: ~p~n", [I, Client]) || {I, Client} <- AvailableClients ]
  end.



% TODO: improve error handling
choose_node(Enodes) ->
  io:format("There are currently ~p Nodes in the network.", [length(Enodes)]),
  io:format("Which one do you want to connect to?~n"),
  EnodeList = lists:zip(lists:seq(1, length(Enodes)), Enodes),
  [ io:format("~2.. B. ~p~n", [I, Enode]) || {I, Enode} <- EnodeList ],
  case io:fread("> ", "~d") of
    {ok, [Int]} ->
      {_, Choice} = lists:keyfind(Int, 1, EnodeList),
      Choice;
    {error, _} ->
      io:format("ERROR: please enter a number between 1 and ~p~n", length(Enodes)),
      choose_node(Enodes)
  end.
