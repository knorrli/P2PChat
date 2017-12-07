-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").

-define(MSG_BUFFER_SIZE, 20).

% Commands
-define(REFRESH, "!r").
-define(NUMBER, "N").
-define(QUIT, "!q").

run() ->
  {ok, [Cookie|Nodes]} = file:consult('./enodes.conf'),

  Username = get_username(),

  net_kernel:start([Username, longnames]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  erlang:set_cookie(node(), ACookie),

  ChosenNode = choose_node(Nodes),
  ConnectedNode = connect_client(Username, ChosenNode),
  spawn_link(ui, prompt, [self()]),
  maintain_connection(ConnectedNode, Username, "Connected to Network", []).

maintain_connection(ConnectedNode, Username, Status, MsgBuffer) ->
  render_ui(ConnectedNode, Username, Status, MsgBuffer),
  receive
    quit ->
      quit(ConnectedNode, Username);
    refresh ->
      maintain_connection(ConnectedNode, Username, "Refresh UI", MsgBuffer);
    {parse_msg, Input} ->
      case string:to_integer(Input) of
        {N, Msg} ->
          PeerName = lists:nth(N, get_available_clients(ConnectedNode)),
          Message = string:strip(Msg),
          send_chat_msg(ConnectedNode, Message, Username, PeerName),
          maintain_connection(ConnectedNode, Username, "Message sent!", add_to_msg_buffer({Username, Message}, MsgBuffer));
        _ ->
          NewStatus = io_lib:format("ERROR: can not parse input: ~s", [Input]),
          maintain_connection(ConnectedNode, Username, NewStatus, MsgBuffer)
      end;
    {incoming_msg, Msg, From} ->
      global:send(observer, {route_msg, self(), From, Username, ConnectedNode, Msg}),
      maintain_connection(ConnectedNode, Username, "Message received!", add_to_msg_buffer({From, Msg}, MsgBuffer))
  end.


render_ui(ConnectedNode, Username, Status, MessageBuffer) ->
  io:format(os:cmd(clear)),

  Separator = io_lib:format("---------------------------------------------------------------------------------------------~n",[]),
  Header = io_lib:format("P2PChat - connected as ~s | ~p~n", [Username, Status]),
  io:format(Separator),
  io:format(Header),
  io:format(Separator),
  io:format("Help:~n~s~n", [help()]),
  io:format(Separator),
  io:format("Peers:~n~s~n", [peers(ConnectedNode)]),
  io:format(Separator),
  io:format("Chat:~n~s~n", [messages(MessageBuffer)]),
  io:format("~s~n", [Separator]).

add_to_msg_buffer({Username, Msg}, Buffer) ->
  {_, {H, M, _}} = erlang:localtime(),
  NewBuffer = [{{H, M}, Username, Msg}|Buffer],
  case length(NewBuffer) > ?MSG_BUFFER_SIZE of
    true -> lists:droplast(NewBuffer);
    false -> NewBuffer
  end.

send_chat_msg(ConnectedNode, Msg, Username, Peername) ->
  try
    global:send(observer, {route_msg, self(), Username, Peername, ConnectedNode, Msg}),
    global:send(ConnectedNode, {route_msg, Username, Peername, Msg})
  catch
    {badarg, _} ->
      % TODO: Error handling
      io:format("ERROR: The chat message could not be sent.")
  end.

connect_client(Username, ConnectedNode) ->
  io:format("Connecting to ~p...~n", [ConnectedNode]),
  net_kernel:connect_node(ConnectedNode),
  % sync global state (although this should happen automatically?)
  io:format("Syncing global state...~n"),
  global:sync(),
  io:format("Global after sync: ~p~n", [global:registered_names()]),
  io:format("Sending ~p ! {connect_client, ~p, ~p}...~n", [ConnectedNode, Username, self()]),
  global:send(ConnectedNode, {connect_client, Username, self()}),
  io:format("Connection established.~n"),
  ConnectedNode.

quit(ConnectedNode, Username) ->
  global:send(ConnectedNode, {disconnect_client, Username, self(), []}),
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

messages(MessageBuffer) ->
  case MessageBuffer of
    [] -> "There are no messages yet.";
    _ ->
      lists:reverse(
        lists:map(
          fun ({{H, M}, Name, Msg}) ->
              io_lib:format("~2..0w:~2..0w | <~p>: ~p~n", [H, M, Name, Msg])
          end,
          MessageBuffer)
       )
  end.

peers(ConnectedNode) ->
  case get_available_clients(ConnectedNode) of
    [] -> "There are no clients available, sorry.";
    Peers ->
      PeersWithIndex = lists:zip(lists:seq(1, length(Peers)), Peers),
      lists:map(fun({I, P}) ->
                    io_lib:format("~p: ~p~n", [I, P])
                end, PeersWithIndex)
  end.

help() ->
  [
    io_lib:format("The following commands are available:~n", []),
    io_lib:format("~p <message> | where N is an integer, send a text message to the client corresponding on the list to N.~n", [?NUMBER]),
    io_lib:format("~p | refresh UI (debugging).~n", [?REFRESH]),
    io_lib:format("~p | quit P2PChat.~n", [?QUIT])
  ].
