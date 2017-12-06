-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").

% Commands
-define(NUMBER, "N").
-define(QUIT, "!q").

run() ->
  {ok, [Cookie|Nodes]} = file:consult('./enodes.conf'),

  Username = get_username(),

  net_kernel:start([Username, longnames]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  erlang:set_cookie(node(), ACookie),

  ChosenNode = choose_node(Node),
  ConnectedNode = connect_client(Username, ChosenNode),
  io:format("ConnectedNode: ~p~n", [ConnectedNode]),
  % we can only access the global information after connecting
  UI = spawn_link(ui, prompt, [self()]),
  io:format("UI link spawned: ~p~n", [UI]),
  maintain_connection(ConnectedNode, Username, "Connected to Network").

maintain_connection(ConnectedNode, Username, Status) ->
  io:format("Maintain connection~nConnectedNode: ~p~nUsername: ~p~nStatus: ~p~n", [ConnectedNode, Username, Status]),
  render_ui(ConnectedNode, Username, Status),
  receive
    quit ->
      quit(ConnectedNode, Username);
    refresh ->
      maintain_connection(ConnectedNode, Username, "Refresh UI");
    {parse_msg, Input} ->
      case string:to_integer(Input) of
        {N, Msg} ->
          PeerName = lists:nth(N, get_available_clients(ConnectedNode)),
          Message = string:strip(Msg),
          send_chat_msg(ConnectedNode, Message, Username, PeerName),
          maintain_connection(ConnectedNode, Username, "Message sent!");
        _ ->
          NewStatus = io_lib:format("ERROR: can not parse input: ~s", [Input]),
          maintain_connection(ConnectedNode, Username, NewStatus)
      end;
    {incoming_msg, Msg, From} ->
      global:send(observer, {route_msg, self(), From, Username, ConnectedNode, Msg}),
      maintain_connection(ConnectedNode, Username, "Message received!")
  end.


render_ui(ConnectedNode, Username, Status) ->
  % io:format(os:cmd(clear)),

  Separator = io_lib:format("---------------------------------------------------------------------------------------------~n",[]),
  Header = io_lib:format("P2PChat - connected as ~s~n", [Username]),
  io:format(Separator),
  io:format(Header),
  io:format(Separator),
  io:format("Status~n~p~n", [Status]),
  io:format(Separator),
  io:format("Help:~n~s~n", [help()]),
  io:format(Separator),
  io:format("Peers:~n~s~n", [peers(ConnectedNode)]),
  io:format("~s~n", [Separator]).

peers(ConnectedNode) ->
  case get_available_clients(ConnectedNode) of
    [] -> "There are no clients available, sorry.";
    Peers ->
      PeersWithIndex = lists:zip(lists:seq(1, length(Peers)), Peers),
      lists:join(
        lists:map(fun({I, P}) ->
                      io_lib:format("~p: ~p~n", [I, P]) end,
                  PeersWithIndex)
       , "")
  end.


connect_client(Username, Node) ->
  io:format("Connecting to ~p...~n", [Node]),
  net_kernel:connect_node(Node),
  % sync global state (although this should happen automatically?)
  io:format("Syncing global state...~n"),
  global:sync(),
  io:format("Global after sync: ~p~n", [global:registered_names()]),
  io:format("Sending ~p ! {connect_client, ~p, ~p}...~n", [ConnectedNode, Username, self()]),
  ConnectedNode ! {connect_client, Username, self()},
  io:format("Connection established.~n"),
  ConnectedNode.

send_chat_msg(ConnectedNode, Msg, Username, Peername) ->
  try
    global:send(observer, {route_msg, self(), Username, Peername, ConnectedNode, Msg}),
    ConnectedNode ! {route_msg, Username, Peername, Msg}
  catch
    {badarg, _} ->
      % TODO: Error handling
      io:format("ERROR: The chat message could not be sent.")
  end.

quit(ConnectedNode, Username) ->
  io:format("~p ! {disconnect_client, ~p, ~p}~n", [ConnectedNode, Username, self()]),
  ConnectedNode ! {disconnect_client, Username, self()},
  receive
    {disconnect_successful, Node} -> io:format("Disconnected from ~p~n", [Node])
  end,
  global:unregister_name(node()),
  init:stop().

get_available_clients(ConnectedNode) ->
  ConnectedNode ! { request_available_clients, self() },
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

help() ->
  [
    io_lib:format("The following commands are available:~n", []),
    io_lib:format("~p | quit P2PChat.~n", [?QUIT]),
    io_lib:format("~p | where N is an integer, send a text message to the client corresponding on the list to N.~n", [?NUMBER])
  ].
