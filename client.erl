-module(client).
-export([run/0]).

-define(MSG_BUFFER_SIZE, 20).

% Commands
run() ->
  {ok, [Cookie|Nodes]} = file:consult('./enodes.conf'),

  Username = get_username(),

  net_kernel:start([Username, longnames]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  erlang:set_cookie(node(), ACookie),

  ChosenNode = choose_node(Nodes),
  ConnectedNode = connect_client(Username, ChosenNode),
  spawn_link(prompt, run, [self()]),
  maintain_connection(ConnectedNode, Username, "Connected to Network", []).

maintain_connection(ConnectedNode, Username, Status, MsgBuffer) ->
  ui:render(Username, Status, get_available_clients(ConnectedNode), MsgBuffer),
  receive
    quit ->
      quit(ConnectedNode, Username);
    refresh ->
      maintain_connection(ConnectedNode, Username, "Refresh UI", MsgBuffer);
    {outgoing_msg, Input} ->
      case string:to_integer(Input) of
        {N, RawMsg} ->
          Peername = lists:nth(N, get_available_clients(ConnectedNode)),
          Msg = string:strip(RawMsg),
          try
            global:send(observer, {send_msg, self(), Username, Peername, Msg}),
            global:send(ConnectedNode, {route_msg, Username, Peername, Msg})
          catch
            {badarg, _} ->
              % TODO: Error handling
              io:format("ERROR: The chat message could not be sent.")
          end,
          maintain_connection(ConnectedNode, Username, "Message sent!", add_to_msg_buffer({Username, Msg}, MsgBuffer));
        _ ->
          NewStatus = io_lib:format("ERROR: can not parse input: ~s", [Input]),
          maintain_connection(ConnectedNode, Username, NewStatus, MsgBuffer)
      end;
    {incoming_msg, Msg, From} ->
      global:send(observer, {receive_msg, self(), From, Username, Msg}),
      maintain_connection(ConnectedNode, Username, "Message received!", add_to_msg_buffer({From, Msg}, MsgBuffer))
  end.

add_to_msg_buffer({Username, Msg}, Buffer) ->
  {_, {H, M, _}} = erlang:localtime(),
  NewBuffer = [{{H, M}, Username, Msg}|Buffer],
  case length(NewBuffer) > ?MSG_BUFFER_SIZE of
    true -> lists:droplast(NewBuffer);
    false -> NewBuffer
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
  global:send(ConnectedNode, {disconnect_client, Username, self()}),
  receive
    {disconnect_successful, Node} -> io:format("Disconnected from ~p~n", [Node])
  end,
  init:stop().

get_available_clients(ConnectedNode) ->
  global:send(ConnectedNode, { request_available_clients, self() }),
  receive
    {available_clients, AvailableClients} -> AvailableClients
  end.


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
