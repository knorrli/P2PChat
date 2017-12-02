-module(node).

-export([ init/0 ]).


init() ->
  global:register_name(node(), self()),
  % this should not be necessary but observer isn't available otherwise??
  %
  global:sync(),

  receive
    { initialize_links, LinkedNodes } ->
      run_node([], [], LinkedNodes)
  end.


% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
run_node(ConnectedClients, AvailableClients, LinkedNodes) ->
  global:send(observer, {node_status, self(), ConnectedClients, AvailableClients, LinkedNodes}),
  receive
    {connect_client, Username, Pid} ->
      case lists:keymember(Pid, 1, ConnectedClients) of
        true ->
          run_node(ConnectedClients, AvailableClients, LinkedNodes);
        false ->
          InformedNodes = [self()|LinkedNodes],
          global:send(observer, {client_connected, self(), Username, Pid}),
          [ Node ! {new_client_online, self(), Username, 0, InformedNodes} || Node <- LinkedNodes ],
          run_node([{Pid, Username}|ConnectedClients], AvailableClients, LinkedNodes)
      end;

    {disconnect_client, Pid} ->
      global:send(observer, {client_disconnected, self(), Pid}),
      [ Node ! {client_disconnected, Pid} || Node <- LinkedNodes ],
      Pid ! {disconnect_successful, self()},
      run_node(lists:keydelete(Pid, 1, ConnectedClients), AvailableClients, LinkedNodes);

    {request_available_clients, Pid} ->
      Pid ! {available_clients, AvailableClients},
      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    % Perform a modified version of Chandy-Misra
    %   InformedNodes holds all the nodes that already know about this Client.
    %   Since the InformedNodes were informed by someone that also informed us,
    %   it is not possible that we would have a shorter path to any of them.
    %   Therefore we don't have to inform these Nodes => less messages.
    {new_client_online, ClientNode, Username, ClientNodeDistance, InformedNodes} ->
      io:format("~p: received {new_client_online, ~p, ~p, ~p, ~p}~n", [self(), ClientNode, Username, ClientNodeDistance, InformedNodes]),

      DistanceToClient = ClientNodeDistance + 1,

      NodesToInform = [ Node || Node <- LinkedNodes, Node =/= ClientNode, not(lists:member(Node, InformedNodes)) ],
      NewInformedNodes = lists:append(InformedNodes, NodesToInform),

      case lists:keyfind(Username, 1, AvailableClients) of
        {Username, _CurrentClientNode, CurrentDistance} ->
          % Client already in our list of clients
          case DistanceToClient < CurrentDistance of
            true ->
              % The new Distance is better
              inform_about_new_client(NodesToInform, Username, DistanceToClient, NewInformedNodes),
              run_node(ConnectedClients, lists:keyreplace(Username, 1, AvailableClients, {Username, ClientNode, DistanceToClient}), LinkedNodes);
            false ->
              % The new Distance is worse
              run_node(ConnectedClients, AvailableClients, LinkedNodes)
          end;
        false ->
          % Client not in our list of clients
          inform_about_new_client(NodesToInform, Username, DistanceToClient, NewInformedNodes),
          run_node(ConnectedClients, [{Username, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes)
      end;

    {client_offline, _} ->
      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    {chat_msg, From, To, Msg} ->
      io:format("~p: received {chat_msg, ~p, ~p, ~p}~n", [self(), From, To, Msg]),
      route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients),

      run_node(ConnectedClients, AvailableClients, LinkedNodes)
  end.

% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients) ->
  io:format("TO: ~p~n", [To]),
  case lists:keyfind(To, 2, ConnectedClients) of
    {Pid, _} -> Pid ! {incoming_msg, Msg, From};
    false ->
      io:format("~p: looking for ~p in list of available clients to find route: ~p~n", [self(), To, AvailableClients]),
      {_, ClosestNode, _} = lists:keyfind(To, 1, AvailableClients),
      global:send(observer, {route_msg, self(), From, To, ClosestNode}),
      ClosestNode ! {chat_msg, From, To, Msg}
  end.

inform_about_new_client(NodesToInform, Client, DistanceToClient, InformedNodes) ->
  [ Node ! {new_client_online, self(), Client, DistanceToClient, InformedNodes} || Node <- NodesToInform ].
