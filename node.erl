-module(node).

-export([ init/0 ]).


init() ->
  global:register_name(node(), self()),
  global:sync(),
  global:send(observer, {node_online, self()}),

  receive
    { initialize_links, LinkedNodes } ->
      lists:foreach(fun(N) ->
                        N ! { link_added, self() },
                        global:send(observer, {link_added, self(), N})
                    end, LinkedNodes),
      run_node([], [], LinkedNodes)
  end.


% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
run_node(ConnectedClients, AvailableClients, LinkedNodes) ->
  receive
    {link_added, Node} ->
      case not(lists:member(Node, LinkedNodes)) of
        true ->
          global:send(observer, {link_added, self(), Node}),
          run_node(ConnectedClients, AvailableClients, [Node|LinkedNodes]);
        false -> run_node(ConnectedClients, AvailableClients, LinkedNodes)
      end;

    {connect_client, Username, Pid} ->
      case lists:keymember(Pid, 1, ConnectedClients) of
        true ->
          run_node(ConnectedClients, AvailableClients, LinkedNodes);
        false ->
          global:send(observer, {client_connected, self(), Username, Pid}),

          InformedNodes = [self()|LinkedNodes],
          [ Node ! {client_available, self(), Username, 0, InformedNodes} || Node <- LinkedNodes ],

          run_node([{Pid, Username}|ConnectedClients], AvailableClients, LinkedNodes)
      end;



    {request_available_clients, Pid} ->
      Pid ! {available_clients, [ Username || {Username, _, _} <- AvailableClients ] },

      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    % Perform a modified version of Chandy-Misra
    %   InformedNodes holds all the nodes that already know about this Client.
    %   Since the InformedNodes were informed by someone that also informed us,
    %   it is not possible that we would have a shorter path to any of them.
    %   Therefore we don't have to inform these Nodes => less messages.
    {client_available, ClientNode, Username, ClientNodeDistance, InformedNodes} ->
      DistanceToClient = ClientNodeDistance + 1,


      NodesToInform = [ Node || Node <- LinkedNodes, Node =/= ClientNode, not(lists:member(Node, InformedNodes)) ],
      NewInformedNodes = lists:append(InformedNodes, NodesToInform),


      case lists:keyfind(Username, 1, AvailableClients) of
        {Username, _CurrentClientNode, CurrentDistance} ->
          % Client already in our list of clients
          case DistanceToClient < CurrentDistance of
            true ->
              % The new Distance is better
              global:send(observer, {client_available, self(), Username, ClientNode, DistanceToClient}),
              inform_about_new_client(NodesToInform, Username, DistanceToClient, NewInformedNodes),
              % inform connected clients about new client
              [ ConnectedClientPid ! {client_available, Username} || {ConnectedClientPid, _} <- ConnectedClients ],
              run_node(ConnectedClients, lists:keyreplace(Username, 1, AvailableClients, {Username, ClientNode, DistanceToClient}), LinkedNodes);
            false ->
              % The new Distance is worse
              run_node(ConnectedClients, AvailableClients, LinkedNodes)
          end;
        false ->
          % Client not in our list of clients
          global:send(observer, {client_available, self(), Username, ClientNode, DistanceToClient}),
          inform_about_new_client(NodesToInform, Username, DistanceToClient, NewInformedNodes),
          % inform connected clients about new client
          [ ConnectedClientPid ! {client_available, Username} || {ConnectedClientPid, _} <- ConnectedClients ],
          run_node(ConnectedClients, [{Username, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes)
      end;

    {client_unavailable, ClientNode, Username, InformedNodes} ->
      global:send(observer, {client_unavailable, self(), Username}),

      NodesToInform = [ Node || Node <- LinkedNodes, not(lists:member(Node, InformedNodes)) ],
      NewInformedNodes = lists:append(InformedNodes, NodesToInform),
      [ Node ! {client_unavailable, ClientNode, Username, NewInformedNodes} || Node <- NodesToInform ],
      [ ConnectedClientPid ! {client_unavailable, Username} || {ConnectedClientPid, _} <- ConnectedClients ],

      run_node(ConnectedClients, lists:keydelete(Username, 1, AvailableClients), LinkedNodes);

    {disconnect_client, Username, ClientPid} ->
      global:send(observer, {client_disconnected, self(), Username, ClientPid}),
      InformedNodes = [self()|LinkedNodes],
      [ Node ! {client_unavailable, self(), Username, InformedNodes} || Node <- LinkedNodes ],
      ClientPid ! {disconnect_successful, self()},
      run_node(lists:keydelete(ClientPid, 1, ConnectedClients), AvailableClients, LinkedNodes);

    {route_msg, From, To, Msg} ->
      route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients),
      run_node(ConnectedClients, AvailableClients, LinkedNodes)
  end.

% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients) ->
  case lists:keyfind(To, 2, ConnectedClients) of
    {ClientPid, _} ->
      global:send(observer, {route_msg, self(), From, To, ClientPid, Msg}),
      io:format("~p: sending MSG ~p to ~p~n", [self(), Msg, ClientPid]),
      ClientPid ! {incoming_msg, Msg, From};
    false ->
      {_, ClosestNode, _} = lists:keyfind(To, 1, AvailableClients),
      global:send(observer, {route_msg, self(), From, To, ClosestNode, Msg}),
      ClosestNode ! {route_msg, From, To, Msg}
  end.

inform_about_new_client(NodesToInform, Client, DistanceToClient, InformedNodes) ->
  [ Node ! {client_available, self(), Client, DistanceToClient, InformedNodes} || Node <- NodesToInform ].
