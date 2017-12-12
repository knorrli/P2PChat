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

          [ Node ! {client_available, self(), Username, 0} || Node <- LinkedNodes ],

          run_node([{Pid, Username}|ConnectedClients], AvailableClients, LinkedNodes)
      end;



    {request_available_clients, Pid} ->
      Pid ! {available_clients, [ Username || {Username, _, _} <- AvailableClients ] },

      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    % Perform Chandy-Misra for finding the shortest path from each node to the
    % new client.
    {client_available, ClientNode, Username, ClientNodeDistance} ->
      DistanceToClient = ClientNodeDistance + 1,


      NodesToInform = [ Node || Node <- LinkedNodes, Node =/= ClientNode ],

      case lists:keyfind(Username, 1, AvailableClients) of
        {Username, _CurrentClientNode, CurrentDistance} ->
          % Client already in our list of clients
          case DistanceToClient < CurrentDistance of
            true ->
              % The new Distance is better
              global:send(observer, {client_available, self(), Username, ClientNode, DistanceToClient}),
              inform_about_new_client(NodesToInform, Username, DistanceToClient),
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
          inform_about_new_client(NodesToInform, Username, DistanceToClient),
          % inform connected clients about new client
          [ ConnectedClientPid ! {client_available, Username} || {ConnectedClientPid, _} <- ConnectedClients ],
          run_node(ConnectedClients, [{Username, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes)
      end;

    {client_unavailable, ClientNode, Username} ->
      global:send(observer, {client_unavailable, self(), Username}),

      NodesToInform = [ Node || Node <- LinkedNodes ],
      [ Node ! {client_unavailable, ClientNode, Username} || Node <- NodesToInform ],
      [ ConnectedClientPid ! {client_unavailable, Username} || {ConnectedClientPid, _} <- ConnectedClients ],

      run_node(ConnectedClients, lists:keydelete(Username, 1, AvailableClients), LinkedNodes);

    {disconnect_client, Username, ClientPid} ->
      global:send(observer, {client_disconnected, self(), Username, ClientPid}),
      [ Node ! {client_unavailable, self(), Username} || Node <- LinkedNodes ],
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

inform_about_new_client(NodesToInform, Client, DistanceToClient) ->
  [ Node ! {client_available, self(), Client, DistanceToClient} || Node <- NodesToInform ].
