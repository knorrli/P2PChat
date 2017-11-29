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


% ConnectedClients: [<Client>]
% AvailableClients: [{<Client>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
run_node(ConnectedClients, AvailableClients, LinkedNodes) ->
  global:send(observer, {node_status, self(), ConnectedClients, AvailableClients, LinkedNodes}),
  receive
    {connect_client, Client} ->
      case lists:member(Client, ConnectedClients) of
        true ->
          run_node(ConnectedClients, AvailableClients, LinkedNodes);
        false ->
          InformedNodes = [self()|LinkedNodes],
          global:send(observer, {client_connected, self(), Client}),
          [ Node ! {new_client_online, self(), Client, 0, InformedNodes} || Node <- LinkedNodes ],
          run_node([Client|ConnectedClients], AvailableClients, LinkedNodes)
      end;

    {disconnect_client, Client} ->
      global:send(observer, {client_disconnected, self(), Client}),
      [ Node ! {client_disconnected, Client} || Node <- LinkedNodes ],
      Client ! {disconnect_successful, self()},
      run_node(lists:delete(Client, ConnectedClients), AvailableClients, LinkedNodes);

    {request_available_clients, Client} ->
      Client ! {available_clients, AvailableClients},
      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    % Perform a modified version of Chandy-Misra
    %   InformedNodes holds all the nodes that already know about this Client.
    %   Since the InformedNodes were informed by someone that also informed us,
    %   it is not possible that we would have a shorter path to any of them.
    %   Therefore we don't have to inform these Nodes => less messages.
    {new_client_online, ClientNode, Client, ClientNodeDistance, InformedNodes} ->
      io:format("~p: received {new_client_online, ~p, ~p, ~p, ~p}~n", [self(), ClientNode, Client, ClientNodeDistance, InformedNodes]),

      DistanceToClient = ClientNodeDistance + 1,

      NodesToInform = [ Node || Node <- LinkedNodes, Node =/= ClientNode, not(lists:member(Node, InformedNodes)) ],
      NewInformedNodes = lists:append(InformedNodes, NodesToInform),

      case lists:keyfind(Client, 1, AvailableClients) of
        {Client, _CurrentClientNode, CurrentDistance} ->
          % Client already in our list of clients
          case DistanceToClient < CurrentDistance of
            true ->
              % The new Distance is better
              inform_about_new_client(NodesToInform, Client, DistanceToClient, NewInformedNodes),
              run_node(ConnectedClients, lists:keyreplace(Client, 1, AvailableClients, {Client, ClientNode, DistanceToClient}), LinkedNodes);
            false ->
              % The new Distance is worse
              run_node(ConnectedClients, AvailableClients, LinkedNodes)
          end;
        false ->
          % Client not in our list of clients
          inform_about_new_client(NodesToInform, Client, DistanceToClient, NewInformedNodes),
          run_node(ConnectedClients, [{Client, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes)
      end;
    % OLD:
    % {new_client_online, ClientNode, Client, ClientNodeDistance} ->
    %   io:format("~p: received {new_client_online, ~p, ~p, ~p}~n", [self(), ClientNode, Client, ClientNodeDistance]),

    %   DistanceToClient = ClientNodeDistance + 1,

    %   case lists:keyfind(Client, 1, AvailableClients) of
    %     {Client, _CurrentClientNode, CurrentDistance} ->
    %       % Client already in our list of clients
    %       case DistanceToClient < CurrentDistance of
    %         true ->
    %           % The new Distance is better
    %           % 1. Inform LinkedNodes about new route to this Client
    %           % 2. Modify own routing information for this Client
    %           [ Node ! {new_client_online, self(), Client, ClientNodeDistance} || Node <- LinkedNodes, Node =/= ClientNode ],
    %           run_node(ConnectedClients, lists:keyreplace(Client, 1, AvailableClients, {Client, ClientNode, DistanceToClient}), LinkedNodes);
    %         false ->
    %           % The new Distance is worse
    %           % 1. Keep running with current information
    %           run_node(ConnectedClients, AvailableClients, LinkedNodes)
    %       end;
    %     false ->
    %       % Client not in our list of clients
    %       % 1. Inform LinkedNodes about new route to this Client
    %       % 2. Add Client to our routing information
    %       [ Node ! {new_client_online, self(), Client, DistanceToClient } || Node <- LinkedNodes, Node =/= ClientNode ],
    %       run_node(ConnectedClients, [{Client, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes)
    %   end;

    {client_offline, _} ->
      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    {chat_msg, From, To, Msg} ->
      io:format("~p: received {chat_msg, ~p, ~p, ~p}~n", [self(), From, To, Msg]),
      global:send(observer, {route_msg, self(), From, To}),
      route_chat_msg(From, To, Msg, ConnectedClients, LinkedNodes),
      io:format("~p: routed message, now restarting receive loop~n", [self()]),
      run_node(ConnectedClients, AvailableClients, LinkedNodes)
  end.

route_chat_msg(From, To, Msg, ConnectedClients, LinkedNodes) ->
  case lists:member(To, ConnectedClients) of
    % Send message to connected Client
    % FIXME: This sends the MSG to all clients??
    true -> [ global:send(Client, {incoming_msg, Msg, From}) || Client <- ConnectedClients ];

    % route message to next Node that has a connection to Client
    false ->
      NodesWithClient = lists:filtermap(fun({N, ClientList}) ->
                                            case lists:keyfind(To, 1, ClientList) of
                                              {_, Dist} -> {true, {N, Dist}};
                                              _ -> false
                                            end
                                        end,
                                        LinkedNodes),
      io:format("NodesWithClient: ~p~n", [NodesWithClient]),
      {_MinDist, ClosestNode} = lists:min([ {Dist, N} || {N, Dist} <- NodesWithClient ]),
      io:format("~p: forwarding chat msg to ~p~n", [self(), ClosestNode]),
      ClosestNode ! {chat_msg, From, To, Msg}
  end.

inform_about_new_client(NodesToInform, Client, DistanceToClient, InformedNodes) ->
  [ Node ! {new_client_online, self(), Client, DistanceToClient, InformedNodes} || Node <- NodesToInform ].
