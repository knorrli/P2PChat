-module(node).

-export([ init/0 ]).


init() ->
  global:register_name(node(), self()),
  % this should not be necessary but observer isn't available otherwise??
  %
  global:sync(),
  helpers:debug("~p: running~n", [self()]),

  receive
    {initialize_links, LinkedNodes } ->
      % initially, no clients are connected
      LinkedNodesWithClients = [ {LinkedNode, []} || LinkedNode <- LinkedNodes ],
      run_node([], LinkedNodesWithClients)
  end.


run_node(ConnectedClients, LinkedNodes) ->
  global:send(observer, {node_status, self(), ConnectedClients, LinkedNodes}),
  receive
    {connect_client, Client} ->
      case lists:member(Client, ConnectedClients) of
        true ->
          io:format("~p: client was already connected: ~p~n", [self(), ConnectedClients]),
          run_node(ConnectedClients, LinkedNodes);
        false ->
          connect_client(Client, LinkedNodes),
          io:format("~p: Added ~p to connected clients~n", [self(), Client]),
          run_node([Client|ConnectedClients], LinkedNodes)
      end;

    {disconnect_client, Client} ->
      disconnect_client(Client, ConnectedClients, LinkedNodes),
      run_node(lists:delete(Client, ConnectedClients), LinkedNodes);

    {request_available_clients, Client} ->
      io:format("~p: Client ~p requests available clients!~n", [self(), Client]),
      AvailableClients = [C || {C, _Dist} <- lists:flatten([Clients || {_, Clients} <- LinkedNodes])],
      io:format("~p: The following Clients are available: ~p~n", [self(), AvailableClients]),
      Client ! {available_clients, AvailableClients};

    % TODO: Implement Chandy-Misra for finding shortest path for msg routing
    % The current algorithm just informs the nodes until every node knows about
    % the client, but not necessarily the shortest path.
    {new_client_online, LinkedNode, Client, Distance} ->
      io:format("~p: {new_client_online, ~p, ~p, ~p}. Linked Nodes: ~p~n", [self(), LinkedNode, Client, Distance, LinkedNodes]),
      % 1. Update own LinkedNodes list s.t. Client with Distance is represented in Clients of LinkedNode
      global:send(observer, {node_status, self(), ConnectedClients, LinkedNodes}),
      io:format("~p: updating node ~p:~n", [self(), LinkedNode]),
      UpdatedLinkedNodes = lists:map(
                             fun({Node, ClientList}) ->
                                 % io:format("Step Node: ~p~n", [Node]),
                                 case Node =:= LinkedNode of
                                   true -> 
                                     % io:format("Node is == LinkedNode~n"),
                                     % add {Client, Distance} to ClientList or update {Client Distance}
                                     case lists:keymember(Client, 1, ClientList) of
                                       true -> NewClientList = lists:keyreplace(Client, 1, ClientList, {Client, Distance});
                                       false -> NewClientList = [{Client, Distance + 1}|ClientList]
                                     end,
                                     {Node, NewClientList};
                                   % don't modify the other nodes
                                   false ->
                                     % io:format("Node is /== LinkedNode~n"),
                                     {Node, ClientList}
                                 end
                             end, LinkedNodes),
      % io:format("~p: Updated Nodes: ~p~n", [self(), UpdatedLinkedNodes]),

      % Inform other nodes about this new Client if they don't know about the Client yet
      NeedsToBeInformedAboutClient = fun({N, ClientList}) ->
                                         io:format("~p: LinkedNode ~p knows about the following clients: ~p~n", [self(), N, ClientList]),
                                         Bool = not(lists:keymember(Client, 1, ClientList)),
                                         io:format("~p: Does LinkedNode ~p need to be informed about ~p? ~p~n", [self(), N, Client, Bool]),
                                         Bool
                                     end,
      UninformedNodes = lists:filter(NeedsToBeInformedAboutClient, UpdatedLinkedNodes),
      io:format("~p: Uninformed Nodes: ~p~n", [self(), UninformedNodes]),

      [ Node ! {new_client_online, self(), Client, Distance + 1} || [{Node, _}] <- UninformedNodes ],
      run_node(ConnectedClients, UpdatedLinkedNodes);

    {client_offline, Client} ->
      run_node(ConnectedClients, LinkedNodes);

    {chat_msg, From, To, Msg} ->
      global:send(observer, {route_msg, self(), From, To})
      %route_chat_msg(From, To, Msg),
  end.

connect_client(Client, LinkedNodes) ->
  global:send(observer, {client_connected, self(), Client}),
  io:format("~p: sending {new_client_online} to LinkedNodes: ~p~n", [self(), LinkedNodes]),
  [ Node ! {new_client_online, self(), Client, 0} || {Node, _} <- LinkedNodes ].

disconnect_client(Client, _, LinkedNodes) ->
  global:send(observer, {client_disconnected, self(), Client}),
  [ Node ! {client_disconnected, Client} || [Node, _] <- LinkedNodes ],
  Client ! {disconnect_successful, self()}.

%route_chat_msg(From, To, Msg) when To == self() ->
% TODO: route_msg_to_optimal_linked_node
%run_node(ConnectedClients, LinkedNodes)

