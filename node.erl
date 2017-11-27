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
      % initially, no clients can be connected
      LinkedNodesWithClients = [ {LinkedNode, []} || LinkedNode <- LinkedNodes ],
      run_node([], LinkedNodesWithClients)
  end.


run_node(ConnectedClients, LinkedNodes) ->
  global:send(observer, {node_status, self(), ConnectedClients, LinkedNodes}),
  receive
    {connect_client, Client} ->
      connect_client(Client, LinkedNodes),
      run_node([Client|ConnectedClients], LinkedNodes);

    {disconnect_client, Client} ->
      disconnect_client(Client, ConnectedClients, LinkedNodes),
      run_node(lists:delete(Client, ConnectedClients), LinkedNodes);

    {request_available_clients, Client} ->
      Client ! {available_clients, lists:flatten([ C || [{_, [C]}] <- LinkedNodes ])};

    {new_client_online, LinkedNode, Client, Distance} ->
      run_node(ConnectedClients, LinkedNodes);

    {client_offline, Client} ->
      run_node(ConnectedClients, LinkedNodes);

    {chat_msg, From, To, Msg} ->
      global:send(observer, {route_msg, self(), From, To})
      %route_chat_msg(From, To, Msg),
  end.

connect_client(Client, LinkedNodes) ->
  global:send(observer, {client_connected, self(), Client}).
  % TODO: inform_linked_nodes_about_connected_client(Client, LinkedNodes) -> [inform_next_node_about_connected_client(Client,LinkedNode) || LinkedNode <- LinkedNodes].
  % inform_next_node_about_connected_client(Client,Node) -> LinkedNode ! {new_client_online, self(), Client}.

% Let other 
disconnect_client(Client, ConnectedClients, LinkedNodes) ->
  global:send(observer, {client_disconnected, self(), Client}),
  [ Node ! {client_disconnected, Client} || [Node, _] <- LinkedNodes ].

%route_chat_msg(From, To, Msg) when To == self() ->
  % TODO: route_msg_to_optimal_linked_node
  %run_node(ConnectedClients, LinkedNodes)

