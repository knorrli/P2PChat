-module(node).

-export([ init/1 ]).


init(Observer) ->
  global:register_name(node(), self()),
  Observer ! { node_initialized, self(), node() },
  helpers:debug("~p: running~n", [self()]),

  receive
    {initialize_links, LinkedNodes, Observer } ->
      Observer ! {node_linked, self(), LinkedNodes},
      % initially, no clients can be connected
      run_node([], LinkedNodes, Observer)
      % RouteMap = [],
      % run_node([], LinkedNodes, Observer, RouteMap)
  end.


run_node(ConnectedClients, LinkedNodes, Observer) ->
  receive
    {client_connected, Client} ->
      Observer ! {client_connected, self(), Client},
      %connect_client(Client, ConnectedClients, LinkedNodes, Observer),
      run_node([Client|ConnectedClients], LinkedNodes, Observer);

    {client_disconnected, Client} ->
      Observer ! {client_disconnected, self(), Client},
      %disconnect_client(Client, ConnectedClients, LinkedNodes, Observer),
      run_node(lists:delete(Client), LinkedNodes, Observer);

    %{new_client_online, LinkedNode, Client} ->
      %run_node(ConnectedClients, LinkedNodes, Observer, lists:flatten([RouteMap, {LinkedNode, Client}]);

    %{client_offline, Client} -> 
      %run_node(ConnectedClients, LinkedNodes, Observer, lists:keydelete(Client, 2, RouteMap));

    {chat_msg, From, To, Msg} ->
      Observer ! {route_msg, self(), From, To}
      %route_chat_msg(From, To, Msg),
  end.

%connect_client(Client, LinkedNodes, Observer) ->
  % TODO: inform_linked_nodes_about_connected_client(Client, LinkedNodes) -> [inform_next_node_about_connected_client(Client,LinkedNode) || LinkedNode <- LinkedNodes].
  % inform_next_node_about_connected_client(Client,Node) -> LinkedNode ! {new_client_online, self(), Client}.

%disconnect_client(Client, LinkedNodes, Observer) ->
  % TODO: inform_linked_nodes_about_disconnected_client(Client, LinkedNodes) -> [inform_next_node_about_disconnected_client(Client,LinkedNode) || LinkedNode <- LinkedNodes].
  % inform_next_node_about_disconnected_client(Client,Node) -> LinkedNode ! {client_offline, Client}.

%route_chat_msg(From, To, Msg) when To == self() ->
  % TODO: route_msg_to_optimal_linked_node
  %run_node(ConnectedClients, LinkedNodes, Observer)

