-module(network).

-export([ run/0, init_node/1, log/2, debug/2 ]).
-define(OUTFILE, "out_network.hrl").
-define(DEBUG, true).

% The network contains of a single observer process that sets up and then
% observes the different network nodes.
run() ->

  {{Year, Month, Day}, {Hour, Minute, _}} = erlang:localtime(),
  file:write_file(?OUTFILE, io_lib:format("~p-~p-~p ~p:~p | creating random network~n", [Year, Month, Day, Hour, Minute]), [write]),
  % 1. create random network
  %   1.1 deploy nodes on TEDA
  {ok, [_|Enodes]} = file:consult('./enodes.conf'),
  Nodes = deploy_nodes(Enodes),
  debug("Nodes: ~p~n", [Nodes]),

  %   1.2 initialize network connections
  initialize_random_network(Nodes),

  observe_network().

observe_network() ->
  receive
    {client_connected, Node, Client} -> log("~p: client ~p connected~n", [Node, Client]);
    {route_msg, Recipient, From, To} -> log("~p: routing message from ~p to ~p~n", [Recipient, From, To])
  end,
  observe_network().

deploy_nodes(Enodes) ->
  [ deploy_node(Enode) || Enode <- Enodes ].

deploy_node(Enode) ->
  Node = spawn(Enode, ?MODULE, init_node, [self()]),
  receive
    { node_initialized, Pid, Name } -> log("~p: node registered as ~p~n", [Pid, Name])
  end,
  Node.

% For each Enode, set up network connections to other nodes
initialize_random_network(Enodes) ->
  [ initialize_network_links(Node, lists:delete(Node, Enodes)) || Node <- Enodes ].

initialize_network_links(Node, OtherNodes) ->
  Node ! { initialize_links, OtherNodes, self() },
  receive
    {node_linked, N, LinkedNodes} -> log("~p: linked to ~p~n", [N, LinkedNodes])
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NODE
%%
% inform the observer that we are running
init_node(Observer) ->
  global:register_name(node(), self()),
  Observer ! { node_initialized, self(), node() },
  debug("~p: running~n", [self()]),

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
      Observer ! {client_connected, self(), Client};
      %connect_client(Client, ConnectedClients, LinkedNodes, Observer),
      run_node([Client|ConnectedClients], LinkedNodes, Observer);

    {client_disconnected, Client} ->
      Observer ! {client_disconnected, self(), Client};
      %disconnect_client(Client, ConnectedClients, LinkedNodes, Observer),
      run_node(lists:delete(Client), LinkedNodes, Observer);

    %{new_client_online, LinkedNode, Client} ->
      %run_node(ConnectedClients, LinkedNodes, Observer, lists:flatten([RouteMap, {LinkedNode, Client});

    {chat_msg, From, To, Msg} ->
      Observer ! {route_msg, self(), From, To}
      %route_chat_msg(From, To, Msg),
  end.

%connect_client(Client, LinkedNodes, Observer) ->
  % TODO: inform_linked_nodes_about_connected_client(Client, LinkedNodes) -> [inform_next_node_about_connected_client(Client,LinkedNode) || LinkedNode <- LinkedNodes].
  % inform_next_node_about_connected_client(Client,Node) -> LinkedNode ! {new_client_online, self(), Client}.

%disconnect_client(Client, LinkedNodes, Observer) ->
  % TODO: inform_linked_nodes_about_disconnected_client(Client, LinkedNodes),

%route_chat_msg(From, To, Msg) when To == self() ->
  % TODO: route_msg_to_optimal_linked_node
  %run_node(ConnectedClients, LinkedNodes, Observer)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
log(Msg, Prms) ->
  debug(Msg, Prms),
  file:write_file(?OUTFILE, io_lib:format(Msg, Prms), [append]).

% helper for switchable debug print output
debug(Str, Prms) ->
  case ?DEBUG of
    true -> io:format(Str, Prms);
    false -> debug_off
  end.

