
run() ->
  % 0. register self as watcher
  register(watcher, self()),
  % 1. create random network
  %   1.1 deploy nodes on TEDA
  {ok, [Cookie|Enodes]} = file:consult('./enodes.conf'),
  deploy_nodes(Enodes),
  %   1.2 initialize network connections
  initialize_random_network(Enodes),

  % For each node receive "initialized" msg
  [ receive
     {node_initialized, Node} -> log("")
   end || Node <- Enodes ],

  [
  ]





deploy_nodes(Enodes) ->
  [ deploy_node(Enode) || Enode <- Enodes ].

deploy_node(Enode) ->
  Node = spawn(Enode, ?MODULE, run_node, []).

% For each Enode, set up network connections to other nodes
initialize_random_network(Enodes) ->
  [ initialize_network_links(Node, OtherNodes) || Node <- Enodes,
                                                  OtherNodes <- lists:delete(Enodes, Node) ].

initialize_network_links(Node, OtherNodes) ->
  Node ! { set_links, OtherNodes }.

% inform the watcher that we are running
run_node() ->
  watcher ! {node_initialized, self()},

  receive
    {set_links, Nodes} -> route_messages(Nodes)
  end.

route_messages(Nodes) ->
  watcher ! {node_in_network, self(), Nodes}.

