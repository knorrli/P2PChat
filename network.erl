-module(network).

-export([ run/0 ]).

-define(OUTFILE, "out_network.hrl").

% The network contains of a single observer process that sets up and then
% observes the different network nodes.
run() ->

  % register self as global observer
  global:register_name(observer, self()),

  {{Year, Month, Day}, {Hour, Minute, _}} = erlang:localtime(),
  Str = io_lib:format("~p-~p-~p ~p:~p | creating random network~n", [Year, Month, Day, Hour, Minute]),
  io:format(Str),
  file:write_file(?OUTFILE, Str, [write]),
  {ok, [_|Enodes]} = file:consult('./enodes.conf'),
  Nodes = deploy_nodes(Enodes),

  initialize_random_network(Nodes),

  observe_network().

observe_network() ->
  receive
    {node_online, Node} -> log("~p: online~n", [Node]);
    {node_offline, Node} -> log("~p: offline~n", [Node]);
    {node_status, Node, ConnectedClients, AvailableClients, LinkedNodes} -> log("~p:~n  ConnectedClients: ~p~n  AvailableClients: ~p~n  Links: ~p~n", [Node, ConnectedClients, AvailableClients, LinkedNodes]);
    {link_added, Node, LinkedNode } -> log("~p: linked to ~p~n", [Node, LinkedNode]);
    {client_connected, Node, Username, Pid} -> log("~p: client ~p (~p) connected~n", [Node, Username, Pid]);
    {client_disconnected, Node, Username, Client} -> log("~p: client ~p (~p) disconnected~n", [Node, Username, Client]);
    {route_msg, Recipient, From, To, Via, Msg} -> log("~p: routing message ~p from ~p to ~p via ~p~n", [Recipient, Msg, From, To, Via])
  end,
  observe_network().

deploy_nodes(Enodes) ->
  [ deploy_node(Enode) || Enode <- Enodes ].

deploy_node(Enode) ->
  spawn(Enode, node, init, []).

% For each Enode, set up network connections to other nodes
initialize_random_network(Enodes) ->
  [ initialize_random_network_links(Node, Enodes) || Node <- Enodes ].

% NOTE: Please be aware that at the moment, the links are directed.
% i.e. A -> B does not imply that B -> A
initialize_random_network_links(Node, Enodes) ->
   OtherNodes = lists:delete(Node, Enodes),

   % TODO: Because the erlang version on TEDA has not yet reached V18, we are
   % stuck with the old and deprecated random module that needs to be seeded
   % before usage. Once TEDA is updated, we could simply use rand:uniform/1
   random:seed(erlang:now()),

   % removes 50% of the links
   RandomNodes = lists:filter(fun(_) -> random:uniform(2) /= 1 end, OtherNodes),
   % Always create at least one link
   case RandomNodes of
     [] -> Node ! { initialize_links, [lists:nth(random:uniform(length(OtherNodes)), OtherNodes)] };
     _ -> Node ! { initialize_links, RandomNodes }
   end.


log(Str, Interpolations) ->
  file:write_file(?OUTFILE, io_lib:format(Str, Interpolations), [append]),
  io:format(Str, Interpolations).
