-module(watcher).

-export([ run/0, init_node/1, log/2, debug/2 ]).
-define(OUTFILE, "out.hrl").
-define(DEBUG, false).

% run() -> run(no_teda).
run() ->

  {{Year, Month, Day}, {Hour, Minute, _}} = erlang:localtime(),
  file:write_file(?OUTFILE, io_lib:format("~p-~p-~p ~p:~p | creating random network~n", [Year, Month, Day, Hour, Minute]), [write]),
  % 1. create random network
  %   1.1 deploy nodes on TEDA
  {ok, [_|Enodes]} = file:consult('./enodes.conf'),
  Nodes = deploy_nodes(Enodes),
  debug("Nodes: ~p~n", [Nodes]),

  %   1.2 initialize network connections
  initialize_random_network(Nodes).

  % LOCAL
  % unregisters the process after running so that the code can be run in the
  % erlang repl multiple times.
  % unregister(watcher).

log(Msg, Prms) ->
  debug(Msg, Prms),
  file:write_file(?OUTFILE, io_lib:format(Msg, Prms), [append]).

deploy_nodes(Enodes) ->
  [ deploy_node(Enode) || Enode <- Enodes ].

deploy_node(Enode) ->
  Node = spawn(Enode, ?MODULE, init_node, [self()]),
  receive
    { node_initialized, N } -> log("~p: node initialized~n", [N])
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

% inform the watcher that we are running
init_node(Watcher) ->
  debug("~p: running~n", [self()]),
  Watcher ! { node_initialized, self() },

  receive
    {initialize_links, LinkedNodes, Watcher } ->
      Watcher ! {node_linked, self(), LinkedNodes},
      route_messages(LinkedNodes, Watcher)
  end.

% TODO
route_messages(LinkedNodes, Watcher) ->
  receive
    {chat_msg, From, To, Msg} ->
      route_chat_msg(From, To, Msg),
      route_messages(LinkedNodes, Watcher)
  end.

%% TODO
route_chat_msg(From, To, Msg) when To == self() ->
  debug("Yay", [self()]).

% helper for switchable debug print output
debug(Str, Prms) ->
  case ?DEBUG of
    true -> io:format(Str, Prms);
    false -> debug_off
  end.

