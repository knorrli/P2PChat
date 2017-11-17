-module(watcher).

-export([ run/0, run/1, run_node/0, log/2, debug/2 ]).
-define(OUTFILE, "out.hrl").
-define(DEBUG, false).

run() -> run(no_teda).
run(Mode) ->

  {{Year, Month, Day}, {Hour, Minute, _}} = erlang:localtime(),
  file:write_file(?OUTFILE, io_lib:format("~p-~p-~p ~p:~p | creating random network", [Year, Month, Day, Hour, Minute]), [write]),
  % 0. register self as watcher
  register(watcher, self()),
  % 1. create random network
  %   1.1 deploy nodes on TEDA
  case Mode == teda of
    true -> {ok, [_|Enodes]} = file:consult('./enodes.conf');
    false -> Enodes = [node(), node(), node(), node()]
  end,

  Nodes = deploy_nodes(Mode, Enodes),
  debug("Nodes: ~p~n", [Nodes]),

  %   1.2 initialize network connections
  initialize_random_network(Nodes),

  % unregisters the process after running so that the code can be run in the
  % erlang repl multiple times.
  unregister(watcher).

log(Msg, Prms) ->
  debug(Msg, Prms),
  file:write_file(?OUTFILE, io_lib:format(Msg, Prms), [append]).

deploy_nodes(Mode, Enodes) ->
  [ deploy_node(Mode, Enode) || Enode <- Enodes ].

deploy_node(Mode, Enode) when Mode == teda ->
  Node = spawn(Enode, ?MODULE, run_node, []),
  receive
    { node_initialized, N, Msg } -> log("~p: ~p", [N, Msg])
  end,
  Node;
deploy_node(_, _) ->
  Node = spawn(?MODULE, run_node, []),
  receive
    { node_initialized, N } -> log("~p: initialized~n", [N])
  end,
  Node.

% For each Enode, set up network connections to other nodes
initialize_random_network(Enodes) ->
  [ initialize_network_links(Node, lists:delete(Node, Enodes)) || Node <- Enodes ].

initialize_network_links(Node, OtherNodes) ->
  Node ! { initialize_links, OtherNodes },
  receive
    {node_linked, N, LinkedNodes} -> log("~p: linked to ~p~n", [N, LinkedNodes])
  end.

% inform the watcher that we are running
run_node() ->
  debug("~p: running~n", [self()]),
  watcher ! { node_initialized, self() },

  receive
    {initialize_links, LinkedNodes} ->
      watcher ! {node_linked, self(), LinkedNodes},
      route_messages(LinkedNodes)
  end.

% TODO
route_messages(LinkedNodes) ->
  receive
    {chat_msg, From, To, Msg} ->
      route_chat_msg(From, To, Msg),
      route_messages(LinkedNodes)
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

