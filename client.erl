-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").

run() ->
  {ok, [Cookie|Enodes]} = file:consult('./enodes.conf'),
  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  io:format("init net_kernel~n"),
  net_kernel:start([client, longnames]),

  io:format("node: ~p, self: ~p, node(self()): ~p~n", [node(), self(), node(self())]),

  ACookie = list_to_atom(integer_to_list(Cookie)),
  io:format("set cookie to ~p~n", [Cookie]),
  erlang:set_cookie(node(), ACookie),
  io:format("current cookie: ~p~n", [erlang:get_cookie()]),

  ConnectedNode = choose_node(Enodes),
  connect_client(ConnectedNode),
  io:format("Welcome to P2PChat. enter !h for help.~n"),
  display_ui(ConnectedNode).

connect_client(Node) ->
  io:format("Connecting to ~p... ", [Node]),
  net_kernel:connect_node(Node),
  % sync global state (although this should happen automatically?)
  global:sync(),
  global:send(Node, {connect_client, self()}),
  io:format("Done.~n").

display_ui(ConnectedNode) ->
  prompt(ConnectedNode).

disconnect_client(ConnectedNode) ->
  global:send(ConnectedNode, {disconnect_client, self()}),
  receive
    {disconnect_successful, Node} -> io:format("Disconnected from ~p~n", [Node])
  end,
  choose_node(Enodes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%

prompt(ConnectedNode) ->
  case io:fread("> ", "~s") of
    {ok, [Input]} -> parse_input(ConnectedNode, Input);
    {error, _} ->
      io:format("USER INPUT ERROR! Help!!!")
  end.

parse_input(ConnectedNode, Input) ->
  case Input of
    "!h" -> display_help();
    "!d" -> disconnect(ConnectedNode);
    _ -> ok
  end.

display_help() ->
  io:format("HELP~n"),
  io:format("The following commands are available:~n"),
  io:format("!c <username> | start chat with the selected user.~n"),
  io:format("!d | disconnect from the network.~n"),
  io:format("!h | display this help message.~n"),
  io:format("!l | list users.~n"),
  prompt().



% TODO: improve error handling
choose_node(Enodes) ->
  io:format("There are currently ~p Nodes in the network.", [length(Enodes)]),
  io:format("Which one do you want to connect to?~n"),
  EnodeList = lists:zip(lists:seq(1, length(Enodes)), Enodes),
  [ io:format("~2.. B. ~p~n", [I, Enode]) || {I, Enode} <- EnodeList ],
  case io:fread("> ", "~d") of
    {ok, [Int]} ->
      {_, Choice} = lists:keyfind(Int, 1, EnodeList),
      Choice;
    {error, _} ->
      io:format("ERROR: please enter a number between 1 and ~p~n", length(Enodes)),
      choose_node(Enodes)
  end.
