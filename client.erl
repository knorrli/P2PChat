-module(client).
-export([run/0]).
-define(OUTFILE, "out_client.hrl").
-define(DEBUG, true).

run() ->
  {ok, [Cookie|Enodes]} = file:consult('./enodes.conf'),
  debug("set cookie to ~p~n", [Cookie]),
  % erlang:set_cookie(node(self()), Cookie),
  io:format("current cookie: ~p~n", [erlang:get_cookie()]),

  ConnectedNode = choose_node(Enodes),
  io:format("Connecting to ~p...~n", [ConnectedNode]),
  connect_client(ConnectedNode).


connect_client(Node) ->
  net_kernel:connect_node(Node),
  Node ! {client_connected, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%
choose_node(Enodes) ->
  io:format("There are currently ~p Nodes in the network.", [length(Enodes)]),
  io:format("Which one do you want to connect to?~n"),
  [ io:format("~2.. B. ~p~n", [I, Enode]) || {Enode, I} <- lists:zip(Enodes, lists:seq(1, length(Enodes))) ],
  case io:fread("> ", "~d") of
    {ok, Int} -> Int;
    {error, _} ->
      io:format("please enter a number~n"),
      [ io:format("~2.. B. ~p~n", [I, Enode]) || {Enode, I} <- lists:zip(Enodes, lists:seq(1, length(Enodes))) ],
      choose_node(Enodes)
  end.

% helper for switchable debug print output
debug(Str, Prms) ->
  case ?DEBUG of
    true -> io:format(Str, Prms);
    false -> debug_off
  end.
