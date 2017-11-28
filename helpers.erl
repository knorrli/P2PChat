-module(helpers).

-export([log/2, debug/2]).
-define(OUTFILE, "out_network.hrl").

-define(DEBUG, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
% log(Msg) -> log(Msg, []).
log(Msg, Prms) ->
  debug(Msg, Prms),
  file:write_file(?OUTFILE, io_lib:format(Msg, Prms), [append]).

% helper for switchable debug print output
% debug(Str) -> debug(Str, []).
debug(Str, Prms) ->
  case ?DEBUG of
    true -> io:format(Str, Prms);
    false -> debug_off
  end.


