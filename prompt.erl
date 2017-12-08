-module(prompt).

-export([run/1]).

-define(REFRESH, "!r").
-define(QUIT, "!q").
-define(PROMPT, "> ").

% prompt the user for an action and relay the chosen action to the client
run(Client) ->
  RawInput = io:get_line(?PROMPT),
  Input = string:trim(RawInput),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?QUIT ->
      Client ! quit,
      io:format("Goodbye!~n");
    ?REFRESH ->
      Client ! refresh,
      run(Client);
    "" ->
      Client ! refresh,
      run(Client);
    _ ->
      Client ! {outgoing_msg, Input},
      run(Client)
  end.
