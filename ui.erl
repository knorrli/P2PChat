-module(ui).

-export([prompt/1]).

-define(REFRESH, "!r").
-define(QUIT, "!q").
-define(PROMPT, "> ").

% prompt the user for an action and relay the chosen action to the client
prompt(Client) ->
  Input = string:strip(io:get_line(?PROMPT), right, $\n),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?QUIT -> Client ! quit;
    ?REFRESH -> Client ! refresh;
    "" -> Client ! refresh;
    _ -> Client ! {parse_msg, Input}
  end,
  prompt(Client).
