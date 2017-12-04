-module(ui).

-export([start/2, render_msg/3, render_peers/2, prompt/2]).

-define(PING, "!p").
-define(HELP, "!h").
-define(NUMBER, "N").
-define(QUIT, "!q").
-define(LIST_USERS, "!l").

-define(PROMPT, "> ").

% start the UI
start(Client, Peers) ->
  io:format("Welcome to P2PChat. Enter !h for help.~n"),
  prompt(Client, Peers).

% public facing, lets the client write to output
render_msg(_, Msg, From) ->
  io:format("<~s>: ~s~n", [From, Msg]).


render_peers(Client, Peers) when Peers =:= [] ->
  io:format("There are no clients available, sorry.~n"),
  prompt(Client, Peers);
render_peers(Client, Peers) ->
  PeersWithIndex = lists:zip(lists:seq(1, length(Peers)), Peers),
  lists:foreach(fun({I, P}) -> io:format("~p: ~p~n", [I, P]) end, PeersWithIndex),
  prompt(Client, Peers).

% prompt the user for an action and relay the chosen action to the client
prompt(Client, Peers) ->
  Input = string:strip(io:get_line(?PROMPT), right, $\n),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?PING -> Client ! ping;
    ?LIST_USERS -> Client ! list_users;
    ?QUIT -> Client ! quit;
    ?HELP -> display_help(),
             prompt(Client, Peers);
    _ -> Cmd2 = string:to_integer(Cmd),
         case Cmd2 of
           {N, _} ->
             PeerName = lists:nth(N, Peers),
             Message = string:sub_word(Input, 2),
             Client ! {outgoing_msg, Message, PeerName},
             prompt(Client, Peers);
           _ -> display_help(),
                prompt(Client,Peers)
         end
  end.

display_help() ->
  io:format("HELP~n"),
  io:format("The following commands are available:~n"),
  io:format("~p | quit P2PChat.~n", [?QUIT]),
  io:format("~p | display this help message.~n", [?HELP]),
  io:format("~p | list users.~n", [?LIST_USERS]),
  io:format("~p | where N is an integer, send a text message to the client corresponding on the list to N.~n", [?NUMBER]).
