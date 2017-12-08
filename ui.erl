-module(ui).

-export([render/4]).

-define(REFRESH, "!r").
-define(NUMBER, "N").
-define(QUIT, "!q").


render(Username, Status, AvailableClients, MessageBuffer) ->
  io:format(os:cmd(clear)),

  Separator = io_lib:format("---------------------------------------------------------------------------------------------~n",[]),
  Header = io_lib:format("P2PChat - connected as ~s | ~p~n", [Username, Status]),

  UI = [
        Separator,
        Header,
        Separator,
        io_lib:format("Help:~n~s~n", [help()]),
        Separator,
        io_lib:format("Peers:~n~s~n", [peers(AvailableClients)]),
        Separator,
        io_lib:format("Chat:~n~s~n", [messages(MessageBuffer)]),
        io_lib:format("~s~n", [Separator])
  ],
  % Rendering everything at once is a lot faster
  io:format(UI).

help() ->
  [
    io_lib:format("The following commands are available:~n", []),
    io_lib:format("~p <message> | where N is an integer, send a text message to the client corresponding on the list to N.~n", [?NUMBER]),
    io_lib:format("~p | refresh UI (debugging).~n", [?REFRESH]),
    io_lib:format("~p | quit P2PChat.~n", [?QUIT])
  ].

peers(AvailableClients) ->
  case AvailableClients of
    [] -> "There are no clients available, sorry.";
    Peers ->
      PeersWithIndex = lists:zip(lists:seq(1, length(Peers)), Peers),
      lists:map(fun({I, P}) ->
                    io_lib:format("~p: ~p~n", [I, P])
                end, PeersWithIndex)
  end.

messages(MessageBuffer) ->
  case MessageBuffer of
    [] -> "There are no messages yet.";
    _ ->
      lists:reverse(
        lists:map(
          fun ({{H, M}, Name, Msg}) ->
              io_lib:format("~2..0w:~2..0w | <~p>: ~p~n", [H, M, Name, Msg])
          end,
          MessageBuffer)
       )
  end.
