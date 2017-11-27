-module(ui).

-export([start/1, render_msg/3, render_peers/2, render_chat/2]).

-define(PING, "!p").
-define(HELP, "!h").
-define(START_CHAT, "!c").
-define(END_CHAT, "!e").
-define(QUIT, "!q").
-define(LIST_USERS, "!l").

-define(PROMPT, "> ").

% start the UI
start(Client) ->
  io:format("Welcome to P2PChat. Enter !h for help.~n"),
  prompt(Client).

% public facing, lets the client write to output
render_msg(Client, Msg, From) ->
  io:format("<~s>: ~s~n", [From, Msg]),
  chat_prompt(Client).

render_peers(Client, Peers) when Peers =:= [] ->
  io:format("There are no clients available, sorry.~n"),
  prompt(Client);
render_peers(Client, Peers) ->
  PeerRepresentation = [ io_lib:format("~2.. B. ~p~n", [I, Enode]) || {I, Enode} <- lists:zip(lists:seq(1, length(Peers)), Peers) ],
  io:format(lists:join(PeerRepresentation, "~n")),
  prompt(Client).

% prompt the user for an action and relay the chosen action to the client
prompt(Client) ->
  io:format("Prompt:~n"),
  Input = string:strip(io:get_line(?PROMPT), right, $\n),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?PING -> Client ! ping;
    ?LIST_USERS -> Client ! list_users;
    ?START_CHAT ->
      PeerName = list_to_atom(string:sub_word(Input, 2)),
      start_chat(Client, PeerName);
    ?QUIT -> Client ! quit;
    ?HELP -> display_help(Client);
    _ -> display_help(Client)
  end.

chat_prompt(Client) ->
  Input = string:strip(io:get_line(?PROMPT), right, $\n),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?LIST_USERS -> Client ! list_users;
    ?END_CHAT -> Client ! end_chat;
    ?QUIT -> Client ! quit;
    ?HELP -> display_help(Client);
    _ ->
      render_msg(Client, Input, node())
  end.

% TODO
start_chat(Client, PeerName) ->
  Client ! {start_chat, PeerName}.

render_chat(Client, PeerName) ->
  io:format("You are now chatting with <~s>~n", [PeerName]),
  chat_prompt(Client).


display_help(Client) ->
  io:format("HELP~n"),
  io:format("The following commands are available:~n"),
  io:format("~p <username> | start chat with the selected user.~n", [?START_CHAT]),
  io:format("~p | quit P2PChat.~n", [?QUIT]),
  io:format("~p | end chat session.~n", [?END_CHAT]),
  io:format("~p | display this help message.~n", [?HELP]),
  io:format("~p | list users.~n", [?LIST_USERS]),
  prompt(Client).
