-module(ui).

-export([start/2, render_msg/3, render_peers/2, render_chat/2, prompt/2]).

-define(PING, "!p").
-define(HELP, "!h").
-define(START_CHAT, "!c").
-define(END_CHAT, "!e").
-define(QUIT, "!q").
-define(LIST_USERS, "!l").

-define(PROMPT, "> ").

% start the UI
start(Client, Peers) ->
  io:format("Welcome to P2PChat. Enter !h for help.~n"),
  prompt(Client, Peers).

% public facing, lets the client write to output
render_msg(Client, Msg, From) ->
  io:format("<~s>: ~s~n", [From, Msg]),
  chat_prompt(Client, From).

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
    ?START_CHAT ->
      PeerName = list_to_atom(string:sub_word(Input, 2)),
      start_chat(Client, PeerName);
    ?QUIT -> Client ! quit;
    ?HELP -> display_help(), prompt(Client, Peers);
    N when is_integer(N) -> 
      PeerName = lists:nth(Cmd, Peers),
      Message = string:sub_word(Input, 2),
      Client ! {outgoing_msg, Message, PeerName};
    _ -> display_help(), prompt(Client, Peers)
  end.

chat_prompt(Client, PeerName) ->
  Prompt = io_lib:format("<~p>: ", [node(Client)]),
  Input = string:strip(io:get_line(Prompt), right, $\n),
  Cmd = string:sub_word(Input, 1),

  case Cmd of
    ?LIST_USERS -> Client ! list_users;
    ?END_CHAT -> Client ! end_chat;
    ?QUIT -> Client ! quit;
    ?HELP -> display_help();
    _ ->
      Client ! {outgoing_msg, Input, PeerName}
  end.

% TODO
start_chat(Client, PeerName) ->
  Client ! {start_chat, PeerName}.

render_chat(Client, PeerName) ->
  io:format("You are now chatting with <~s>~n", [PeerName]),
  chat_prompt(Client, PeerName).


display_help() ->
  io:format("HELP~n"),
  io:format("The following commands are available:~n"),
  io:format("~p <username> | start chat with the selected user.~n", [?START_CHAT]),
  io:format("~p | quit P2PChat.~n", [?QUIT]),
  io:format("~p | end chat session.~n", [?END_CHAT]),
  io:format("~p | display this help message.~n", [?HELP]),
  io:format("~p | list users.~n", [?LIST_USERS]).
