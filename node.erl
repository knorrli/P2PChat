-module(node).

-export([ init/0 ]).


init() ->
  global:register_name(node(), self()),
  global:sync(),
  global:send(observer, {node_online, self()}),

  receive
    { initialize_links, LinkedNodes } ->
      lists:foreach(fun(N) ->
                        N ! { link_added, self() },
                        global:send(observer, {link_added, self(), N})
                    end, LinkedNodes),
      run_node([], [], LinkedNodes)
  end.


% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
run_node(ConnectedClients, AvailableClients, LinkedNodes) ->
  receive
    {link_added, Node} ->
      case not(lists:member(Node, LinkedNodes)) of
        true ->
          global:send(observer, {link_added, self(), Node}),
          run_node(ConnectedClients, AvailableClients, [Node|LinkedNodes]);
        false -> run_node(ConnectedClients, AvailableClients, LinkedNodes)
      end;

    {connect_client, Username, Pid} ->
      case lists:keymember(Pid, 1, ConnectedClients) of
        true ->
          run_node(ConnectedClients, AvailableClients, LinkedNodes);
        false ->
          source_chandy_misra(Username, LinkedNodes),
          global:send(observer, {client_connected, self(), Username, Pid}),
          run_node([{Pid, Username}|ConnectedClients], AvailableClients, LinkedNodes)
      end;

    {request_available_clients, Pid} ->
      Pid ! {available_clients, [ Username || {Username, _, _} <- AvailableClients ] },
      run_node(ConnectedClients, AvailableClients, LinkedNodes);

    % Perform Chandy-Misra for finding the shortest path from each node to the
    % new client.
    {chandy_misra, Username, ClosestNode, Distance} ->
      [Node ! {chandy_misra, Username, self(), Distance + 1} || Node <- LinkedNodes ],
      {ClientNode, DistanceToClient} = perform_chandy_misra(Username, ClosestNode, Distance, LinkedNodes),

      % inform connected clients about new available client
      [ ConnectedClientPid ! {client_available, Username} || {ConnectedClientPid, _} <- ConnectedClients ],
      run_node(ConnectedClients, [{Username, ClientNode, DistanceToClient}|AvailableClients], LinkedNodes);

    {client_unavailable, ClientNode, Username} ->
      global:send(observer, {client_unavailable, self(), Username}),

      NodesToInform = [ Node || Node <- LinkedNodes ],
      [ Node ! {client_unavailable, ClientNode, Username} || Node <- NodesToInform ],
      [ ConnectedClientPid ! {client_unavailable, Username} || {ConnectedClientPid, _} <- ConnectedClients ],

      run_node(ConnectedClients, lists:keydelete(Username, 1, AvailableClients), LinkedNodes);

    {disconnect_client, Username, ClientPid} ->
      global:send(observer, {client_disconnected, self(), Username, ClientPid}),
      [ Node ! {client_unavailable, self(), Username} || Node <- LinkedNodes ],
      run_node(lists:keymerge(Username, 2, ConnectedClients, {ClientPid, Username, length(LinkedNodes)}), AvailableClients, LinkedNodes);

    {route_msg, From, To, Msg} ->
      route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients),
      run_node(ConnectedClients, AvailableClients, LinkedNodes)
  end.

source_chandy_misra(Username, LinkedNodes) ->
  io:format("~p: starting chandy-misra to make client ~p available to all nodes of ~p~n", [self(), Username, LinkedNodes]),
  [ Node ! {chandy_misra, Username, self(), 1} || Node <- LinkedNodes ],
  source_wait_for_acks(length(LinkedNodes), LinkedNodes),
  io:format("~p: Client ~p successfully connected!~n", [self(), Username]).

source_wait_for_acks(MissingAcks, LinkedNodes) ->
  io:format("~p: waiting for ~p ACKs~n", [self(), MissingAcks]),
  receive
    % because we need to inform everyone to go into Chandy-Misra mode, we might receive a couple of unnecessary messages.
    {chandy_misra, _, _, _} -> ok;
    {dist, Node, _} ->
      Node ! ack,
      source_wait_for_acks(MissingAcks, LinkedNodes);
    ack when MissingAcks == 1 ->
      [Node ! {stop, self()} || Node <- LinkedNodes ],
      io:format("Client successfully connected~n");
    ack -> source_wait_for_acks(MissingAcks - 1, LinkedNodes)
  end.

perform_chandy_misra(Username, ClosestNode, ClosestDistance, LinkedNodes) ->
  io:format("~p: send initial new client msg to linked nodes ~p~n", [self(), LinkedNodes]),
  [Node ! {dist, self(), ClosestDistance} || Node <- LinkedNodes],
  chandy_misra(Username, LinkedNodes, ClosestNode, ClosestDistance, length(LinkedNodes)).

chandy_misra(Username, LinkedNodes, Pred, Dist, Num) ->
  receive
    {chandy_misra, _, _, _} ->
      ok,
      chandy_misra(Username, LinkedNodes, Pred, Dist, Num);
    {dist, Exp, D} ->
      case D < Dist of
        true ->
          io:format("~p: new distance_msg {~p, ~p} received. Better than {~p, ~p}.~n", [self(), Exp, D, Pred, Dist]),
          case Num > 0 of
            true ->
              io:format("~p: Waiting for ACKs, sending ACK to ~p~n", [self(), Pred]),
              Pred ! ack;
            _ -> ok
          end,
          NewDist = D,
          NewPred = Exp,
          NewNum = Num + length(LinkedNodes),
          io:format("~p: sending improved distance to linked nodes ~p~n", [self(), LinkedNodes]),
          [Node ! {dist, self(), NewDist+1} || Node <- LinkedNodes ],
          case NewNum of
            0 -> NewPred ! ack;
            _ -> ok
          end,
          chandy_misra(Username, LinkedNodes, NewPred, NewDist, NewNum);
        false ->
          io:format("~p: new distance_msg {~p, ~p} received. Worse than {~p, ~p}.~n", [self(), Exp, D, Pred, Dist]),
          Exp ! ack,
          chandy_misra(Username, LinkedNodes, Pred, Dist, Num)
      end;
    ack when Num == 1 ->
      Pred ! ack,
      chandy_misra(Username, LinkedNodes, Pred, Dist, Num - 1);
    ack when Num < 1 ->
      io:format("ERROR: THIS MUST NOT HAPPEN!~n");
    ack ->
      chandy_misra(Username, LinkedNodes, Pred, Dist, Num - 1);
    {stop, N} ->
      [Node ! stop || Node <- LinkedNodes, Node =/= N],
      {Pred, Dist}
  end.



% ConnectedClients: [{Pid, Username}]
% AvailableClients: [{<Username>, <closest Node>, <Distance to closest Node>}]
% LinkedNodes: [<Node>]
route_chat_msg(From, To, Msg, ConnectedClients, AvailableClients) ->
  case lists:keyfind(To, 2, ConnectedClients) of
    {ClientPid, _, _} ->
      global:send(observer, {route_msg, self(), From, To, ClientPid, Msg}),
      io:format("~p: sending MSG ~p to ~p~n", [self(), Msg, ClientPid]),
      ClientPid ! {incoming_msg, Msg, From};
    false ->
      {_, ClosestNode, _} = lists:keyfind(To, 1, AvailableClients),
      global:send(observer, {route_msg, self(), From, To, ClosestNode, Msg}),
      ClosestNode ! {route_msg, From, To, Msg}
  end.
