-module(messenger).

-export([start/0, stop/0, send/3, broadcast/2, broadcast_many/2, get_log/0, get_pretty_log/2]).

start() ->
  loop([]).

loop(Messages) ->
  receive
    {send, From, To, Msg} ->
      To ! Msg,
      loop([{From, To, Msg} | Messages]);
    {get_log, Pid} ->
      Pid ! {log, lists:reverse(Messages)},
      flush(),
      loop([]);
    ok -> ok
  end.

send(From, To, Msg) ->
  messenger ! {send, From, To, Msg}.

broadcast(From, Msg) ->
  cl_monitor ! {get_nodes, self()},
  receive
    {ok, Nodes} -> [send(From, N, Msg) || N <- Nodes],
    length(Nodes)
  end.

broadcast_many(From, MsgProvider) ->
  cl_monitor ! {get_nodes, self()},
  receive
    {ok, Nodes} -> [send(From, N, MsgProvider()) || N <- Nodes],
    length(Nodes)
  end.

get_log() ->
  messenger ! {get_log, self()},
  receive
    {log, Messages} -> Messages
  end.

stop() ->
  messenger ! ok.

flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.

get_pretty_log(ClientPid, Result) ->
  {Nodes, Malicious} = cluster_monitor:get_nodes_separately(),
  #{result => Result,
    nodes => #{
      leader => ClientPid,
      normal_nodes => Nodes,
      byzantine => Malicious
    },
    messages => [format_entry(X) || X <- get_log()]
   }.

format_entry({From, To, Msg}) ->
  #{from => From, to => To, msg => Msg}.