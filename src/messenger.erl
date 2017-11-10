-module(messenger).

-export([start/0, stop/0, send/3, get_log/0]).

start() ->
  loop([]).

loop(Messages) ->
  receive
    {send, From, To, Msg} ->
      To ! Msg,
      loop([Msg | Messages]);
    {get_log, Pid} ->
      Pid ! {log, lists:reverse(Messages)},
      flush(),
      loop([]);
    ok -> ok
  end.

send(From, To, Msg) ->
  messenger ! {send, From, To, Msg}.

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
