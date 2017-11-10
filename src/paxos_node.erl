-module(paxos_node).

-export([start/1]).

start(SeqNumber) ->
    listen(SeqNumber, #{}).

listen(SeqNumber, ValuesMap) ->
    receive
        {get, Key, Pid} ->
            messenger:send(self(), Pid, {self(), maps:get(Key, ValuesMap)}),
            listen(SeqNumber, ValuesMap);
        {propose, Key, Value, MessageSeqNumber, SenderPid} ->
            case MessageSeqNumber > SeqNumber of
                true -> messenger:send(self(), SenderPid, {ok, Key, Value});
                false -> messenger:send(self(), SenderPid, {nope, Key, Value})
            end,
            listen(MessageSeqNumber, ValuesMap);
        {save, Key, Value, MessageSeqNumber, SenderPid} ->
            case MessageSeqNumber >= SeqNumber of
                true -> messenger:send(self(), SenderPid, {saved, Key, Value});
                false -> messenger:send(self(), SenderPid, {rejected, Key, Value})
            end,
            listen(MessageSeqNumber, maps:put(Key, Value, ValuesMap));
        ok -> ok
    end.
