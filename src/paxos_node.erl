-module(paxos_node).

-export([start/1]).

start(SeqNumber) ->
    listen(SeqNumber, #{}).

listen(SeqNumber, ValuesMap) ->
    receive
        {get, Key, Pid} ->
            get(Pid, Key, SeqNumber, ValuesMap);
        {propose, Key, Value, MessageSeqNumber, SenderPid} ->
            case MessageSeqNumber > SeqNumber of
                true -> promise(SenderPid, Key, Value, SeqNumber, ValuesMap);
                false -> reject(SenderPid, Key, Value, SeqNumber, ValuesMap)
            end;
        ok -> ok;
        _ -> listen(SeqNumber, ValuesMap)
    end.

get(SenderPid, Key, SeqNumber, ValuesMap) ->
    case maps:find(Key, ValuesMap) of
        {ok, Value} -> messenger:send(self(), SenderPid, {self(), Value});
        error -> messenger:send(self(), SenderPid, {self(), no_value})
    end,
    listen(SeqNumber, ValuesMap).

reject(SenderPid, Key, Value, SeqNumber, ValuesMap) ->
    messenger:send(self(), SenderPid, {rejected, Key, Value}),
    listen(SeqNumber, ValuesMap).

promise(SenderPid, Key, Value, SeqNumber, ValuesMap) ->
    NodesCount = messenger:broadcast(self(), {promise, Key, Value}),
    Quorum = (NodesCount - 1) / 3 * 2 + 1,
    Responses = [receive {promise, Key, PromisedValue} -> PromisedValue  after 100 -> timeout end || N <- lists:seq(1, NodesCount)],
    case length([X || X <- Responses, X == Value]) >= Quorum of
        true ->
            messenger:send(self(), SenderPid, {promise, Key, Value}),
            await_save(Key, Value, SeqNumber, ValuesMap);
        _ ->
            reject(SenderPid, Key, Value, SeqNumber, ValuesMap)
    end.

    
await_save(Key, Value, SeqNumber, ValuesMap) ->
    receive
        {save, Key, Value, MessageSeqNumber, SenderPid} ->
            case MessageSeqNumber >= SeqNumber of
                true -> save(SenderPid, Key, Value, MessageSeqNumber, ValuesMap);
                false -> reject(SenderPid, Key, Value, SeqNumber, ValuesMap)
            end
    end.

save(SenderPid, Key, Value, SeqNumber, ValuesMap) ->
    NodesCount = messenger:broadcast(self(), {try_save, Key, Value}),
    Quorum = (NodesCount - 1) / 3 * 2 + 1,
    Responses = [receive {try_save, Key, PromisedValue} -> PromisedValue  after 100 -> timeout end || N <- lists:seq(1, NodesCount)],
    case length([X || X <- Responses, X == Value]) >= Quorum of
        true ->
            messenger:send(self(), SenderPid, {saved, Key, Value}),
            listen(SeqNumber, maps:put(Key, Value, ValuesMap));
        _ ->
            reject(SenderPid, Key, Value, SeqNumber, ValuesMap)
    end.