-module(paxos_node).

-export([start/1]).

start(SeqNumber) ->
    listen(SeqNumber, #{<<"42">> => <<"fortytwo">>}).

listen(SeqNumber, ValuesMap) ->
    receive
        {get, Key, Pid} ->
            Pid ! maps:get(Key, ValuesMap);
        ok -> ok
    end.
