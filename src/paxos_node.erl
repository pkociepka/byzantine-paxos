-module(paxos_node).

-export([start/1]).

start(SeqNumber) ->
    listen(SeqNumber, #{}).

listen(SeqNumber, Values) ->
    receive
        ok -> ok
    end.
