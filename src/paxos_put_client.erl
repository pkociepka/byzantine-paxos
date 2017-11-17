-module(paxos_put_client).

-export([init/2]).
-export([content_types_provided/2]).
-export([handle/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle}
    ], Req, State}.

handle(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    Value = cowboy_req:binding(value, Req),
    clock ! {get, self()},
    cl_monitor ! {get_nodes, self()},
    receive
        {seq, SeqNumber} -> receive
            {ok, Nodes} ->
                {Response, Req, State} = propose(Nodes, Key, Value, SeqNumber, Req, State),
                {response:create(Response), Req, State}
        end
    end.

propose(Nodes, Key, Value, SeqNumber, Req, State) ->
    [messenger:send(self(), N, {propose, Key, Value, SeqNumber, self()}) || N <- Nodes],
    Responses = [receive X -> X end || _N <- Nodes],
    % ordinary Paxos!
    Quorum = (length(Responses) - 1) / 3 * 2 + 1,
    try_accept(Key, Value, SeqNumber, Responses, Quorum, Nodes, Req, State).

try_accept(Key, Value, SeqNumber, Responses, Quorum, Nodes, Req, State) ->
    case length([X || X <- Responses, X == {promise, Key, Value}]) >= Quorum of
        true ->
            [messenger:send(self(), N, {save, Key, Value, SeqNumber, self()}) || N <- Nodes],
            AckResponses = [receive X -> X after 1000 -> {nope} end || _N <- Nodes],
            accept(Key, Value, AckResponses, Quorum, Req, State);
        false -> reject(Req, State)
    end.

accept(Key, Value, Responses, Quorum, Req, State) ->
    case length([X || X <- Responses, X == {saved, Key, Value}]) >= Quorum of
        true -> accept(Req, State);
        false -> reject(Req, State)
    end.

accept(Req, State) -> {true, Req, State}.

reject(Req, State) -> {false, Req, State}.