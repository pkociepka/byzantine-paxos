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
            {ok, Nodes} -> propose(Nodes, Key, Value, SeqNumber, Req, State)
        end
    end.

propose(Nodes, Key, Value, SeqNumber, Req, State) ->
    [messenger:send(self(), N, {propose, Key, Value, SeqNumber, self()}) || N <- Nodes],
    Responses = [receive X -> X end || _N <- Nodes],
    % ordinary Paxos!
    try_accept(Key, Value, SeqNumber, Responses, length(Responses) div 2, Nodes, Req, State).

try_accept(Key, Value, SeqNumber, Responses, Quorum, Nodes, Req, State) ->
    case length([X || X <- Responses, X == {ok, Key, Value}]) >= Quorum of
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

accept(Req, State) ->
    Response = json_formatter:format_response(true, self(), [], [], []),
    {Response, Req, State}.

reject(Req, State) ->
    Response = json_formatter:format_response(false, self(), [], [], []),
    {Response, Req, State}.
