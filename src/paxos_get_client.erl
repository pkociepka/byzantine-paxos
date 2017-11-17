-module(paxos_get_client).

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
        {<<"text/plain">>, handle}
    ], Req, State}.

handle(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    clock ! {get, self()},
    cl_monitor ! {get_nodes, self()},
    receive
        {seq, SeqNumber} -> receive
            {ok, Nodes} -> ask(Nodes, Key, SeqNumber, Req, State)
        end
    end.

ask(Nodes, Key, _SeqNumber, Req, State) ->
    [messenger:send(self(), N, {get, Key, self()}) || N <- Nodes],
    Responses = lists:sort(fun({Pid1, V1}, {Pid2, V2}) -> Pid1 < Pid2 end,
                           [receive X -> X end || _N <- Nodes]),
    Values = [V || {Pid, V} <- Responses],
    % ordinary Paxos!
    find_winner(Nodes, Values, length(Values) div 2, Req, State).

find_winner(_Nodes, [], _Quorum, Req, State) ->
    {<<"no winner">>, Req, State};

find_winner(Nodes, Values, Quorum, Req, State) ->
    [Candidate | Rest] = Values,
    case length([X || X <- Values, X == Candidate]) >= Quorum of
        true -> {Candidate, Req, State};
        _ -> find_winner(Nodes, Rest, Quorum, Req, State)
    end.
