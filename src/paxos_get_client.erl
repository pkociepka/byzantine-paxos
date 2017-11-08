-module(paxos_get_client).

-export([init/2]).
-export([content_types_provided/2]).
-export([handle/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, handle}
    ], Req, State}.

handle(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    cl_monitor ! {get_nodes, self()},
    receive
        {ok, Nodes} -> ask(Nodes, Key, Req, State)
    end.

ask(Nodes, Key, Req, State) ->
    [N ! {get, Key, self()} || N <- Nodes],
    Responses = [receive X -> X end || _N <- Nodes],
    % ordinary Paxos!
    find_winner(Responses, length(Responses) div 2, Req, State).

find_winner([], _Quorum, Req, State) ->
    {<<"no winner">>, Req, State};

find_winner(Responses, Quorum, Req, State) ->
    [Candidate | Rest] = Responses,
    case length([X || X <- Responses, X == Candidate]) >= Quorum of
        true -> {Candidate, Req, State};
        _ -> find_winner(Rest, Quorum, Req, State)
    end.
