%%%-------------------------------------------------------------------
%% @doc byzantine public API
%% @end
%%%-------------------------------------------------------------------

-module(byzantine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    {ok, _}   = cowboy:start_clear(http, TransOpts, ProtoOpts),
    byzantine_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
routes() ->
    [
     {'_', [
            {"/", cowboy_static, {priv_file, byzantine, "static/index.html"}},
            {"/assets/[...]", cowboy_static, {priv_dir, byzantine, "static/assets"}},
            {"/get/:key", paxos_get_client, []},
            {"/put/:key/:value", paxos_put_client, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
