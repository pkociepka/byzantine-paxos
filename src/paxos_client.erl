-module(paxos_client).

-export([init/2]).
-export([content_types_provided/2]).
-export([readme/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, readme}
	], Req, State}.

readme(Req, State) ->
{<<"To put value in claster, send /put/<key>/<value>~nTo get previously saved value, send /get/<key>~n">>, Req, State}.

