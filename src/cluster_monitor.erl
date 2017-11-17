-module(cluster_monitor).

-export([start/1]).

start(N) ->
    Nodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, 2*N+1)],
    Malicious = [spawn_link(malicious_node, start, [1]) || _X <- lists:seq(1, N)],
    listen(Nodes, Malicious).

listen(Nodes, Malicious) ->
    receive
        {get_nodes, SenderPid} ->
            SenderPid ! {ok, Nodes ++ Malicious},
            listen(Nodes, Malicious);
        {reset_cluster, N} ->
            [N ! ok || N <- Nodes],
            NewNodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, 2*N+1)],
            NewMalicious = [spawn_link(malicious_node, start, [1]) || _X <- lists:seq(1, N)],
            listen(NewNodes, NewMalicious);
        ok -> ok
    end.
