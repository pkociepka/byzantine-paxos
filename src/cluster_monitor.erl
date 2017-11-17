-module(cluster_monitor).

-export([start/1, get_nodes/0, get_nodes_separately/0, reset_cluster/1]).

start(N) ->
    Nodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, 2*N+1)],
    Malicious = [spawn_link(malicious_node, start, [1]) || _X <- lists:seq(1, N)],
    listen(Nodes, Malicious).

listen(Nodes, Malicious) ->
    receive
        {get_nodes, SenderPid} ->
            SenderPid ! {ok, Nodes ++ Malicious},
            listen(Nodes, Malicious);
        {get_nodes_separately, SenderPid} ->
            SenderPid ! {ok, Nodes, Malicious},
            listen(Nodes, Malicious);
        {reset_cluster, N} ->
            [N ! ok || N <- Nodes],
            NewNodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, 2*N+1)],
            NewMalicious = [spawn_link(malicious_node, start, [1]) || _X <- lists:seq(1, N)],
            listen(NewNodes, NewMalicious);
        ok -> ok
    end.

get_nodes() ->
    cl_monitor ! {get_nodes, self()},
    receive
        {ok, Nodes} -> Nodes
    end.

get_nodes_separately() ->
    cl_monitor ! {get_nodes_separately, self()},
    receive
        {ok, Nodes, Malicious} -> {Nodes, Malicious}
    end.

reset_cluster(N) ->
    cl_monitor ! {reset_cluster, N}.
