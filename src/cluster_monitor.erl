-module(cluster_monitor).

-export([start/1]).

start(NodesCount) ->
    Nodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, NodesCount)],
    listen(Nodes).

listen(Nodes) ->
    receive
        {get_nodes, SenderPid} ->
            SenderPid ! {ok, Nodes},
            listen(Nodes);
        {reset_cluster, NodesCount} ->
            [N ! ok || N <- Nodes],
            NewNodes = [spawn_link(paxos_node, start, [1]) || _X <- lists:seq(1, NodesCount)],
            listen(NewNodes);
        ok -> ok
    end.
