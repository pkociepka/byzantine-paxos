-module(functional_test).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    register(cl_monitor, spawn_link(cluster_monitor, start, [3])),
    register(messenger, spawn_link(messenger, start, [])).

put_test() ->
    SeqNumber = 1,
    cluster_monitor:reset_cluster(1),
    Nodes = cluster_monitor:get_nodes(),
    ?assertEqual(
        {true, req, state},
        paxos_put_client:propose(Nodes, <<"Key">>, <<"Value">>, SeqNumber+1, req, state)
    ).

put_fail_with_too_low_seq_number_test() ->
    SeqNumber = 0,
    cluster_monitor:reset_cluster(1),
    Nodes = cluster_monitor:get_nodes(),
    ?assertEqual(
        {false, req, state},
        paxos_put_client:propose(Nodes, <<"Key">>, <<"Value">>, SeqNumber, req, state)
    ).

put_get_test() ->
    SeqNumber = 1,
    cluster_monitor:reset_cluster(1),
    Nodes = cluster_monitor:get_nodes(),
    Key = <<"42">>,
    Value = <<"fortytwo">>,
    paxos_put_client:propose(Nodes, Key, Value, SeqNumber+1, req, state),
    ?assertEqual(
        {Value, req, state},
        paxos_get_client:ask(Nodes, Key, SeqNumber+2, req, state)
    ).

override_test() ->
    SeqNumber = 1,
    cluster_monitor:reset_cluster(1),
    Nodes = cluster_monitor:get_nodes(),
    Key = <<"42">>,
    Value1 = <<"fortytwo">>,
    Value2 = <<"Fortythree">>,
    paxos_put_client:propose(Nodes, Key, Value1, SeqNumber+1, req, state),
    ?assertEqual(
        {Value1, req, state},
        paxos_get_client:ask(Nodes, Key, SeqNumber+2, req, state)
    ),
    paxos_put_client:propose(Nodes, Key, Value2, SeqNumber+3, req, state),
    ?assertEqual(
        {Value2, req, state},
        paxos_get_client:ask(Nodes, Key, SeqNumber+4, req, state)
    ).

get_non_existing_key_test() ->
    SeqNumber = 1,
    cluster_monitor:reset_cluster(1),
    Nodes = cluster_monitor:get_nodes(),
    ?assertEqual(
        {<<"no winner">>, req, state},
        paxos_get_client:ask(Nodes, dummy_key, SeqNumber+1, req, state)
    ).
