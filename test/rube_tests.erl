-module(rube_tests).

-include_lib("eunit/include/eunit.hrl").

rube_test_() ->
    [
        {"riak ping",
            fun() ->
                    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
                    ?assertEqual(pong, riakc_pb_socket:ping(Pid)),
                    ?assert(exit(Pid, normal))
            end},
        {"",
            fun() ->
                    application:start(rube),

                    C = rube:child_spec(default, 10, 20, "127.0.0.1", 8087),
                    {ok, Pid} = supervisor:start_child(rube_sup, C),

                    ?assertEqual(not_found, rube:get(<<"api-key">>, <<"uuid">>)),

                    ?assertEqual(ok, rube:put(<<"api-key">>, <<"uuid">>, <<"domain">>)),

                    %% from riak
                    ?assertEqual(<<"domain">>, rube:get(<<"api-key">>, <<"uuid">>)),

                    %% from cache
                    ?assertEqual(<<"domain">>, rube:get(<<"api-key">>, <<"uuid">>)),

                    ?assertEqual(ok, rube:delete(<<"api-key">>, <<"uuid">>)),

                    ?assertEqual(not_found, rube:get(<<"api-key">>, <<"uuid">>)),

                    %% 空に対して delete を発行してもエラーにはならない？
                    ?assertEqual(ok, rube:delete(<<"api-key">>, <<"uuid">>)),

                    ?assert(exit(Pid, normal)),

                    application:stop(rube)
            end}
    ].
