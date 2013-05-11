-module(rube).

-export([child_spec/5]).
-export([get/2, put/3, delete/2]).

%% Args = [{name, {local, pool1}},
%%         {size, 10},
%%         {max_overflow, 20},
%%         {address, "127.0.0.1"},
%%         {port, },
%% Pool = rube:child_spec(pool1, Args),
%% {ok, {{one_for_one, 10, 10}, [Pool]}}.

child_spec(Name, Size, MaxOverflow, Address, Port) when is_atom(Name) ->
    PoolArgs = [{name, {local, Name}},
                {size, Size},
                {max_overflow, MaxOverflow},
                {worker_module, rube_worker}],
    WorkerArgs = [{address, Address}, {port, Port}],
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).


-spec get(binary(), binary()) -> ok | {ok, any()} | {error, term()}.
get(Bucket, Key) ->
    F = fun(Worker) ->
            rube_worker:get(Worker, Bucket, Key)
        end,        
    PoolName = pool_name(),
    poolboy:transaction(PoolName, F).

-spec put(binary(), binary(), any()) -> ok | {error, term()}.
put(Bucket, Key, Value) ->
    F = fun(Worker) ->
            rube_worker:put(Worker, Bucket, Key, Value)
        end,
    PoolName = pool_name(),
    poolboy:transaction(PoolName, F).

-spec delete(binary(), binary()) -> ok | {error, term()}.
delete(Bucket, Key) ->
    F = fun(Worker) ->
            rube_worker:delete(Worker, Bucket, Key)
        end,
    PoolName = pool_name(),
    poolboy:transaction(PoolName, F).

pool_name() ->
    %% TODO(nakai): PoolName を判定する
    default.
