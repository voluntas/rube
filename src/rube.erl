-module(rube).

-export([child_spec/5]).
-export([get/3, put/4, delete/3]).

child_spec(Name, Size, MaxOverflow, Address, Port)
        when is_atom(Name) andalso
             is_integer(Size) andalso
             is_integer(MaxOverflow) andalso
             is_list(Address) andalso
             is_integer(Port) ->
    PoolArgs = [{name, {local, Name}},
                {size, Size},
                {max_overflow, MaxOverflow},
                {worker_module, rube_worker}],
    WorkerArgs = [{address, Address}, {port, Port}],
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).


-spec get(atom(), binary(), binary()) -> {ok, any()} | {error, term()}.
get(PoolName, Bucket, Key) ->
    F = fun(Worker) ->
            rube_worker:get(Worker, Bucket, Key)
        end,        
    poolboy:transaction(PoolName, F).

-spec put(atom(), binary(), binary(), any()) -> ok | {error, term()}.
put(PoolName, Bucket, Key, Value) ->
    F = fun(Worker) ->
            rube_worker:put(Worker, Bucket, Key, Value)
        end,
    poolboy:transaction(PoolName, F).

-spec delete(atom(), binary(), binary()) -> ok | {error, term()}.
delete(PoolName, Bucket, Key) ->
    F = fun(Worker) ->
            rube_worker:delete(Worker, Bucket, Key)
        end,
    poolboy:transaction(PoolName, F).
