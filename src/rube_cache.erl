-module(rube_cache).

-export([start/0]).

-export([get/2, put/3, delete/2]).

%% API キー情報を ets でキャッシュする expire 付き

%% API Key / [Domain], Expire

%% {Bucket, Key} / Value, Expire

-define(TABLE, rube_cache_table).

-define(DEFAULT_EXPIRE, 86400).

-record(rube_cache_object, {id :: binary(),
                            value :: any(),
                            expire :: non_neg_integer()}).

start() ->
    _Tid = ets:new(?TABLE, [public, set, named_table, {keypos, #rube_cache_object.id}]),
    ok.

%% stop() ->
%%     true = ets:delete(?TABLE),
%%     ok.

id(Bucket, Key) ->
    <<Bucket/binary, "$", Key/binary>>.

-spec get(binary(), binary()) -> not_found | any().
get(Bucket, Key) ->
    Datetime = calendar:now_to_local_time(os:timestamp()),
    Now = calendar:datetime_to_gregorian_seconds(Datetime),
    case ets:lookup(?TABLE, id(Bucket, Key)) of
        [] ->
            not_found;
        [#rube_cache_object{expire = Expire}] when Expire < Now ->
            %% どうせ上書きされるので ets:delete/2 しない
            not_found;
        [#rube_cache_object{value = Value}] ->
            Value 
    end.

-spec put(binary(), binary(), any()) -> ok.
put(Bucket, Key, Value) ->
    put(Bucket, Key, Value, expire(os:timestamp())).

put(Bucket, Key, Value, Expire) ->
    O = #rube_cache_object{id = id(Bucket, Key),
                           value = Value,
                           expire = Expire},
    true = ets:insert(?TABLE, O),
    ok.

delete(Bucket, Key) ->
    true = ets:delete(?TABLE, id(Bucket, Key)),
    ok.

expire(Now) ->
    Datetime = calendar:now_to_local_time(Now),
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Seconds + application:get_env(rube, cache_expire, ?DEFAULT_EXPIRE).
