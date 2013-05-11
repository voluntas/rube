-module(rube_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([get/3, put/4, delete/3]).

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {pid :: pid()}).

get(Worker, Bucket, Key) ->
    case rube_cache:get(Bucket, Key) of
        not_found ->
            case gen_server:call(Worker, {get, Bucket, Key}) of
                not_found ->
                    not_found;
                Value ->
                    ok = rube_cache:put(Bucket, Key, Value),
                    Value
            end;
        Value ->
            Value
    end.

put(Worker, Bucket, Key, Value) ->
    ok = gen_server:call(Worker, {put, Bucket, Key, Value}).

delete(Worker, Bucket, Key) ->
    ok = gen_server:call(Worker, {delete, Bucket, Key}),
    ok = rube_cache:delete(Bucket, Key).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Address = proplists:get_value(address, Args),
    Port = proplists:get_value(port, Args),
    {ok, Pid} = riakc_pb_socket:start_link(Address, Port),

    pong = riakc_pb_socket:ping(Pid),

    {ok, #state{pid = Pid}}.

handle_call({get, Bucket, Key}, _From, #state{pid = Pid} = State) ->
    Result = case riakc_pb_socket:get(Pid, Bucket, Key) of
                 {ok, O} ->
                     riakc_obj:get_value(O);
                 {error, _Reason} ->
                     ?debugVal(_Reason),
                     not_found
             end,
    {reply, Result, State};
handle_call({put, Bucket, Key, Value}, _From, #state{pid = Pid} = State) ->
    O = riakc_obj:new(Bucket, Key, Value),
    {reply, riakc_pb_socket:put(Pid, O), State};
handle_call({delete, Bucket, Key}, _From, #state{pid = Pid} = State) ->
    %% TODO(nakai): 存在するかどうか確認すべき？
    {reply, riakc_pb_socket:delete(Pid, Bucket, Key), State};
handle_call(_Request, _From, State) ->
    %% TODO(nakai): 実装ミスなので error/1 で落とすべき
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pid = Pid}) ->
    %% XXX(nakai): exit でいいの？
    exit(Pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




