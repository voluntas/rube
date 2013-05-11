####
Rube
####

deps::

   {rube,
    ".*", {git, "git://github.com/voluntas.git", {branch, "develop"}}}
    

sample::

    > application:start(rube).
    ok

    %% Name, Size, MaxOverflow, Address, Port
    > C = rube:child_spec(pool, 10, 20, "127.0.0.1", 8087).
    {pool,{poolboy,start_link,
                   [[{name,{local,pool}},
                     {size,10},
                     {max_overflow,20},
                     {worker_module,rube_worker}],
                    [{address,"127.0.0.1"},{port,8087}]]},
          permanent,5000,worker,
          [poolboy]}

    %% set supervisor
    > {ok, Pid} = supervisor:start_child(rube_sup, C).
    {ok,<0.40.0>}

    > rube:get(pool, <<"api-key">>, <<"uuid">>).
    not_found

    > rube:put(pool, <<"api-key">>, <<"uuid">>, <<"domain">>).
    ok

    %% from riak
    > rube:get(pool, <<"api-key">>, <<"uuid">>).
    <<"domain">>

    %% from cache
    > rube:get(pool, <<"api-key">>, <<"uuid">>).
    <<"domain">>

    > rube:delete(pool, <<"api-key">>, <<"uuid">>).

    > rube:get(pool, <<"api-key">>, <<"uuid">>).
    not_found

    > application:stop(rube).
    ok
    =INFO REPORT==== 12-May-2013::02:51:44 ===
        application: rube
        exited: stopped
        type: temporary

feature
=======

- Connection Pool
- Object Caching
- Easy to built-in (Use child_spec function)

TODO
====

- Dynamic Pool

  - Add
  - Remove
- Load balancing
- Fault monitoring
- Automatic uncoupling
