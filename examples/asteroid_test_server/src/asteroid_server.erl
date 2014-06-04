-module(asteroid_server).

-export([
    start/0,
    stop/0
    ]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(celery),
    start_cowboy(),
    ok = sync:go().

stop() ->
    ok = sync:stop(),
    ok = application:stop(asteroid),
    ok = application:stop(celery),
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    ok = application:stop(ranch),
    ok = application:stop(crypto).

start_cowboy() ->
    RpcHandlers = dict:from_list([{celery, asteroid_celery},
                                  {pubsub, asteroid_pubsub},
                                  {undefined, asteroid_default}]),
    Dispatch = cowboy_router:compile([
                {'_', [{"/static/[...]", cowboy_static, {dir,"priv"}},
                       {"/bullet", bullet_handler,
                        [{handler, asteroid_stream_handler},
                         {rpc_handlers, RpcHandlers}]}]}]),
    Ip = {0, 0, 0, 0},
    Port = 8008,
    {ok, _} = cowboy:start_http(
            http, 100, [{ip, Ip}, {port, Port}], [{env, [{dispatch, Dispatch}]}]),
    subscriber:start_link().
