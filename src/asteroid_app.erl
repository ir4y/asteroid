-module(asteroid_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Hello~n"),
    RpcHandlers = dict:from_list([{celery, python_celery},
                                  {pubsub, pubsub}]),
    Dispatch = cowboy_router:compile([
                {'_', [{"/static/[...]", cowboy_static, {dir,"priv"}},
                       {"/bullet", bullet_handler,
                        [{handler, asteroid_stream_handler},
                         {rpc_handlers, RpcHandlers}]}]}]),
    Ip = {0, 0, 0, 0},
    Port = 8008,
    {ok, Res} = cowboy:start_http(
            http, 100, [{ip, Ip}, {port, Port}], [{env, [{dispatch, Dispatch}]}]),
    io:format("~p~n",[Res]),
    asteroid_sup:start_link(),
    subscriber:start_link().
stop(_State) ->
    ok.
