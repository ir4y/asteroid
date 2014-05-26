-module(asteroid).

-export([
    start/0,
    stop/0
    ]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(celery),
    ok = application:start(asteroid),
    ok = sync:go().

stop() ->
    ok = sync:stop(),
    ok = application:stop(asteroid),
    ok = application:stop(celery),
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    ok = application:stop(ranch),
    ok = application:stop(crypto).
