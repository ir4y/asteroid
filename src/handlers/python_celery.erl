-module(python_celery).

-include_lib("celery/include/celery.hrl").

-export([add/1,
         add_delay/1]).

add([A, B]) ->
    Msg = #celery_msg{task= <<"tasks.add">>, args=[A,B]},
    Res = celery:call(Msg),
    jsx:encode([{<<"status">>, Res#celery_res.status},
                {<<"result">>, Res#celery_res.result}]).

add_delay([A, B]) ->
    Msg = #celery_msg{task= <<"tasks.add_delay">>, args=[A,B]},
    Res = celery:call(Msg),
    jsx:encode([{<<"status">>, Res#celery_res.status},
                {<<"result">>, Res#celery_res.result}]).
