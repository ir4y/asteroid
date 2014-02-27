-module(python_celery).

-include_lib("celery/include/celery.hrl").

-export([add/2,
         add_delay/2]).

add(_Resource, [A, B]) ->
    Msg = #celery_msg{task= <<"tasks.add">>, args=[A,B]},
    Res = celery:call(Msg),
    jsx:to_json([{<<"status">>, Res#celery_res.status},
                 {<<"result">>, Res#celery_res.result}]).

add_delay(_Resource, [A, B]) ->
    Msg = #celery_msg{task= <<"tasks.add_delay">>, args=[A,B]},
    Res = celery:call(Msg),
    jsx:to_json([{<<"status">>, Res#celery_res.status},
                 {<<"result">>, Res#celery_res.result}]).
