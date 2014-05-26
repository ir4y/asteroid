-module(python_celery).

-include_lib("celery/include/celery.hrl").

-export([handle/2]).

handle(Function, Arguments) ->
    Msg = #celery_msg{task=Function, args=Arguments},
    Res = celery:call(Msg),
    jsx:encode([{<<"status">>, Res#celery_res.status},
                {<<"result">>, Res#celery_res.result}]).
