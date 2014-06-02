-module(asteroid_default).
-export([handle/2, is_periodical/1]).
-behaviour(asteroid_handler).

handle(_Function, _Arguments) ->
  jsx:encode([{<<"status">>, <<"ERROR">>},
              {<<"result">>, <<"ERROR">>}]).

is_periodical(_) -> false.
