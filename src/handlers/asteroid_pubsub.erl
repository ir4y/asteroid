-module(asteroid_pubsub).

-export([handle/2, is_periodical/1]).

-export([subscribe_to/1]).

-behaviour(asteroid_handler).

handle(FunctionName, Arguments) ->
  Function = erlang:binary_to_atom(FunctionName, utf8),
  ?MODULE:Function(Arguments).

is_periodical(FunctionName) ->
  case FunctionName of
    <<"subscribe_to">> -> true;
    _ -> false
  end.

subscribe_to([Channel]) ->
  {fun() ->
    subscriber:unsubscribe(Channel)
   end,
   jsx:encode([{<<"status">>, <<"SUCCESS">>},
               {<<"result">>, subscriber:subscribe(Channel)}])}.

