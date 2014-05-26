-module(pubsub).

-export([handle/2, subscribe_to/1]).

handle(FunctionName, Arguments) ->
  Function = erlang:binary_to_atom(FunctionName, utf8),
  ?MODULE:Function(Arguments).

subscribe_to([Channel]) ->
    jsx:encode([{<<"status">>, <<"SUCCESS">>},
                {<<"result">>, subscriber:subscribe(Channel)}]).
