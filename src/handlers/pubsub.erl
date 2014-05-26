-module(pubsub).

-export([subscribe_to/1]).

subscribe_to([Channel]) ->
    jsx:encode([{<<"status">>, <<"SUCCESS">>},
                {<<"result">>, subscriber:subscribe(Channel)}]).
