-module(pubsub).

-export([subscribe_to/4]).

subscribe_to(_Resource, [Channel], Uuid, Parent) ->
    jsx:to_json([{<<"status">>, <<"SUCCESS">>},
                 {<<"result">>, subscriber:subscribe(Channel,
                                                     {Uuid, Parent})}]).
