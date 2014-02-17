-module(kvs_box).

-include_lib("kvs/include/kvs.hrl").

-record(box,{id,user,email}).
-record(box_subscription,{who,whom}).

-export([init/1, json_to_rec/1, rec_to_json/1, bucket/0]).

init(Backend=store_mnesia) ->
    ?CREATE_TAB(box),
    ?CREATE_TAB(box_subscription),
    Backend:add_table_index(box, user),
    Backend:add_table_index(box, email),
    Backend:add_table_index(box_subscription, who),
    Backend:add_table_index(box_subscription, whom);
init(_) -> ok.

json_to_rec([{<<"email">>, Email},
             {<<"id">>, Id},
             {<<"user">>, User}]) ->
    #box{id=Id, email=Email, user=User}.

rec_to_json(Rec) -> 
    [{<<"email">>, Rec#box.email},
     {<<"id">>, Rec#box.id},
     {<<"user">>, Rec#box.user}].

bucket() -> box.
