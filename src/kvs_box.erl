-module(kvs_box).

-include_lib("kvs/include/kvs.hrl").

-record(box,{id,user,email}).
-record(box_subscription,{who,whom}).

-export([init/1]).
-export([echo/2, create/2, get/2, filter/2]).

init(Backend=store_mnesia) ->
    ?CREATE_TAB(box),
    ?CREATE_TAB(box_subscription),
    Backend:add_table_index(box, user),
    Backend:add_table_index(box, email),
    Backend:add_table_index(box_subscription, who),
    Backend:add_table_index(box_subscription, whom);
init(_) -> ok.

echo(_Resource, [String]) ->
    String.

create(_Resource, [Json]) ->
    Record = json_to_rec(Json),
    kvs:put(Record),
    <<"ok">>.

get(_Resource, [Index]) ->
    {ok, Rec} = kvs:get(box, Index),
    Json = rec_to_json(Rec),
    jsx:to_json(Json).

filter(_Resource, [Index, Value]) ->
    Data = kvs:all_by_index(box,
                     case Index of
                         <<"id">> -> #box.id;
                         <<"email">> -> #box.email;
                         <<"user">> -> #box.user
                     end,
                     Value),
    jsx:to_json(lists:map(fun(A)->rec_to_json(A)end, Data)).

json_to_rec([{<<"email">>, Email},
             {<<"id">>, Id},
             {<<"user">>, User}]) ->
    #box{id=Id, email=Email, user=User}.

rec_to_json(Rec) -> 
    [{<<"email">>, Rec#box.email},
     {<<"id">>, Rec#box.id},
     {<<"user">>, Rec#box.user}].
