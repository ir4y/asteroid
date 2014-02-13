-module(kvs_box).

-include_lib("kvs/include/kvs.hrl").

-record(box,{id,user,email}).
-record(box_subscription,{who,whom}).

-export([init/1]).

init(Backend=store_mnesia) ->
    ?CREATE_TAB(box),
    ?CREATE_TAB(box_subscription),
    Backend:add_table_index(box, user),
    Backend:add_table_index(box, email),
    Backend:add_table_index(box_subscription, who),
    Backend:add_table_index(box_subscription, whom);
init(_) -> ok.
