-module(my_asteroid_chat_handler).

-export([handle/2, is_periodical/1]).
-export([login/1, send_message/1]).

-behaviour(asteroid_handler).

handle(FunctionName, Arguments) ->
  Function = erlang:binary_to_atom(FunctionName, utf8),
  ?MODULE:Function(Arguments).

is_periodical(FunctionName) ->
  case FunctionName of
    <<"login">> -> true;
    _ -> false
  end.

login([Username]) ->
  {session_ref, SessionRef} = chat:login(Username),
  {fun() ->
   chat:logout(SessionRef)
   end,
   jsx:encode([{<<"status">>, <<"SUCCESS">>},
               {<<"session_ref">>, SessionRef}])}.

send_message([SessionRef, Message]) ->
  case chat:send_message(SessionRef, Message) of
    ok -> jsx:encode([{<<"status">>, <<"SUCCESS">>},
                      {<<"result">>, <<"ok">>}]);
    error -> jsx:encode([{<<"status">>, <<"ERROR">>},
                         {<<"result">>, <<"You should login first">>}])
  end.
