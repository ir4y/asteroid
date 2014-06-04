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
  {fun() ->
    chat:logout()
   end,
   jsx:encode([{<<"status">>, <<"SUCCESS">>},
               {<<"result">>, chat:login(Username)}])}.

send_message([Username, Message]) ->
  jsx:encode([{<<"status">>, <<"SUCCESS">>},
              {<<"result">>, chat:send_message(Username, Message)}]).
