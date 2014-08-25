-module(asteroid_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state, {rpc_handlers, periodicals}).

init(_Transport, Req, Opts, _Active) ->
	io:format("bullet init~n"),
  RpcHandlers = proplists:get_value(rpc_handlers, Opts),
	{ok, Req, #state{rpc_handlers=RpcHandlers,
                   periodicals=[]}}.

stream(<<"ping">>, Req, State) ->
	{reply,
     jsx:encode([{<<"status">>, <<"pong">>}]),
     Req,
     State};
stream(Data, Req, State) ->
    Json = jsx:decode(Data),
    TerminateUuid = proplists:get_value(<<"terminate">>, Json),
    Function = proplists:get_value(<<"function">>, Json),
    Resource = proplists:get_value(<<"resource">>, Json),
    Uuid = proplists:get_value(<<"uuid">>, Json),
    Arguments = proplists:get_value(<<"arguments">>, Json),
  
    case TerminateUuid of 
      undefined ->
          Key = erlang:binary_to_atom(Resource, utf8),
          Module = case dict:is_key(Key, State#state.rpc_handlers) of
                    true -> dict:fetch(Key, State#state.rpc_handlers);
                    false ->dict:fetch(undefined, State#state.rpc_handlers)
          end,
          case call_rpc(Module, Function, Arguments, Uuid) of
            {periodical, Pid} -> {ok, Req, State#state{periodicals=[{Uuid, Pid} | State#state.periodicals]}};
            {ok} -> {ok, Req, State}
          end;
      _ -> 
        NewState = terminate_periodical(TerminateUuid, State),
        {reply,
         jsx:encode([{<<"uuid">>, Uuid},{<<"result">>, State#state.periodicals =/= NewState#state.periodicals}]),
         Req,
         NewState}
    end.

info({rpc_done, Uuid, Response}, Req, State) ->
    {reply,
     jsx:encode([{<<"uuid">>, Uuid}] ++ jsx:decode(Response)),
     Req,
     State}.

terminate(_Req, State) ->
	io:format("bullet terminate~n"),
  lists:foreach(fun({_Uuid, Pid}) ->
                    Pid ! {terminate}
                end,
                State#state.periodicals),
	ok.


terminate_periodical(Uuid, State) ->
  lists:foreach(fun({CurrentUuid, Pid}) ->
                    if CurrentUuid =:= Uuid -> asteroid_call_handler:stop(Pid);
                       true -> ok
                    end
                end,
                State#state.periodicals),
	State#state{periodicals=lists:filter(
      fun({CurrentUuid, _Pid}) -> CurrentUuid =/= Uuid end,
      State#state.periodicals)}.

call_rpc(Module, Function, Arguments, Uuid) ->
    Parent = self(),
    Periodical =  Module:is_periodical(Function),
    case Periodical of 
      true -> {periodical,
               erlang:spawn_link(fun()->
                                     {OnTerminate, Result} = Module:handle(Function,Arguments),
                                     Parent ! {rpc_done, Uuid, Result},
                                     rpc_loop(Parent, Uuid, OnTerminate)
                                 end)};
      false    -> erlang:spawn_link(fun() ->
                                      Parent ! {rpc_done, Uuid, Module:handle(Function, Arguments)}
                                  end),
                  {ok}
   end.


rpc_loop(Parent, Uuid, OnTerminate) -> 
  receive 
     {message, Message} -> Parent ! {rpc_done, Uuid, Message},
                           rpc_loop(Parent, Uuid, OnTerminate);
     {terminate} -> OnTerminate()
  end.
