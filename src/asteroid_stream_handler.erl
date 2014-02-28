-module(asteroid_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state, {rpc_handlers}).

init(_Transport, Req, Opts, _Active) ->
	io:format("bullet init~n"),
    RpcHandlers = proplists:get_value(rpc_handlers, Opts),
	{ok, Req, #state{rpc_handlers=RpcHandlers}}.

stream(<<"ping">>, Req, State) ->
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
    Json = jsx:decode(Data),
    Function = proplists:get_value(<<"function">>, Json),
    Resource = proplists:get_value(<<"resource">>, Json),
    Uuid = proplists:get_value(<<"uuid">>, Json),
    Arguments = proplists:get_value(<<"arguments">>, Json),
    Handler = dict:fetch(erlang:binary_to_atom(Resource, utf8),
                         State#state.rpc_handlers),
    Parent = self(),
    erlang:spawn(fun() ->
                         Response = Handler:(erlang:binary_to_atom(Function, utf8))(Resource, Arguments),
                         Parent ! {rpc_done, Uuid, Response}
                 end),
    {ok, Req, State}.

info({rpc_done, Uuid, Response}, Req, State) ->
    {reply,
     erlang:binary_to_list(Uuid) ++ ":" ++ Response,
     Req,
     State};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.
