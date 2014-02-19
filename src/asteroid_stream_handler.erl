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

stream(<<"ping: ", Name/binary>>, Req, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
    [{<<"function">>, Function},
     {<<"resource">>, Resource},
     {<<"arguments">>,Arguments}] = jsx:decode(Data),
    Handler = dict:fetch(erlang:binary_to_atom(Resource, utf8),
                         State#state.rpc_handlers),
    Responce = Handler:(erlang:binary_to_atom(Function, utf8))(Resource, Arguments),
    {reply, Responce, Req, State}.

info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.
