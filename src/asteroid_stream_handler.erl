-module(asteroid_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-export([echo/4, create/4, get/4]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	{ok, Req, state}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
    [{<<"function">>, Function},
     {<<"resource">>, Resource},
     {<<"arguments">>,Arguments}] = jsx:decode(Data),
    ?MODULE:(erlang:binary_to_atom(Function, utf8))(Resource, Arguments, Req, State).

info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.


echo(_Resource, [String], Req, State) ->
    {reply, String, Req, State}.

create(Resource, [Json], Req, State) ->
    Module = (erlang:binary_to_atom(Resource, utf8)),
    Record = Module:json_to_rec(Json),
    kvs:put(Record),
    {reply, <<"ok">>, Req, State}.

get(Resource, [Index], Req, State) ->
    Module = (erlang:binary_to_atom(Resource, utf8)),
    {ok, Rec} = kvs:get(Module:bucket(), Index),
    Json = Module:rec_to_json(Rec),
    {reply, jsx:to_json(Json), Req, State}.
