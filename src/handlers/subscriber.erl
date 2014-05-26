-module(subscriber).

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribe, clients}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(Channel, Info) ->
    gen_server:call(?MODULE, {subscribe, Channel, Info}).

unsubscribe(Channel) ->
    gen_server:call(?MODULE, {unsubscribe, Channel}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, Subscribe} = eredis_sub:start_link(),
    ok = eredis_sub:controlling_process(Subscribe, self()),
    {ok, #state{subscribe=Subscribe, clients=[]}}.

handle_call({subscribe, Channel, {Uuid, Parent}}, {_Pid, _Ref}, State) ->
    case has_channel(State#state.clients, Channel) of
        false -> 
            io:format("Subscribed to ~p~n", [Channel]),
            ok = eredis_sub:subscribe(State#state.subscribe, [Channel]);
        true -> ok
    end,
    {reply, ok, State#state{clients=[{Channel, Uuid, Parent} | State#state.clients]}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({subscribed, _Channel, _Pid}, State) ->
    eredis_sub:ack_message(State#state.subscribe),
    {noreply, State};
handle_info({unsubscribed, _Channel, _Pid}, State) ->
    eredis_sub:ack_message(State#state.subscribe),
    {noreply, State};
handle_info(Message={message, Channel, Mes, _Pid}, State) ->
    io:format("Got message ~p~n", [Mes]),
    [Pid !  {rpc_done, Uuid, Mes} || {SubChannel, Uuid, Pid} <- State#state.clients, Channel =:= SubChannel],
    eredis_sub:ack_message(State#state.subscribe),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Unknown message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% internal functions 
%%%%%%===================================================================
has_channel(Clients, Channel) ->
    lists:any(fun({Ch, _, _}) -> Ch =:= Channel end,
              Clients).

