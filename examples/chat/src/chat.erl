-module(chat).

-behaviour(gen_server).

%% API
-export([start_link/0, login/1, logout/0, send_message/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {clients}).

%%%===================================================================
%%% public API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Username) ->
  gen_server:call(?MODULE, {login, Username}).

logout() ->
  gen_server:call(?MODULE, {logout}).

send_message(Username, Message) ->
  gen_server:call(?MODULE, {message, Username, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{clients=[]}}.

handle_call({login, Username}, {Pid, _Ref}, State) ->
    Clients = State#state.clients ++ [{Username, Pid}],
    Mesage = erlang:iolist_to_binary([<<"User ">>, Username, <<" login">>]),
    self() ! {message, Mesage},
    {reply, ok, State#state{clients=Clients}};

handle_call({logout}, {Pid, _Ref}, State) ->
    Clients = lists:filter(fun({Username, UserPid}) -> 
                                 case Pid =/= UserPid of
                                   true -> true;
                                   false -> Message = erlang:iolist_to_binary([<<"User ">>, Username, <<" logout">>]),
                                            self() ! {message, Message},
                                            false
                                 end
                               end,
                           State#state.clients),
    {reply, ok, State#state{clients=Clients}};

handle_call({message, ClientUsername, ClientMessage}, _From, State) ->
    lists:foreach(fun({_Username, UserPid}) ->
                       UserPid ! {message, jsx:encode([{<<"username">>, ClientUsername},
                                                       {<<"message">>, ClientMessage}])}
                  end,
                  State#state.clients),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({message, MessageText}, State) ->
    lists:foreach(fun({_Username, UserPid}) ->
                      UserPid ! {message, jsx:encode([{<<"username">>, <<"CHAT SERVER">>},
                                                      {<<"message">>, MessageText}])}
                  end,
                  State#state.clients),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
