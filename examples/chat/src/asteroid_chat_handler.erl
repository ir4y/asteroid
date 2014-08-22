-module(asteroid_chat_handler).

-behaviour(asteroid_handler).
-behaviour(gen_server).

%%public API
-export([start_link/0,
         login/1,
         send_message/1]).
 
%% asteroid_handler callbacks
-export([handle/2, is_periodical/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {clients}).
-record(userinfo, {name, session, pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle(FunctionName, Arguments) ->
  Function = erlang:binary_to_atom(FunctionName, utf8),
  ?MODULE:Function(Arguments).

is_periodical(FunctionName) ->
  case FunctionName of
    <<"login">> -> true;
    _ -> false
  end.

login([Username]) ->
  {session_ref, SessionRef} = gen_server:call(?MODULE, {login, Username}),
  {fun() ->
       gen_server:call(?MODULE, {logout, SessionRef})
       end,
   jsx:encode([{<<"status">>, <<"SUCCESS">>},
               {<<"session_ref">>, SessionRef}])}.

send_message([SessionRef, Message]) ->
  case gen_server:call(?MODULE, {message, SessionRef, Message}) of
    ok -> jsx:encode([{<<"status">>, <<"SUCCESS">>},
                      {<<"result">>, <<"ok">>}]);
    error -> jsx:encode([{<<"status">>, <<"ERROR">>},
                         {<<"result">>, <<"You should login first">>}])
  end.
 
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{clients=[]}}.

handle_call({login, Username}, {Pid, _Ref}, State) ->
    SessionRef = erlang:list_to_binary(erlang:ref_to_list(make_ref())),
    Clients = State#state.clients ++ [#userinfo{name=Username,
                                                session=SessionRef,
                                                pid=Pid}],
    send_to_all(<<"CHAT SERVER">>,  
                erlang:iolist_to_binary([<<"User ">>,
                                         Username,
                                         <<" login">>]),
                Clients),
    {reply, {session_ref, SessionRef}, State#state{clients=Clients}};
handle_call({logout, SessionRef}, _From, State) ->
   {ok, User} = userinfo_by_session(SessionRef, State#state.clients),
    Clients = State#state.clients -- [User],
    send_to_all(<<"CHAT SERVER">>,  
                erlang:iolist_to_binary([<<"User ">>,
                                         User#userinfo.name,
                                         <<" logout">>]),
                Clients),
    {reply, ok, State#state{clients=Clients}};
handle_call({message, SessionRef, Message}, _From, State) ->
    case userinfo_by_session(SessionRef, State#state.clients) of
        {ok, User} -> send_to_all(User#userinfo.name,
                                  Message,
                                  State#state.clients),
                      {reply, ok, State};
        {error, _ErrorInfo} -> 
                      {reply, error, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_to_all(UserName, MessageText, Clients) ->
  lists:foreach(fun(User) ->
                      User#userinfo.pid ! {message,
                                           jsx:encode([{<<"username">>, UserName},
                                                       {<<"message">>, MessageText}])}
                end,
                Clients).

userinfo_by_session(SessionRef, Clients) ->
  case lists:filter(
    fun(User) -> 
        User#userinfo.session =:= SessionRef 
    end,
    Clients) of
    [User] -> {ok, User};
    [] -> {error, not_found}
  end.
