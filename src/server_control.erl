%% @author zhuchaodi
%% @doc @todo Add description to server_control.


-module(server_control).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, kick_user/1,online_check/0]).
-define(SERVER, ?MODULE).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% init/1
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
handle_cast({kick_user_failed, UserName}, State) ->
	io:format("kick user failed UserName:[~p] invalid!~n", [UserName]),
	{noreply, State};
handle_cast({kick_user_success, UserName}, State) ->
	io:format("kick user :~p success!~n", [UserName]),
	{noreply, State};
handle_cast({online_check_ack, Count}, State) ->
	io:format("echo-server online count:~p~n", [Count]),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
start_link() ->
	process_flag(trap_exit,true),
	io:format("server_control process start_link ...~n"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

kick_user(UserName) ->
	gen_server:cast(server_userlist, {kick_user, UserName}).

online_check() ->
	gen_server:cast(server_userlist, {online_check}).
