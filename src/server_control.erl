%% @author zhuchaodi
%% @doc @todo Add description to server_control.


-module(server_control).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, kick_user/1,online_check/0,
	start_profiling/0,stop_profiling/0, analyze/0,get_server_userlst_info/0]).
-define(SERVER, ?MODULE).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {server_userlist_info = []}).
-include("common.hrl").
%% init/1
init([]) ->
	eprof:start(),
	io:format("eprof start~n"),
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
handle_cast({online_check_ack, ServerIndex, Count}, State) ->

	ServerList = [{ServerIndex, Count} | State#state.server_userlist_info],
	NewState =
		case length(ServerList) of
		?SERVERUSERLISTPOOL ->
			Total = lists:sum([ServerCount|| {ServerIndex, ServerCount}<-ServerList]),
			io:format("echo-server online count:~p~n", [Total]),
			State#state{server_userlist_info = []};
		_->
			State#state{server_userlist_info = ServerList}
	end,

	{noreply, NewState};

handle_cast(eprof_start_profiling, State) ->
	case eprof:start_profiling([server_userlist]) of
		profiling ->
			io:format("eprof start profiling~n");
		{error, Reason} ->
			io:format("eprof start failed Reason:~p~n", [Reason])
	end,
	{noreply, State};
handle_cast(eprof_stop_profiling, State) ->
	eprof:stop_profiling(),
	io:format("eprof stop profiling~n"),
	{noreply, State};
handle_cast(eprof_analyze, State) ->
	io:format("eprof_analyze REPORT:=============~n"),
	eprof:analyze(),
	io:format("eprof_analyze REPORT:End=============~n"),
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
	ServerList = ets_control:get_whole_server_userlist(),
	[gen_server:cast(ServerPid, {online_check})||{_ServerIndex, ServerPid}<-ServerList].
	%gen_server:cast(server_userlist, {online_check}).

start_profiling() ->
	gen_server:cast(server_control, eprof_start_profiling).

stop_profiling() ->
	gen_server:cast(server_control, eprof_stop_profiling).

analyze() ->
	gen_server:cast(server_control, eprof_analyze).

get_server_userlst_info()->
	gen_server:cast(server_userlist, get_process_info).