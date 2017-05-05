%% @author zhuchaodi
%% @doc @todo Add description to server_socket.


-module(server_socket).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(Timeout, 120*1000).
-define(WAIT_FOR_LOGIN,0).
-define(LOGIN,1).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,handle_limit_word/3]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket, status, limitlist = []}).

%% init/1
init([Socket]) ->
	io:format("server_socket start.. Socket[~p] .. ~n", [Socket]),
    inet:setopts(Socket, [{active, once}]),
	State = #state{socket = Socket, status = ?WAIT_FOR_LOGIN},
    {ok, State}.


%% handle_call/3
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% handle_cast/2
handle_cast(login_success, State) ->
	NewState = State#state{status = ?LOGIN},
	io:format("server_socket recv login success! State:~p~n", [NewState]),
	{noreply, NewState};
handle_cast({kick_user, Reason}, #state{socket = Socket} = State) ->
	io:format("kick_user recv data Reason~p~n", [Reason]),
	ipc_control:kick_user_ack(Socket, Reason),
	gen_tcp:close(Socket),
	io:format("socket closed~n"),
	{noreply, State};
handle_cast(user_password_invalid, #state{socket = Socket} = State) ->
	gen_tcp:close(Socket),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
handle_info({tcp, Socket, Bin}, State) ->
	inet:setopts(Socket, [{active, once}]),
	Data = binary_to_term(Bin),
	NewState = case Data of
		{heart_beat_req, Cnt} ->
			%心跳包响应
			ipc_control:heart_beat_info(Socket, Cnt),
			State;
		{login, _UserName, _PassWord} ->
			%登录请求不需要拦截处理
			gen_server:cast(server_userlist, {self(), Socket, Data}),
			State;
		_-> 
			handle_limit_word(Socket, Data, State)
	end,
	{noreply, NewState, ?Timeout};

handle_info({tcp_closed, Socket}, State) ->
    io:format("tcp handler info ~p clent disconnected ~n", [self()]),
    gen_server:cast(server_userlist, {offline, Socket}),
	gen_tcp:close(Socket),
    {stop, normal, State};

handle_info(timeout, State) ->
    io:format("tcp handler info ~p client connection timeout~n", [self()]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
terminate(_Reason, _State) ->
    ok.


%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(Socket) ->
    io:format("server_socket start_link~n"),
    gen_server:start_link(?MODULE, [Socket], []).

%限制一分钟最多发送50条信息
handle_limit_word(Socket, Data, #state{limitlist = LimitList, status = Status} = State) when Status =:= ?LOGIN->
	NewState = 
	case Data of
		{sendto, _DestUserName, _Word} ->
			NewLimitList = 
			case ipc_control:insert_time_stamp(LimitList) of
			{ok, NextLimitList} ->
				gen_server:cast(server_userlist, {self(), Socket, Data}),
				NextLimitList;
			{failed, NextLimitList} ->
				io:format("out of limit~n"),
				NextLimitList
			end,
			State#state{limitlist = NewLimitList};
		{send_msg, _SendData} ->
			NewLimitList = 
			case ipc_control:insert_time_stamp(LimitList) of
			{ok, NextLimitList} ->
				gen_server:cast(server_userlist, {self(), Socket, Data}),
				NextLimitList;
			{failed, NextLimitList} ->
				io:format("out of limit~n"),
				NextLimitList
			end,
			State#state{limitlist = NewLimitList};
		{enter_group, _GroupId} ->
			NewLimitList = 
			case ipc_control:insert_time_stamp(LimitList) of
			{ok, NextLimitList} ->
				gen_server:cast(server_userlist, {self(), Socket, Data}),
				NextLimitList;
			{failed, NextLimitList} ->
				io:format("out of limit~n"),
				NextLimitList
			end,
			State#state{limitlist = NewLimitList};
		_->
			gen_server:cast(server_userlist, {self(), Socket, Data}),
			State#state{limitlist = LimitList}
	end,
	NewState;

handle_limit_word(_Socket, _Data, State) ->
	io:format("user has not login!~n"),
	State.