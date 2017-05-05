%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 四月 2017 10:06
%%%-------------------------------------------------------------------
-module(ipc_control).
-author("zhuchaodi").

%% API
-compile(export_all).

%此文件是mServer的子模块，用于定义所有IPC通信内容
kick_user_by_name(Pid, UserName) ->
	Pid ! {kick_user_by_name, UserName}.

kick_user(Pid, Reason) ->
	gen_server:cast(Pid, {kick_user, Reason}).
	%Pid ! {kick_user_by_socket, Socket}.

offline_to_msg(UserListPid, Socket) ->
	UserListPid ! {offline, Socket}.

%server查询在线人数调用函数
online_check(MsgPid) ->
	MsgPid ! {online_check}.

%发送给server中Socket对应的进程(SocketPid) user_password_invalid信号
%作用：提示指定socket进程，用户登录失败,需要主动关闭socket
user_password_invalid_to_socket(SocketPid, Socket)->
	SocketPid ! {user_password_invalid, Socket},
	gen_server:cast(SocketPid, user_password_invalid).

try_modify_password_to_msg(UserListPid, Socket, UserName, NewPassWord)->
	UserListPid!{try_modify_password, Socket, {UserName, NewPassWord}}.

login_to_msg(UserListPid, Socket, LoginData)->
	UserListPid!{login, self(), Socket, binary_to_term(LoginData)}.

invalid_username_to_msg(UserListPid, Socket)->
	UserListPid!{invalid_username, Socket}.

sendto_user_to_msg(UserListPid, Socket, DestUser, Word)->
	UserListPid!{sendto_user, Socket, {DestUser, Word}}.

groupto_to_msg(UserListPid, Socket, GroupId, Word)->
	UserListPid!{groupto, Socket, {GroupId, Word}}.

enter_group_to_msg(UserListPid, Socket, GroupId)->
	UserListPid!{enter_group, Socket, GroupId}.

leave_group_to_msg(UserListPid, Socket, GroupId)->
	UserListPid !{leave_group, Socket, GroupId}.

send_msg_to_msg(UserListPid, Socket, Bin)->
	UserListPid!{send_msg, Socket, binary_to_term(Bin)}.

%心跳接受请求后响应
heart_beat_info(Socket, Cnt) ->
	case Cnt >= 5 of 
		true ->
			ipc_control:offline_to_msg(server_userlist, Socket);
		_ ->
			gen_tcp:send(Socket, term_to_binary({heart_beat_ack, Cnt+1}))
	end.

%用户被踢后响应
kick_user_ack(Socket, Reason) ->
	gen_tcp:send(Socket, term_to_binary(Reason)).

%用户在执行server_control以后返回异步请求给sever_control剔除用户成功
kick_failed(UserName) ->
	io:format("answer server_control kick_failed!~n"),
	gen_server:cast(server_control, {kick_user_failed, UserName}).
kick_success(UserName) ->
	io:format("answer server_control kick_success!~n"),
	gen_server:cast(server_control, {kick_user_success, UserName}).

insert_time_stamp(LimitList) ->
	{MegaSecs, Secs, _} = erlang:timestamp(),
	TimeStamp = MegaSecs * 1000000 + Secs,
	if 
		length(LimitList) > 50 ->
			[Last | Tail] = LimitList,
			if 
				TimeStamp - Last > 60 ->
					NextLimitList = lists:reverse([TimeStamp| lists:reverse(Tail)]),
					{ok, NextLimitList};
				TimeStamp - Last =< 60 ->
					NextLimitList = LimitList,
					{failed, NextLimitList}
			end;
		length(LimitList) =< 50 ->
			NextLimitList = lists:reverse([TimeStamp|lists:reverse(LimitList)]),
			{ok, NextLimitList}
	end.