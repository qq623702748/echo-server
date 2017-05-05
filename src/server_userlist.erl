%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 四月 2017 15:44
%%%-------------------------------------------------------------------
-module(server_userlist).
-author("zhuchaodi").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-include("table_name_def.hrl").
%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([%init_group/0,
		 handle_msg_kick_user/2,
		 handle_msg_online_check/1,
		 handle_msg_groupto/4,
		 handle_msg_leave_group/3,
		 handle_msg_enter_group/3,
		 handle_msg_sendto/4,
		 handle_msg_offline/2,
		 handle_msg_modify_info/4,
		 handle_msg_modify/4,
		 handle_msg_send_msg/3,
		handle_msg_passwd_invalid/3,
		handle_msg_login/5,
		handle_msg_online/4,
		get_group_socket/2,
		add_group_user/2,
		del_group_user/2,
		update_group_user_list/2,
		%update_group_info/4,
		check_group_user/2,
		%get_group_Info/2,
		find_username_by_user/2,
		find_username_by_socket/2,
		send_online_info/2,
		send_online_user_msg/4,
		send_group_user_msg/5,
		server_send_user_msg/2,
		send_user_msg/2,
		remove_socket/3]).

-define(SERVER, ?MODULE).

-record(state, {tablename, userlist=[], chat_record_count}).
%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	process_flag(trap_exit,true),
	io:format("server_userlist process start_link ...~n"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	db_control:init(),
	TableName = ets_control:create_user_info_ets(),

	io:format("[info] server_userlist init TableName:~p ...~n", [TableName]),
	ChatRecordCount = db_control:get_chat_record_size(),
	{ok, #state{tablename = TableName,

		chat_record_count = ChatRecordCount}}.

handle_call(Request, _From, State) ->
	io:format("userlist handle_call start... Request:~p~n",[Request]),
	{reply, ok, State}.
%处理服务器提交请求业务
handle_cast({kick_user, UserName}, State) ->
	NewState = handle_msg_kick_user(UserName, State),
	{noreply, NewState};
handle_cast({online_check}, State) ->
	%io:format("recv server_control online_check~n"),
	handle_msg_online_check(State),
	{noreply, State};

%处理各个server_socket返回的tcp数据
handle_cast({SocketPid, Socket, Data}, State) ->
	io:format("server_userlist handle_cast State:[~p]~n", [State]),
	NewState = 
		case Data of
		{login, UserName, PassWord}->
			handle_msg_login(UserName, PassWord, Socket, SocketPid, State);
		{send_msg, SendData} ->
			handle_msg_send_msg(SendData, Socket, State);
		{modify, UserName, NewPassWord} ->
			handle_msg_modify(UserName, NewPassWord, Socket, State);
		{sendto, DestUserName, Word} ->
			handle_msg_sendto(Socket, DestUserName, Word, State);
		{enter_group, GroupId} ->
			handle_msg_enter_group(Socket, GroupId, State);
		{leave_group, GroupId} ->
			handle_msg_leave_group(Socket, GroupId, State);
		{groupto, GroupId, Word} ->
			handle_msg_groupto(Socket, GroupId, Word, State);
		get_world_chat_record ->
			handle_msg_get_world_chat_record(Socket, State);
		_ ->
			io:format("recv peer socket data:~p~n", [Data])
	end,
	%mnesia:
	io:format("server_userlist NewState:[~p]~n", [NewState]),
	{noreply, NewState};
handle_cast({offline, Socket}, State) ->
	io:format("user loffline!~n"),
	NewState = handle_msg_offline(Socket, State),
	{noreply, NewState}.

handle_info(_Info, State) ->
	io:format("userlist info... ~n"),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("userlist terminate... ~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%业务处理代码
handle_msg_get_world_chat_record(Socket, State) ->
	OutData =
		case db_control:select_all_chat_record() of
			empty ->
				[];
			{not_empty, QueryResult, _} ->
				db_control:order_chat_record_by_asc(QueryResult)
		end,
	send_user_msg(Socket, {chat_record_ack, OutData}),
	State.

handle_msg_kick_user(UserName, #state{userlist = UserList} = State) ->
	NewState = 
	case find_username_by_user(UserList, UserName) of
		{find_user_invalid} ->
			ipc_control:kick_failed(UserName),
			State;
		{ok, {Pid, Socket, UserName}} ->
			server_send_user_msg(Socket, "server has kick you!"),
			ipc_control:kick_user(Pid, "server active kick you"),
			ipc_control:kick_success(UserName),
			io:format("kick_user_success~n"),
			OnlineList = remove_socket(UserList, Socket, UserName),
			io:format("remove_socket success OnlineList:~p~n", [OnlineList]),
			State#state{userlist = OnlineList}
	end,
	io:format("NewState:~p~n", [NewState]),
	NewState.
handle_msg_online_check(#state{userlist = UserList} = State) ->
	gen_server:cast(server_control, {online_check_ack, length(UserList)}),
	State.

handle_msg_groupto(Socket, GroupId, Word, #state{userlist = UserList} = State) ->
	GroupInfo = db_control:select_group_info_by_groupid(GroupId),
	case GroupInfo of
		{grouop_empty} -> [];
		_->
			UserName = find_username_by_socket(UserList, Socket),
			GroupUserNameList = GroupInfo#group_info_p.gp_userlist,
			GroupTitle = GroupInfo#group_info_p.gp_title,
			GroupSocketList = get_group_socket(GroupUserNameList, UserList),
			send_group_user_msg(GroupSocketList, Socket, GroupTitle, UserName, Word)
	end,
	State.
handle_msg_leave_group(Socket, GroupId, #state{userlist = UserList} = State)->
	UserName = find_username_by_socket(UserList, Socket),
	GroupInfo = db_control:select_group_info_by_groupid(GroupId),

	NewState = 
	case length(check_group_user(GroupInfo, UserName)) of
		0 ->
			OutData = "You have not in this Group!",
			server_send_user_msg(Socket, OutData),
			State;
		1 ->
			NewGroupInfo = del_group_user(GroupInfo, UserName),
			db_control:update_group_info_by_groupid(NewGroupInfo),
			OutData = "leave GroupID:" ++ GroupId ++ " success!",
			server_send_user_msg(Socket, OutData),
			State
	end,
	NewState.
handle_msg_enter_group(Socket, GroupId, #state{userlist = UserList} = State)->
	UserName = find_username_by_socket(UserList, Socket),
	%GroupInfo = get_group_Info(GroupList, GroupId),
	GroupInfo = db_control:select_group_info_by_groupid(GroupId),
	io:format("[server_userlist] [info] GroupInfo:~p~n", [GroupInfo]),
	NewState = 
	case length(check_group_user(GroupInfo, UserName)) of
		0 ->
			NewGroupInfo = add_group_user(GroupInfo, UserName),
			db_control:update_group_info_by_groupid(NewGroupInfo),
			OutData = "Add GroupID:" ++ GroupId ++ " success!",
			server_send_user_msg(Socket, OutData),
			State;
		_ ->
			OutData = "You have been in this Group!",
			server_send_user_msg(Socket, OutData),
			State
	end,
	NewState.
handle_msg_sendto(Socket, DestUser, Word, #state{userlist = UserList} = State) ->
	io:format("sendto dest user:[~p]~n", [DestUser]),
	case find_username_by_user(UserList, DestUser) of 
		{find_user_invalid} -> [];
		{ok, {_, PeerSocket, _}} ->
			UserName = find_username_by_socket(UserList, Socket),
			send_user_msg(PeerSocket, "[" ++ UserName ++ "]:" ++ Word)
	end,
	State.

handle_msg_offline(Socket, #state{userlist = UserList} = State) ->
	UserName = find_username_by_socket(UserList, Socket),
	io:format("Username:[~p] offline ~n", [UserName]),
	OnlineList = remove_socket(UserList, Socket, UserName),
	State#state{userlist = OnlineList}.

handle_msg_modify_info(UserName, NewPassWord, Socket, ModifyInfo) ->
	Str = "[" ++ UserName ++ "," ++ NewPassWord ++ "] " ++ ModifyInfo,
	server_send_user_msg(Socket, Str).

handle_msg_modify(UserName, NewPassWord, Socket, State) ->
	io:format("[info] handle_msg_modify UserName[~p]  try to modify password~n", [UserName]),
	case user_control:user_modify_module(UserName, NewPassWord) of
		user_modify_success ->
			handle_msg_modify_info(UserName, NewPassWord, Socket, "modify success!");
		user_empty ->
			handle_msg_modify_info(UserName, NewPassWord, Socket, "modify failed!")
	end,
	State.

handle_msg_send_msg(Data, Socket, #state{userlist = UserList, chat_record_count = ChatRecordCount} = State) ->
	io:format("recv msg:~p ~n", [Data]),
	UserName = find_username_by_socket(UserList, Socket),
	send_online_user_msg(UserList, Socket, UserName, Data),
	UpdataChatRecordCount = ChatRecordCount + 1,
	db_control:insert_chat_record(UpdataChatRecordCount, UserName, Data),
	State#state{chat_record_count = UpdataChatRecordCount}.

handle_msg_online(UserName, Socket, From, #state{userlist = UserList} = State) ->
	NewState =
		case find_username_by_user(UserList, UserName) of
			{ok, {Pid, OldSocket, UserName}} ->
				server_send_user_msg(OldSocket, "server has kick you!"),
				OnlineList = remove_socket(UserList, OldSocket, UserName),
				ipc_control:kick_user(Pid, OldSocket),

				io:format("[~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | OnlineList]};
			_ ->
				io:format("[~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | State#state.userlist]}
		end,
	NewState.


handle_msg_passwd_invalid(Socket, From, State) ->
	server_send_user_msg(Socket, "user_password_invalid!"),
	ipc_control:user_password_invalid_to_socket(From, Socket),
	State.

handle_msg_login(UserName, PassWord, Socket, From, State) ->
	io:format("UserName:~p Passwd:~p try to login! ~n", [UserName, PassWord]),
	NewState =
		case user_control:user_login_module(UserName, PassWord) of
			user_login_success ->
				io:format("user_login_success~n"),
				io:format("From:~p~n", [From]),
				gen_server:cast(From, login_success),
				handle_msg_online(UserName, Socket, From, State);
			user_password_invalid ->
				io:format("[info] user_password_invalid~n"),
				handle_msg_passwd_invalid(Socket, From, State)
		end,
	NewState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_group_socket(GroupUserNameList, UserList) ->
	[{Pid, Socket, UserName}|| MUserName<-GroupUserNameList, {Pid, Socket, UserName} <- UserList, MUserName==UserName].

%添加用户到指定的GroupInfo中
add_group_user(GroupInfo, UserName) ->
	#group_info_p{gp_id = GroupInfo#group_info_p.gp_id,
		   gp_title = GroupInfo#group_info_p.gp_title,
		   gp_userlist = [UserName|GroupInfo#group_info_p.gp_userlist]}.

%在GroupInfo中删除用户
del_group_user(GroupInfo, UserName) ->
	MList = GroupInfo#group_info_p.gp_userlist,
	NewUserList = update_group_user_list(MList, UserName),
	#group_info_p{gp_id = GroupInfo#group_info_p.gp_id, gp_title = GroupInfo#group_info_p.gp_title, gp_userlist = NewUserList}.

%更新UserList的列表（删除指定用户）
update_group_user_list(UserList, UserName) ->
	[Name|| Name <- UserList, Name /= UserName].


%通过GroupList获取指定讨论组用户列表
check_group_user(GroupInfo, UserName) ->
	[MUserName ||MUserName <- GroupInfo#group_info_p.gp_userlist, MUserName =:= UserName].

%通过username查询用户是否存在
find_username_by_user(List, UserName) ->
	Node = [{Pid, Socket, UserName} || {Pid, Socket, MUser} <- List, MUser =:= UserName],
	case length(Node) of
		0 -> {find_user_invalid};
		1  ->
			[Ret] = Node,
			{ok,Ret}
	end.

%通过socket寻找用户Username
find_username_by_socket(UserList, Socket) ->
	[Ret] = [UserName|| {_, DestSocket, UserName} <- UserList, DestSocket =:= Socket],
	Ret.

%广播在线消息
send_online_info(List, UserName) ->
	OutData = "[" ++ UserName ++ "]online!",
	[gen_tcp:send(Socket, term_to_binary(OutData))||{_, Socket,_} <- List].

%广播发送数据
send_online_user_msg(List, Socket, UserName, Data) ->
	OutData = "[" ++ UserName ++ "]:" ++ Data,
	io:format("OutData:~p~n", [OutData]),
	[gen_tcp:send(DestSocket, term_to_binary(OutData))||{_,DestSocket,_} <- List,DestSocket /= Socket].

%讨论组发送数据
send_group_user_msg(GroupSocketList, Socket, GroupTitle, UserName, Word) ->
	OutData ="[" ++ GroupTitle ++ "]" ++ "[" ++ UserName ++ "]:" ++ Word,
	io:format("OutData:~p~n", [OutData]),
	[gen_tcp:send(DestSocket, term_to_binary(OutData))||{_, DestSocket, _} <- GroupSocketList,DestSocket /= Socket].


%发送给指定用户
server_send_user_msg(Socket, Data) ->
	OutData = "[server ack]:" ++ Data,
	gen_tcp:send(Socket, term_to_binary(OutData)).

send_user_msg(Socket, Data) ->
	OutData = Data,
	gen_tcp:send(Socket, term_to_binary(OutData)).

%移除下线的Socket
remove_socket(List, Socket, UserName) ->
	OutData = "[" ++ UserName ++ "]offline!",
	io:format("Send info: ~p~n", [OutData]),
	Filter = [{Pid, X, Node} || {Pid, X, Node} <- List, X /= Socket],
	[gen_tcp:send(DestSocket, term_to_binary(OutData)) || {_, DestSocket, _} <- Filter],
	Filter.