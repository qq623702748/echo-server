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
-export([start_link/1]).
-include("table_name_def.hrl").
%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([%init_group/0,
		rpc_handle_msg_sendto/2,
		rpc_msg_groupto/3,
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
		send_online_user_msg/3,
		send_group_user_msg/4,
		server_send_user_msg/2,
		send_user_msg/2,
		remove_socket/3]).

-define(SERVER, ?MODULE).
-include("mlogs.hrl").
-include("common.hrl").
-record(state, {userlist=[],
	world_chat_record_index,
	private_chat_record_index,
	group_chat_record_index,
	serverindex}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerIndex) ->
	process_flag(trap_exit,true),
	?LOGINFO("[server_userlist] process start_link ...~n"),
	gen_server:start_link(?MODULE, [ServerIndex], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerIndex]) ->
	%db_control:init(), 将由server_userlist_sup启动
	ets_control:add_new_server_userlist_mapper(ServerIndex, self()),
	db_control:wait_for_tables(),
	WorldChatRecordCountIndex = db_control:get_chat_record_size(),
	server_chat_cache:cache_init(WorldChatRecordCountIndex),
	ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, WorldChatRecordCountIndex),
	ets_control:insert_msg_queue_record(?PRIVATE_CHAT_BLACKBOARD, ServerIndex, 0),
	{ok, #state{world_chat_record_index = WorldChatRecordCountIndex, private_chat_record_index = 0, serverindex = ServerIndex, group_chat_record_index = 0}}.

handle_call(Request, _From, State) ->
	?LOGINFO("[server_userlist] handle_call start... Request:~p~n",[Request]),
	{reply, ok, State}.
%处理服务器提交请求业务
handle_cast({kick_user, UserName}, State) ->
	NewState = handle_msg_kick_user(UserName, State),
	{noreply, NewState};
handle_cast({online_check}, State) ->
	%io:format("recv server_control online_check~n"),
	handle_msg_online_check(State),
	{noreply, State};
handle_cast(get_process_info, State) ->
	?LOGINFO("[server_userlist] get_process_info~n~p~n", [erlang:process_info(self())]),
	{noreply, State};

%rpc_world_chat_record在ipc_control中定义，用于进程间通知消息已存储到ets中
handle_cast({rpc_world_chat_record, NewWorldChatRecordIndex},
			#state{world_chat_record_index = WorldChatRecordIndex,
					serverindex = ServerIndex} = State) ->

	?TRACE("[server_userlist][~p] rpc_world_chat_record Index[~p] NewIndex[~p]~n", [ServerIndex, WorldChatRecordIndex, NewWorldChatRecordIndex]),

	%(1)将异步收到的数据加入缓存中
	server_chat_cache:add_cache(NewWorldChatRecordIndex),
	%(2)读取最长可转发序列区间
	{WorldChatRecordIndex, MaxSendMsgIndex} = server_chat_cache:get_cache_continuous_sequence(ServerIndex),
	%(3)转发数据
	send_online_user_msg(WorldChatRecordIndex, MaxSendMsgIndex, State),
	%(4)删除已转发数据，并插入消息队列，已处理记录
	server_chat_cache:del_cache(ServerIndex, WorldChatRecordIndex, MaxSendMsgIndex),
	ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, MaxSendMsgIndex),
	{noreply, State#state{world_chat_record_index = MaxSendMsgIndex}};

%rpc_private_chat_record在ipc_control中定义，用于进程间通知消息已存储到ets中
handle_cast({rpc_private_chat_record, NewPrivateChatRecordIndex},
	#state{serverindex = ServerIndex, private_chat_record_index = PIdx} = State) ->
	%遍历从ChatRecordIndex到UpdateChatRecordIndex所有信息，并转发给用户
	?TRACE("[server_usrelist][~p] recv rpc_private_chat_record[~p][~p]~n", [ServerIndex, NewPrivateChatRecordIndex, PIdx]),
	rpc_handle_msg_sendto(NewPrivateChatRecordIndex, State),
	ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, NewPrivateChatRecordIndex),
	{noreply, State#state{private_chat_record_index = NewPrivateChatRecordIndex}};

%rpc_group_chat_record在ipc_control中定义，用于进程间通知消息已存储到ets中
handle_cast({rpc_group_chat_record, GroupId, NewGroupChatRecordIndex},
	#state{serverindex = ServerIndex,group_chat_record_index = GrouopChatRecordIndex} = State) ->
	%遍历从ChatRecordIndex到UpdateChatRecordIndex所有信息，并转发给用户
	?TRACE("recv rpc group chat record[~p] NewIndex:[~p] Index:[~p]~n", [ServerIndex, NewGroupChatRecordIndex, GrouopChatRecordIndex]),
	rpc_msg_groupto(NewGroupChatRecordIndex, GroupId, State),
	ets_control:insert_msg_queue_record(?GROUP_CHAT_BLACKBOARD, ServerIndex, NewGroupChatRecordIndex),
	{noreply, State#state{group_chat_record_index = NewGroupChatRecordIndex}};

%处理各个server_socket返回的tcp数据
handle_cast({SocketPid, Socket, Data}, State) ->
	?LOGINFO("[server_userlist] handle_cast State:[~p]~n", [State]),
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
			?LOGINFO("[server_userlist] recv peer socket data:~p~n", [Data])
	end,
	%mnesia:
	?LOGINFO("[server_userlist] NewState:[~p]~n", [NewState]),
	{noreply, NewState};
handle_cast({offline, Socket}, State) ->
	?LOGINFO("[server_userlist] user loffline!~n"),
	NewState = handle_msg_offline(Socket, State),
	{noreply, NewState}.

handle_info(_Info, State) ->
	?LOGINFO("[server_userlist] info... ~n"),
	{noreply, State}.

terminate(_Reason, _State) ->
	?LOGINFO("[server_userlist] terminate... ~n"),
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
			?LOGINFO("[server_userlist] kick_user_success~n"),
			OnlineList = remove_socket(UserList, Socket, UserName),
			?LOGINFO("[server_userlist] remove_socket success OnlineList:~p~n", [OnlineList]),
			State#state{userlist = OnlineList}
	end,
	?LOGINFO("[server_userlist] NewState:~p~n", [NewState]),
	NewState.
handle_msg_online_check(#state{userlist = UserList, serverindex = ServerIndex} = State) ->
	gen_server:cast(server_control, {online_check_ack, ServerIndex, length(UserList)}),
	State.

rpc_msg_groupto(GPNewIndex, GroupId,
	#state{userlist = UserList, serverindex = ServerIndex, group_chat_record_index = GPIndex} = State)
							when GPIndex < GPNewIndex->
	GroupInfo = db_control:select_group_info_by_groupid(GroupId),
	case GroupInfo of
		{grouop_empty} -> ok;
		_->
			GroupUserNameList = GroupInfo#group_info_p.gp_userlist,
			GroupTitle = GroupInfo#group_info_p.gp_title,
			GroupSocketList = get_group_socket(GroupUserNameList, UserList),
			?TRACE("[server_userlist][~p] GroupUserList[~p]~n", [ServerIndex, GroupUserNameList]),
			case ets_control:get_group_chat_record_single(GPIndex + 1) of
				[{Id, GroupId, UserName, Word}] ->
					send_group_user_msg(GroupSocketList, GroupTitle, UserName, Word);
				_->ok
			end,
			rpc_msg_groupto(GPNewIndex, GroupId, State#state{group_chat_record_index = GPIndex + 1})
	end;

rpc_msg_groupto(_GPNewIndex, _GroupId, _State) ->ok.



handle_msg_groupto(Socket, GroupId, Word, #state{userlist = UserList, serverindex = ServerIndex} = State) ->
	GroupInfo = db_control:select_group_info_by_groupid(GroupId),
	NewIndex =
		case GroupInfo of
		{grouop_empty} -> [];
		_->
			UserName = find_username_by_socket(UserList, Socket),
			GroupUserNameList = GroupInfo#group_info_p.gp_userlist,
			GroupTitle = GroupInfo#group_info_p.gp_title,
			GroupSocketList = get_group_socket(GroupUserNameList, UserList),
			send_group_user_msg(GroupSocketList, GroupTitle, UserName, Word),
			ServerUserList = ets_control:get_whole_server_userlist(),
			Id = ets_control:insert_group_chat_record(UserName, GroupId, Word),
			ipc_control:rpc_group_chat_msg(ServerUserList, ServerIndex, GroupId, Id),
			ets_control:insert_msg_queue_record(?GROUP_CHAT_BLACKBOARD,ServerIndex, Id),
			Id
	end,
	State#state{group_chat_record_index = NewIndex}.

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
	?LOGINFO("[server_userlist] GroupInfo:~p~n", [GroupInfo]),
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
handle_msg_sendto(Socket, DestUser, Word, #state{userlist = UserList,serverindex = ServerIndex} = State) ->
	?TRACE("[server_userlist] sendto dest user:[~p]~n", [DestUser]),
	UserName = find_username_by_socket(UserList, Socket),
	case find_username_by_user(UserList, DestUser) of 
		{find_user_invalid} ->
			%若私聊的人不在自己进程中，在发射信息到黑板中
			?TRACE("[server_userlist][~p] find_user_invalid!~n", [ServerIndex]),
			Id = ets_control:insert_private_chat_record(UserName, DestUser, Word),
			ServerUserList = ets_control:get_whole_server_userlist(),
			ipc_control:rpc_private_chat_msg(ServerUserList, ServerIndex, Id),
			ets_control:insert_msg_queue_record(?PRIVATE_CHAT_BLACKBOARD, ServerIndex, Id);
		{ok, {_, PeerSocket, _}} ->
			send_user_msg(PeerSocket, "[" ++ UserName ++ "]:" ++ Word)
	end,
	State.

%正常业务则检测目标用户名是否在该进程中，是则直接转发
rpc_handle_msg_sendto(NCRecordIndex,
			#state{private_chat_record_index = PCRecordIndex,
					userlist = UserList,
			        serverindex = ServerIndex} = State) when PCRecordIndex < NCRecordIndex->

	%?TRACE("[server_userlist] length:[~p]~n", [length(UserList)]),
	case ets_control:get_private_chat_record_single(PCRecordIndex + 1) of
		[{Id, UserName, DestUser, Word}] ->
			case find_username_by_user(UserList, DestUser) of
				{find_user_invalid} ->ok;
				{ok, {_, PeerSocket, _}} ->
					%?TRACE("[server_userlist] find DestUser~n"),
					send_user_msg(PeerSocket, "[" ++ UserName ++ "]:" ++ Word)
			end;
		_ ->ok
			%?TRACE("[server_userlist] rpc_private empty~n")
	end,
	rpc_handle_msg_sendto(NCRecordIndex,
		State#state{private_chat_record_index = PCRecordIndex + 1});

rpc_handle_msg_sendto(_NCRecordIndex, _State) ->
	ok.



handle_msg_offline(Socket, #state{userlist = UserList} = State) ->
	UserName = find_username_by_socket(UserList, Socket),
	?LOGINFO("[server_userlist] Username:[~p] offline ~n", [UserName]),
	OnlineList = remove_socket(UserList, Socket, UserName),
	State#state{userlist = OnlineList}.

handle_msg_modify_info(UserName, NewPassWord, Socket, ModifyInfo) ->
	Str = "[" ++ UserName ++ "," ++ NewPassWord ++ "] " ++ ModifyInfo,
	server_send_user_msg(Socket, Str).

handle_msg_modify(UserName, NewPassWord, Socket, State) ->
	?LOGINFO("[server_userlist] handle_msg_modify UserName[~p]  try to modify password~n", [UserName]),
	case user_control:user_modify_module(UserName, NewPassWord) of
		user_modify_success ->
			handle_msg_modify_info(UserName, NewPassWord, Socket, "modify success!");
		user_empty ->
			handle_msg_modify_info(UserName, NewPassWord, Socket, "modify failed!")
	end,
	State.

handle_msg_send_msg(Data, Socket,
	#state{userlist = UserList,
			world_chat_record_index = WorldChatRecordIndex,
			serverindex = ServerIndex} = State) ->

	UserName = find_username_by_socket(UserList, Socket),

	%(1)插入新的记录在公共ets表中，并获取插入数据后的ID值
	NewWorldChatRecordIndex = ets_control:insert_world_chat_record(UserName, Data),

	%(2)新插入记录加入聊天缓存中，表示此数据可读
	server_chat_cache:add_cache(NewWorldChatRecordIndex),

	%(3)通过【ipc模块】通知其他进程读取数据
	?TRACE("[server_userlist][~p] insert new rd:~p ~n", [ServerIndex, NewWorldChatRecordIndex]),
	ServerUserList = ets_control:get_whole_server_userlist(),
	ipc_control:rpc_world_chat_msg(ServerUserList, ServerIndex, NewWorldChatRecordIndex),


	%(4)从server_chat_cache缓存中遍历最长连续序列，并转发给用户,因为insert和cast存在不同步问题，必须要检测哪些数据可以转发
	{MinIndex, MaxSendMsgIndex} = server_chat_cache:get_cache_continuous_sequence(ServerIndex),
	?TRACE("[server_userlist][~p] cache:~p ~n", [ServerIndex, get(cache)]),
	server_chat_cache:del_cache(ServerIndex, MinIndex, MaxSendMsgIndex),
	send_online_user_msg(WorldChatRecordIndex, MaxSendMsgIndex, State),

	%(4)更新自身进程到server_msg_queue，表示自身已处理数据
	ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, MaxSendMsgIndex),
	?TRACE("[server_userlist][~p] MaxSendMsgIndex:~p ~n", [ServerIndex, MaxSendMsgIndex]),
	State#state{world_chat_record_index = MaxSendMsgIndex}.

handle_msg_online(UserName, Socket, From, #state{userlist = UserList} = State) ->
	NewState =
		case find_username_by_user(UserList, UserName) of
			{ok, {Pid, OldSocket, UserName}} ->
				server_send_user_msg(OldSocket, "server has kick you!"),
				OnlineList = remove_socket(UserList, OldSocket, UserName),
				ipc_control:kick_user(Pid, OldSocket),

				?LOGINFO("[server_userlist] [~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | OnlineList]};
			_ ->
				?LOGINFO("[server_userlist] [~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | State#state.userlist]}
		end,
	NewState.


handle_msg_passwd_invalid(Socket, From, State) ->
	server_send_user_msg(Socket, "user_password_invalid!"),
	ipc_control:user_password_invalid_to_socket(From, Socket),
	State.

handle_msg_login(UserName, PassWord, Socket, From, #state{serverindex = ServerIndex} = State) ->
	?LOGINFO("[server_userlist]UserName:~p Passwd:~p try to login! ~n", [UserName, PassWord]),
	NewState =
		case user_control:user_login_module(UserName, PassWord) of
			user_login_success ->
				?LOGINFO("[server_userlist][~p][~p] user_login_success~n", [ServerIndex, UserName]),
				gen_server:cast(From, {login_success, UserName}),
				handle_msg_online(UserName, Socket, From, State);
			user_password_invalid ->
				?LOGINFO("[server_userlist] user_password_invalid~n"),
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

%广播发送数据,并且发送通知其他ServerUserList接收数据以及更新自身的记录到WORLD_CHAT_BLACKBOARD
send_online_user_msg(ChatRecordIndex, ServerHandleCount,
	#state{userlist = List} = State) when ChatRecordIndex < ServerHandleCount->


	{_Id, UserName, Data} = ets_control:get_world_chat_record(ChatRecordIndex + 1, 1),

	OutData = "[" ++ UserName ++ "]:" ++ Data,
	?LOGINFO("[server_userlist] OutData:~p~n", [OutData]),
	[gen_tcp:send(DestSocket, term_to_binary(OutData))||{_,DestSocket,MUser} <- List,UserName /= MUser],
	send_online_user_msg(ChatRecordIndex + 1, ServerHandleCount, State);

send_online_user_msg(WorldChatRecordIndex, NewWorldChatRecordIndex, #state{serverindex = ServerIndex}) ->
	if
		WorldChatRecordIndex > NewWorldChatRecordIndex ->
			ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, WorldChatRecordIndex);
		true ->
			ets_control:insert_msg_queue_record(?WORLD_CHAT_BLACKBOARD, ServerIndex, NewWorldChatRecordIndex)
	end.
%讨论组发送数据
send_group_user_msg(GroupSocketList, GroupTitle, UserName, Word) ->
	OutData ="[" ++ GroupTitle ++ "]" ++ "[" ++ UserName ++ "]:" ++ Word,
	?LOGINFO("[server_userlist] OutData:~p~n", [OutData]),
	[gen_tcp:send(DestSocket, term_to_binary(OutData))||{_, DestSocket, DestUserName} <- GroupSocketList,DestUserName /= UserName].


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
	?LOGINFO("[server_userlist] Send info: ~p~n", [OutData]),
	Filter = [{Pid, X, Node} || {Pid, X, Node} <- List, X /= Socket],
	[gen_tcp:send(DestSocket, term_to_binary(OutData)) || {_, DestSocket, _} <- Filter],
	Filter.