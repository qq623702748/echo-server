%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 四月 2017 9:08
%%%-------------------------------------------------------------------
-module(ets_control).
-author("zhuchaodi").
-include("common.hrl").
-include("mlogs.hrl").
%% API
-export([create_server_userlist_mapper/0,
	add_new_server_userlist_mapper/2,
	add_new_server_login_mapper/2,
	get_server_login_by_hash/1,
	get_server_userlist_by_hash/1,
	get_server_userlist_by_index/1,
	get_whole_server_userlist/0,
	get_private_chat_record_single/1,
	get_group_chat_record_single/1,
	create_server_login_mapper/0,
	create_server_msg_queue_blackboard/1,
	create_server_world_chat_blackboard/1,
	create_server_private_chat_blackboard/1,
	create_server_group_chat_blackboard/1,

	insert_msg_queue_record/3,
	insert_world_chat_record/2,
	insert_group_chat_record/3,
	insert_private_chat_record/3,
	clean_world_chat_msg/0,
	loop_delete_msg/3,

	get_world_chat_record/2]).



%%所有对于ETS表的操作均放在此文件中

create_server_userlist_mapper() ->
	ets:new(?SERVERMAPPER, [public, bag, named_table, {read_concurrency, true}]),%创建服务器映射表
	io:format("create table success , table name:~p~n", [?SERVERMAPPER]),
	?SERVERMAPPER.

create_server_login_mapper() ->
	ets:new(?SERVERLOGINMAPPER, [public, bag, named_table, {read_concurrency, true}]),%创建服务器映射表
	io:format("create table success , table name:~p~n", [?SERVERMAPPER]),
	?SERVERMAPPER.

create_server_msg_queue_blackboard(LastChatIndex) ->
	ets:new(?MSG_QUEUE_BLACKBOARD, [public, set, named_table]), %创建服务器公共黑板
	io:format("create table success , table name:~p~n", [?MSG_QUEUE_BLACKBOARD]),
	ets:insert(?MSG_QUEUE_BLACKBOARD, {prev_handle_seq, LastChatIndex}),

	?MSG_QUEUE_BLACKBOARD.

create_server_world_chat_blackboard(LastChatIndex) ->
	ets:new(?WORLD_CHAT_BLACKBOARD, [public, set, named_table, {read_concurrency, true}]), %创建服务器公共黑板
	io:format("create table success , table name:~p~n", [?WORLD_CHAT_BLACKBOARD]),
	ets:insert(?WORLD_CHAT_BLACKBOARD, {seq, LastChatIndex}),
	?WORLD_CHAT_BLACKBOARD.

create_server_private_chat_blackboard(LastChatIndex) ->
	ets:new(?PRIVATE_CHAT_BLACKBOARD, [public, set, named_table, {read_concurrency, true}]), %创建服务器公共黑板
	io:format("create table success , table name:~p~n", [?PRIVATE_CHAT_BLACKBOARD]),
	ets:insert(?PRIVATE_CHAT_BLACKBOARD, {seq, LastChatIndex}),
	?PRIVATE_CHAT_BLACKBOARD.

create_server_group_chat_blackboard(LastChatIndex) ->
	ets:new(?GROUP_CHAT_BLACKBOARD, [public, set, named_table, {read_concurrency, true}]), %创建服务器公共黑板
	io:format("create table success , table name:~p~n", [?GROUP_CHAT_BLACKBOARD]),
	ets:insert(?GROUP_CHAT_BLACKBOARD, {seq001, LastChatIndex}),
	ets:insert(?GROUP_CHAT_BLACKBOARD, {seq002, LastChatIndex}),
	?GROUP_CHAT_BLACKBOARD.

%ServerHandleType 必须使用前面三个黑板的宏定义
insert_msg_queue_record(ServerHandleType, ServerIndex, ServerHandleCount) ->
	%?TRACE("[ets_control] ServerIndex:[~p] ServerHandleType:[~p], ServerHandleCount:[~p]~n", [ServerIndex, ServerHandleType, ServerHandleCount]),
	ets:insert(?MSG_QUEUE_BLACKBOARD, {{ServerHandleType, ServerIndex}, ServerHandleCount}).


insert_world_chat_record(UserName, Word) ->
	Id = ets:update_counter(?WORLD_CHAT_BLACKBOARD, seq, 1),
	ets:insert(?WORLD_CHAT_BLACKBOARD, {Id, UserName, Word}),
	Id.

insert_group_chat_record(UserName, GroupId, Word) ->
	Id = ets:update_counter(?GROUP_CHAT_BLACKBOARD, seq001, 1),
	ets:insert(?GROUP_CHAT_BLACKBOARD, {Id, GroupId, UserName, Word}),
	Id.
	%case GroupId of
	%	"001" ->
	%		Id = ets:update_counter(?GROUP_CHAT_BLACKBOARD, seq001, 1),
	%		ets:insert(?GROUP_CHAT_BLACKBOARD, {Id, UserName, Word});
	%	"002" ->
	%		Id = ets:update_counter(?GROUP_CHAT_BLACKBOARD, seq002, 1),
	%		ets:insert(?GROUP_CHAT_BLACKBOARD, {Id, UserName, Word})
	%end.

insert_private_chat_record(UserName, DestUser, Word) ->
	Id = ets:update_counter(?PRIVATE_CHAT_BLACKBOARD, seq, 1),
	ets:insert(?PRIVATE_CHAT_BLACKBOARD, {Id, UserName, DestUser, Word}),
	Id.


add_new_server_userlist_mapper(ServerIndex, ServerPid) ->
	io:format("add new mapper ServerIndex:[~p], ServerPid:[~p]~n", [ServerIndex, ServerPid]),
	ets:insert(?SERVERMAPPER, {ServerIndex, ServerPid}).

add_new_server_login_mapper(ServerIndex, ServerPid) ->
	io:format("add new login mapper ServerIndex:[~p], ServerPid:[~p]~n", [ServerIndex, ServerPid]),
	ets:insert(?SERVERLOGINMAPPER, {ServerIndex, ServerPid}).



get_server_login_by_hash(HashCode) when is_integer(HashCode)->
	ServerIndex = HashCode rem ?SERVERLOGINPOOL + 1,
	[{ServerIndex, ServerPid}] = ets:lookup(?SERVERLOGINMAPPER, ServerIndex),
	ServerPid.

get_server_userlist_by_index(ServerIndex) when ServerIndex > 0->
	[{ServerIndex, ServerPid}] = ets:lookup(?SERVERMAPPER, ServerIndex),
	ServerPid.

get_server_userlist_by_hash(HashCode) when is_integer(HashCode)->
	ServerIndex = HashCode rem ?SERVERUSERLISTPOOL + 1,
	[{ServerIndex, ServerPid}] = ets:lookup(?SERVERMAPPER, ServerIndex),
	ServerPid.

get_whole_server_userlist() ->
	get_whole_userlist(?SERVERUSERLISTPOOL, []).

get_whole_userlist(PoolIndex, L) when PoolIndex >= 1->
	[{ServerIndex, ServerPid}] = ets:lookup(?SERVERMAPPER, PoolIndex),
	get_whole_userlist(PoolIndex - 1, [{ServerIndex, ServerPid}|L]);
get_whole_userlist(_PoolIndex, L) ->
	L.


clean_world_chat_msg() ->
	Ret = get_server_msg_queue(?WORLD_CHAT_BLACKBOARD, ?SERVERUSERLISTPOOL, []),
	DelIndex = case Ret of
		[_|_] ->
			lists:min(Ret);
		_->
			0
	end,
	%注意，如果以后要适配讨论组和私聊，需要额外添加4个记录
	PrevHandle = case ets:lookup(?MSG_QUEUE_BLACKBOARD, prev_handle_seq) of
		[{prev_handle_seq, PrevHandleSeq}] ->
			PrevHandleSeq;
		_-> 0
	end,
	loop_insert_world_chat_record(PrevHandle, DelIndex),
	loop_delete_msg(?WORLD_CHAT_BLACKBOARD,PrevHandle, DelIndex).

get_server_msg_queue(ServerHandleType, ServerUserListIndex, Ret) when ServerUserListIndex >=1 ->
	% 查找ets消息队列表中各个进程记录的已操作记录数目
	case ets:lookup(?MSG_QUEUE_BLACKBOARD, {ServerHandleType, ServerUserListIndex}) of
		[{{ServerHandleType, ServerIndex}, HandleCount}] ->
			get_server_msg_queue(ServerHandleType, ServerIndex - 1, [HandleCount|Ret]);
		_->
			get_server_msg_queue(ServerHandleType, ServerUserListIndex - 1, Ret)
	end;
get_server_msg_queue(_ServerHandleType, _ServerUserListIndex, Ret) ->
	% 查找ets消息队列表中各个进程记录的已操作记录数目
	Ret.

get_private_chat_record_single(Id) ->
	ets:lookup(?PRIVATE_CHAT_BLACKBOARD, Id).

get_group_chat_record_single(Id) ->
	ets:lookup(?GROUP_CHAT_BLACKBOARD, Id).

loop_insert_world_chat_record(PrevHandleIndex, NewHandleIndex)
							when PrevHandleIndex == 0->
	loop_insert_world_chat_record(PrevHandleIndex + 1, NewHandleIndex);

loop_insert_world_chat_record(PrevHandleIndex, NewHandleIndex)
							when PrevHandleIndex =< NewHandleIndex->
	case ets:lookup(?WORLD_CHAT_BLACKBOARD, PrevHandleIndex) of
		[{PrevHandleIndex, UserName, Word}] ->
			db_control:insert_chat_record(PrevHandleIndex, UserName, Word);
		_->
			ok
	end,
	loop_insert_world_chat_record(PrevHandleIndex + 1, NewHandleIndex);
loop_insert_world_chat_record(_PrevHandleIndex, _NewHandleIndex) -> ok.

loop_delete_msg(TableName, PrevHandle, DelIndex) when PrevHandle =< DelIndex->
	%?TRACE("[ets_control] ready to delete [~p]~n", [PrevHandle]),
	ets:delete(TableName, PrevHandle),
	loop_delete_msg(TableName, PrevHandle + 1, DelIndex);
loop_delete_msg(TableName, _PrevHandle, DelIndex) ->
	ets:insert(?MSG_QUEUE_BLACKBOARD, {prev_handle_seq, DelIndex}).

get_world_chat_record(Id, TryLoopCnt) when TryLoopCnt > 0->
	Result = case ets:lookup(?WORLD_CHAT_BLACKBOARD, Id) of
		[Ret] ->
			Ret;
		Error->
			io:format("[~p] recordID:~p orrured error:~p try again! Cnt:~p ~n", [?WORLD_CHAT_BLACKBOARD, Id, Error, 20-TryLoopCnt+1]),
			get_world_chat_record(Id, TryLoopCnt - 1)
	end,
	Result;
get_world_chat_record(Id, _TryLoopCnt) ->
	io:format("Record Id:~p give up!~n", [Id]),
	{Id, "Error", "Record error to get"}.

sleep(TimeOut) ->
	receive
		after 1000 ->
			ok
	end.