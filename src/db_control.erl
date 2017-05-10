%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2017 9:16
%%%-------------------------------------------------------------------
-module(db_control).
-author("zhuchaodi").
-include("table_name_def.hrl").
-include("mlogs.hrl").
%% API
-export([init/0, try_start/0, try_create_schema/0, try_create_table/0, init_group_info/0,select_all_group_info/0,select_all/4,
	insert_new_record/2, read_record/2, get_last/1, insert_chat_record/3,
	get_chat_record_size/0,get_size/1,order_chat_record_by_asc/1,
	select_all_chat_record/0,
	insert_user_info/2,update_user_passwd/2,select_user_info_by_username/1,
	select_group_info_by_groupid/1,
	update_group_info_by_groupid/1,
	wait_for_tables/0]).

init() ->
	Ret = try_create_schema(),

	case Ret of
		ok ->
			?LOGINFO("[db_control] Mnesia schema start success!~n");
		error ->
			?LOGINFO("[db_control] Mnesia schema start failed!~n")
	end,
	try_start(),
	try_create_table(),
	?LOGINFO("[db_control] Mnesia create table success!~n"),
	init_group_info().

try_create_schema() ->

	case mnesia:create_schema([node()]) of
		{error, Reason} ->
			case Reason of
				{_, {already_exists, _}} -> ok;
				_ ->
					?LOGINFO("[db_control] Mnesia create_schema create failed ! Reason:~p~n", [Reason]),
					error
			end;
		_->
			ok
	end.

try_start() ->
	mnesia:start().

try_create_table() ->
 	case mnesia:create_table(db_world_chat_record, [{record_name, chat_record_p}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_record_p)}]) of
 		{aborted,{already_exists,_}} ->
 			ok;
 		_->
 			error
 	end,
 	case mnesia:create_table(db_user_info, [{record_name, user_info_p}, {disc_copies, [node()]}, {attributes, record_info(fields, user_info_p)}]) of
 		{aborted,{already_exists,_}} ->
 			ok;
 		_->
 			error
 	end,
	case mnesia:create_table(db_group_info, [{record_name, group_info_p}, {disc_copies, [node()]}, {attributes, record_info(fields, group_info_p)}]) of
		{aborted,{already_exists,_}} ->
			?LOGINFO("[db_control] create db_group_info success~n"),
			ok;
		Ret->
			?LOGINFO("[db_control] Ret:~p~n", [Ret]),
			error
	end,
	mnesia:wait_for_tables([db_world_chat_record, db_user_info, db_group_info], 2000).

init_group_info() ->
	case select_all_group_info() of
		empty ->
			?LOGINFO("[db_control] init_group_info: try to init group info~n"),
			GroupA = #group_info_p{gp_id = "001", gp_title = "groupA", gp_userlist = []},
			GroupB = #group_info_p{gp_id = "002", gp_title = "groupB", gp_userlist = []},
			?LOGINFO("[db_control] GroupA :~p GroupB:~p~n", [GroupA, GroupB]),
			insert_new_record(db_group_info, GroupA),
			insert_new_record(db_group_info, GroupB);
		{not_empty,  QueryResult, RecordCount} ->
			?LOGINFO("[db_control] init_group_info: group info has already exists!~n QueryResult:~p , RecordCount~p ~n", [QueryResult, RecordCount])
	end.


%==============指定表操作

insert_user_info(UserName, PassWord) ->
	Row = #user_info_p{username = UserName, password = PassWord},
	insert_new_record(db_user_info, Row).

update_user_passwd(UserName, PassWord) ->
	Row = #user_info_p{username = UserName, password = PassWord},
	insert_new_record(db_user_info, Row).

select_user_info_by_username(UserName) ->
	case read_record(db_user_info, UserName) of
		[#user_info_p{username = UserName, password = PassWord}] -> {user_find, {UserName, PassWord}};
		_ -> {user_empty}
	end.

select_group_info_by_groupid(GroupId) ->
	Ret =
		case read_record(db_group_info, GroupId) of
			[#group_info_p{gp_id = GroupId} = GroupInfo] -> GroupInfo;
			_ -> {group_empty}
		end,
	?LOGINFO("[db_control] select_group_info_by_groupid Ret:~p~n", [Ret]),
	Ret.
update_group_info_by_groupid(NewGroupInfo) ->
	insert_new_record(db_group_info, NewGroupInfo).


select_all_group_info() ->
	MatchHead = #group_info_p{ _ = '_' },
	Guard = [],
	Result = ['$_'],
	select_all(db_group_info, MatchHead, Guard, Result).

select_all_chat_record() ->
	MatchHead = #chat_record_p{ _ = '_' },
	Guard = [],
	Result = ['$_'],
	select_all(db_world_chat_record, MatchHead, Guard, Result).


insert_chat_record(Id, UserName, Data) ->
	?LOGINFO("[db_control] db_control try to insert_chat_record~n"),
	Row = #chat_record_p{id = Id, username = UserName, word = Data},
	insert_new_record(db_world_chat_record, Row).

get_chat_record_size() ->
	get_size(db_world_chat_record).

order_chat_record_by_asc([X|Tail]) ->
	order_chat_record_by_asc([Y||Y<-Tail , Y#chat_record_p.id =< X#chat_record_p.id]) ++ [X]
	++ order_chat_record_by_asc([Y||Y<-Tail , Y#chat_record_p.id > X#chat_record_p.id]);

order_chat_record_by_asc([]) ->[].


%==============通用表操作
insert_new_record(TableName, Row) ->
	mnesia:dirty_write(TableName, Row).

read_record(TableName, Key) ->
	mnesia:dirty_read(TableName, Key).

get_last(TableName) ->
	mnesia:dirty_last(TableName).

get_size(TableName) ->
	mnesia:table_info(TableName, size).

select_all(TableName, MatchHead, Guard, Result) ->
	QueryResult = mnesia:dirty_select(TableName, [{MatchHead, Guard, Result}]),
	?LOGINFO("[db_control] QueryResult:~p~n", [QueryResult]),
	case length(QueryResult) of
		0 ->
			?LOGINFO("[db_control] empty~n"),
			empty;
		_->
			?LOGINFO("[db_control] not_empty QueryResult~n"),
			{not_empty, QueryResult, length(QueryResult)}
	end.

wait_for_tables() ->
	mnesia:wait_for_tables([db_world_chat_record, db_user_info, db_group_info], 2000).