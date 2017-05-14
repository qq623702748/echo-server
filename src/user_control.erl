%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 四月 2017 9:06
%%%-------------------------------------------------------------------
-module(user_control).
-author("zhuchaodi").
%% API
-export([user_login_module/2, user_modify_module/2
		%, user_login/2,user_modify/2
		]).

-include("mlogs.hrl").
user_login_module(UserName, PassWord) ->
	?LOGINFO("[user_control] user_login_module start~n"),
	case user_login(UserName, PassWord) of

		{user_login_success, _UserName, PassWord} ->
			user_login_success;

		{user_password_invalid, UserName, PassWord}->
			user_password_invalid
	end.

user_modify_module(UserName, NewPassWord) ->
	?LOGINFO("[user_control] user_modify_module~n"),
	case user_modify(UserName, NewPassWord) of
		{user_modify_success, UserName, NewPassWord} ->
			user_modify_success;
		{user_empty} ->
			user_empty
	end.
%% 用户功能
% 用户可以通过

%用户登录
user_login(UserName, PassWord) ->
	%调用ets_controll模块查询用户是否存在，
	?LOGINFO("[user_control] user_login~n"),
	case db_control:select_user_info_by_username(UserName) of
		{user_find, {UserName, PassWord}}	->
			{user_login_success, UserName, PassWord};
		{user_find, {UserName, _}}			-> {user_password_invalid, UserName, PassWord};
		{user_empty}						->
			db_control:insert_user_info(UserName, PassWord),
			{user_login_success, UserName, PassWord}
	end.

%修改用户密码
user_modify(UserName, PassWord) ->
	%调用ets_controll模块查询用户是否存在，存在则修改密码
	?LOGINFO("[user_control] user_modify UserName:~p, PassWord:~p ~n", [UserName, PassWord]),
	case db_control:select_user_info_by_username(UserName) of
		{user_find, {UserName, _}}	->
			db_control:update_user_passwd(UserName, PassWord),
			{user_modify_success, UserName, PassWord};
		{empty}						-> 
			{user_invalid, UserName, PassWord}
	end.