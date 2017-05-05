%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2017 10:05
%%%-------------------------------------------------------------------
-author("zhuchaodi").
-record(chat_record_p, {id, username, word}). %定义说的顺序id，说话的名称，说的话
-record(user_info_p, {username, password}). %定义用户表结构, 用户名，密码
-record(group_info_p, {gp_id, gp_title, gp_userlist = []}). %定义讨论组结构, 讨论组id， 讨论组名称， 讨论组的在线用户