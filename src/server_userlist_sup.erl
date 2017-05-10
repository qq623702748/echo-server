%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 五月 2017 10:22
%%%-------------------------------------------------------------------
-module(server_userlist_sup).
-author("zhuchaodi").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("common.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	io:format("server_userlist_sup init ~n"),
	db_control:init(),
	LastChatIndex = db_control:get_chat_record_size(),
	%创建服务器映射表
	ets_control:create_server_userlist_mapper(),
	ets_control:create_server_world_chat_blackboard(LastChatIndex),
	ets_control:create_server_group_chat_blackboard(0),
	ets_control:create_server_private_chat_blackboard(0),
	ets_control:create_server_msg_queue_blackboard(LastChatIndex),
	{ok,
		{ {simple_one_for_one, 1, 1},
			[
				{ server_userlist,
					{server_userlist, start_link, []},
					permanent,
					brutal_kill,
					worker,
					[server_userlist]
				}
			]
		}
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_child(ServerIndex) ->
	supervisor:start_child(server_userlist_sup, [ServerIndex]).