%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 五月 2017 9:10
%%%-------------------------------------------------------------------
-module(server_login_pool_sup).
-author("zhuchaodi").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	io:format("server_userlist_sup init ~n"),
	{ok,
		{ {simple_one_for_one, 1, 1},
			[
				{ server_login_pool,
					{server_login_pool, start_link, []},
					permanent,
					brutal_kill,
					worker,
					[server_login_pool]
				}
			]
		}
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_child(ServerIndex) ->
	supervisor:start_child(server_login_pool_sup, [ServerIndex]).