%% @author zhuchaodi
%% @doc @todo Add description to server_accept_pool_sup.


-module(server_accept_pool_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_child/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
init([]) ->
	process_flag(trap_exit,true),
	monitor(process, server_accept_pool_sup),
	io:format("server_accept_pool_sup init ~n"),
    {ok,
		{ {simple_one_for_one, 1, 1},
			[
				{ server_accept_pool,
					{server_accept_pool, start_link, []},
					permanent,
					brutal_kill,
					worker,
					[server_accept_pool]
				}
			]
		}
	}.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_child(LSock, Index) ->
%%  	io:format("server_accept_pool_sup start_child Index:~p~n", [Index]),
	supervisor:start_child(server_accept_pool_sup, [LSock, Index]).

