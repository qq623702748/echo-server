%% @author zhuchaodi
%% @doc @todo Add description to server_socket_sup.


-module(server_socket_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_child/1]).
-include("mlogs.hrl").


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
init([]) ->
	?LOGINFO("[server_socket_sup] init ...~n"),
   {ok,
		{ {simple_one_for_one, 0, 1},
			[
				{ server_socket,
					{server_socket, start_link, []},
					temporary,
					brutal_kill,
					worker,
					[server_socket]
				}
			]
		}
	}.


%% ====================================================================
%% Internal functions
%% ====================================================================
start_child(Socket) ->
 	?LOGINFO("[server_socket_sup] start_child ~n"),
	supervisor:start_child(server_socket_sup, [Socket]).
