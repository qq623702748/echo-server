%% @author zhuchaodi
%% @doc @todo Add description to server_socket_sup.


-module(server_socket_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_child/1]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
init([]) ->
	io:format("server_socket_sup init ...~n"),
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
 	io:format("server_socket_sup start_child ~n"),
	supervisor:start_child(server_socket_sup, [Socket]).

