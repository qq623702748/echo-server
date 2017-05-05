%% @author zhuchaodi
%% @doc @todo Add description to server_super_sup.


-module(server_super_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link(Port, AcceptPool) ->
	io:format("server_super_sup start link~n"),
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, AcceptPool]).
%% ====================================================================
init([Port, AcceptPool]) ->
	io:format("server_super_sup init ~n"),
    {ok,
		{ {one_for_one, 5, 60},
			[
			 	% server control
				{ server_control,
					{server_control, start_link, []},
					permanent,
					2000,
					worker,
					[server_control]
				},
				% client supervisor
				{ server_accept_pool_sup,
					{supervisor, start_link, [{local, server_accept_pool_sup}, server_accept_pool_sup, []]},
					permanent,
					2000,
					supervisor,
					[server_accept_pool_sup]
				},
				% server_socket_sup
				{ server_socket_sup,
					{supervisor, start_link, [{local, server_socket_sup}, server_socket_sup, []]},
					permanent,
					2000,
					supervisor,
					[server_socket_sup]
				},

				% online userinfo
				{ server_userlist,
					{server_userlist, start_link, []},
					permanent,
					2000,
					worker,
					[server_userlist]
				},
				% server_listener
				{ server_listener,
					{server_listener, start_link, [Port, AcceptPool]},
					permanent,
					2000,
					worker,
					[server_listener]
				}
			]
		}
	}.

%% ====================================================================
%% Internal functions
%% ====================================================================

