%% @author zhuchaodi
%% @doc @todo Add description to server_super_sup.


-module(server_super_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link(Port, AcceptPool, ServerUserListPool) ->
	io:format("server_super_sup start link~n"),
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, AcceptPool,ServerUserListPool]).
%% ====================================================================
init([Port, AcceptPool, ServerUserListPool]) ->
	io:format("server_super_sup init pid:~p ~n", [self()]),
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
				{ server_userlist_sup,
					{supervisor, start_link, [{local, server_userlist_sup}, server_userlist_sup, []]},
					permanent,
					2000,
					supervisor,
					[server_userlist_sup]
				},
				% server_listener
				{ server_listener,
					{server_listener, start_link, [Port, AcceptPool, ServerUserListPool]},
					permanent,
					2000,
					worker,
					[server_listener]
				},
				% msg_queue_protected
				{ msg_queue_protected,
					{msg_queue_protected, start_link, []},
					permanent,
					2000,
					worker,
					[msg_queue_protected]
				}

			]
		}
	}.

%% ====================================================================
%% Internal functions
%% ====================================================================

