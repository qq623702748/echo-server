%% @author zhuchaodi
%% @doc @todo Add description to server_listener.


-module(server_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_login_pool/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, start_accept_pool/2, start_userlist_pool/1]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {lsock}).

%% ====================================================================
init([Port, AcceptPool, ServerUserListPool, ServerLoginPool]) ->
	io:format("server_listener init start..~n"),
	process_flag(trap_exit, true),
	Opts = [binary, {packet, 4}, {reuseaddr, true},
		 {backlog, 30}, {active, false}],
	State =
		case gen_tcp:listen(Port, Opts) of
			{ok, LSock} ->
				#state{lsock = LSock};
			_Other ->
				throw({error, {could_not_listen_on_port, Port}}),
				#state{}
		end,
	io:format("========server_listener ready to start accept pool~n"),
 	start_accept_pool(State#state.lsock, AcceptPool),
	io:format("========server_listener ready to start userlist pool~n"),
	start_userlist_pool(ServerUserListPool),
	io:format("========server_listener ready to start login pool~n"),
	start_login_pool(ServerUserListPool),
	{ok, State}.

%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.

%% ====================================================================
terminate(_Reason, _State) ->
    ok.

%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
start_link(Port, AcceptPool, ServerUserListPool, ServerLoginPoolCnt)->
	io:format("server_listener start_link~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptPool, ServerUserListPool, ServerLoginPoolCnt],[]).

start_accept_pool(LSock, AcceptCnt) when AcceptCnt > 0 ->
	server_accept_pool_sup:start_child(LSock, AcceptCnt),
	start_accept_pool(LSock, AcceptCnt - 1);
start_accept_pool(_LSock, AcceptCnt) when AcceptCnt =:= 0 ->
	[].

start_userlist_pool(ServerUserListPoolCnt) when ServerUserListPoolCnt > 0 ->
	server_userlist_sup:start_child(ServerUserListPoolCnt),
	start_userlist_pool(ServerUserListPoolCnt - 1);
start_userlist_pool(ServerUserListPoolCnt) when ServerUserListPoolCnt =:= 0 ->
	[].

start_login_pool(ServerLoginPoolCnt) when ServerLoginPoolCnt > 0 ->
	server_login_pool_sup:start_child(ServerLoginPoolCnt),
	start_login_pool(ServerLoginPoolCnt - 1);
start_login_pool(ServerLoginPoolCnt) when ServerLoginPoolCnt =:= 0 ->
	[].