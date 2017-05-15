%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 五月 2017 9:12
%%%-------------------------------------------------------------------
-module(server_login_pool).
-author("zhuchaodi").

-behaviour(gen_server).
-include("mlogs.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {userlist = [], serverindex}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerIndex) ->
	gen_server:start_link(?MODULE, [ServerIndex], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerIndex]) ->
	ets_control:add_new_server_login_mapper(ServerIndex,self()),
	{ok, #state{serverindex = ServerIndex}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({SocketPid, Socket, {login, UserName, PassWord}}, State) ->
	?TRACE("recv login info UserName:[~p] PassWord:[~p]~n", [UserName, PassWord]),
	NewState = handle_msg_login(UserName, PassWord, Socket, SocketPid, State),
	{noreply, NewState}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_msg_login(UserName, PassWord, Socket, From, #state{serverindex = ServerIndex} = State) ->
	?LOGINFO("[server_userlist]UserName:~p Passwd:~p try to login! ~n", [UserName, PassWord]),
	NewState =
		case user_control:user_login_module(UserName, PassWord) of
			user_login_success ->
				?LOGINFO("[server_userlist][~p][~p] user_login_success~n", [ServerIndex, UserName]),
				gen_server:cast(From, {login_success, UserName}),
				ServerUserlistPid = ets_control:get_server_userlist_by_index(ServerIndex),
				gen_server:cast(ServerUserlistPid, {login_success, From, Socket, UserName}),
				handle_msg_online(UserName, Socket, From, State);
			user_password_invalid ->
				?LOGINFO("[server_userlist] user_password_invalid~n"),
				handle_msg_passwd_invalid(Socket, From, State)
		end,
	NewState.

handle_msg_online(UserName, Socket, From, #state{userlist = UserList} = State) ->
	NewState =
		case find_username_by_user(UserList, UserName) of
			{ok, {Pid, OldSocket, UserName}} ->
				server_send_user_msg(OldSocket, "server has kick you!"),
				OnlineList = remove_socket(UserList, OldSocket, UserName),
				ipc_control:kick_user(Pid, OldSocket),

				?LOGINFO("[server_userlist] [~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | OnlineList]};
			_ ->
				?LOGINFO("[server_userlist] [~p] login success! ~n", [UserName]),
				server_send_user_msg(Socket, UserName ++ " login success!"),
				send_online_info(UserList, UserName),
				State#state{userlist = [{From, Socket, UserName} | State#state.userlist]}
		end,
	NewState.


handle_msg_passwd_invalid(Socket, From, State) ->
	server_send_user_msg(Socket, "user_password_invalid!"),
	ipc_control:user_password_invalid_to_socket(From, Socket),
	State.

%通过username查询用户是否存在
find_username_by_user(List, UserName) ->
	Node = [{Pid, Socket, UserName} || {Pid, Socket, MUser} <- List, MUser =:= UserName],
	case length(Node) of
		0 -> {find_user_invalid};
		1  ->
			[Ret] = Node,
			{ok,Ret}
	end.

%发送给指定用户
server_send_user_msg(Socket, Data) ->
	OutData = "[server ack]:" ++ Data,
	gen_tcp:send(Socket, term_to_binary(OutData)).


%移除下线的Socket
remove_socket(List, Socket, UserName) ->
	OutData = "[" ++ UserName ++ "]offline!",
	?LOGINFO("[server_userlist] Send info: ~p~n", [OutData]),
	Filter = [{Pid, X, Node} || {Pid, X, Node} <- List, X /= Socket],
	[gen_tcp:send(DestSocket, term_to_binary(OutData)) || {_, DestSocket, _} <- Filter],
	Filter.

%广播在线消息
send_online_info(List, UserName) ->
	OutData = "[" ++ UserName ++ "]online!",
	[gen_tcp:send(Socket, term_to_binary(OutData))||{_, Socket,_} <- List].
