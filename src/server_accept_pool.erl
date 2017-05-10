%% @author zhuchaodi
%% @doc @todo Add description to server_accept_pool.


-module(server_accept_pool).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(Timeout, 120*1000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2, start_socket_child/1]).

-include("mlogs.hrl").

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {lsock, index, socket}).
%% ====================================================================
init([LSock, Index]) ->
 	?LOGINFO("[server_accept_pool][~p] start...~n", [Index]),
	gen_server:cast(self(), tcp_accept),
	inet:setopts(LSock, [{active, false}]),
    {ok, #state{lsock = LSock, index = Index}}.


%% handle_call/3
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2

handle_cast(tcp_accept, #state{lsock = LSock} = State) ->
	{ok, AcceptSock} = gen_tcp:accept(LSock),
	?LOGINFO("[server_accept_pool] handle_cast accept new client ~p~n",[AcceptSock]),
	%{ok, {IP, _Port}} = inet:peername(AcceptSock),

	{ok, Pid} = start_socket_child(AcceptSock),
	gen_tcp:controlling_process(AcceptSock, Pid),
	gen_server:cast(self(), tcp_accept),
	{noreply, State#state{socket = AcceptSock}, ?Timeout};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Info, State) ->
	?LOGINFO("[server_accept_pool] handle info~n"),
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(Reason, _State) ->
	?TRACE("[server_accept_pool] terminate, Reason:[~p]~n", [Reason]),
    ok.


%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(LSock, Index) ->
%% 	?LOGINFO("[server_accept_pool] server_accept_pool start_link Index:~p~n", [Index]),
	gen_server:start_link(?MODULE, [LSock, Index], []).

start_socket_child(Socket) ->
	server_socket_sup:start_child(Socket).