%% @author zhuchaodi
%% @doc @todo Add description to server_app.


-module(server_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(PORT, 9527).	%服务器指定端口值
-define(ACCEPTPOOL, 2).%socket连接池大小
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_Type, _StartArgs) ->
	io:format("server_app start~n"),
    case server_super_sup:start_link(?PORT, ?ACCEPTPOOL) of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.


stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


