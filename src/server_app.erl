%% @author zhuchaodi
%% @doc @todo Add description to server_app.


-module(server_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_Type, _StartArgs) ->

	io:format("server_app start~n"),
    case server_super_sup:start_link(?PORT, ?ACCEPTPOOL, ?SERVERUSERLISTPOOL, ?SERVERLOGINPOOL) of
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

