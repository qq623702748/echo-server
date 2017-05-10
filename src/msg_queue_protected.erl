%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2017 10:31
%%%-------------------------------------------------------------------
-module(msg_queue_protected).
-author("zhuchaodi").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-include("common.hrl").
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	io:format("msg_queue_protected start~n"),
	{ok, #state{}, ?MSG_QUEUE_TIMEOUT}.

handle_call(_Request, _From, State) ->
	{reply, ok, State, ?MSG_QUEUE_TIMEOUT}.

handle_cast(_Request, State) ->
	{noreply, State, ?MSG_QUEUE_TIMEOUT}.

handle_info(_Info, State) ->
	ets_control:clean_world_chat_msg(),
	{noreply, State, ?MSG_QUEUE_TIMEOUT}.

terminate(Reason, _State) ->
	io:format("msg_queue_protected terminate Reason:~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
