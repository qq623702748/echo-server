%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 五月 2017 16:50
%%%-------------------------------------------------------------------
-module(server_chat_cache).
-author("zhuchaodi").
-include("mlogs.hrl").
%% API
%-export([cache_init/1, add_cache/1, del_cache/3, get_cache_continuous_sequence/0]).
-compile(export_all).
cache_init(StartIndex) ->
	%?TRACE("DICT init [~p]", [StartIndex]),
	put(cache, [StartIndex]).

add_cache(NewIndex) ->
	case get(cache) of
		undefined ->
			io:format("cache error! try to re init~n"),
			put(cache, [NewIndex]);
		Ret ->
			put(cache, [NewIndex|Ret])
	end.
del_cache(ServerIndex, StartIndex, EndIndex) when EndIndex > StartIndex->
	Sequence = get(cache),
	lists:sort(Sequence),
	Ret = [X||X<-Sequence, X>=EndIndex],
	?TRACE("[~p] New cache Status: ~p~n", [ServerIndex, Ret]),
	put(cache, Ret);
del_cache(ServerIndex, _, _) -> ok.
%%获取最大的连续序列
get_cache_continuous_sequence(ServerIndex) ->
	Sequence = get(cache),
	MinIndex = lists:min(Sequence),
	case Sequence of
		[_,_|_] ->
			SortSeq = lists:sort(Sequence),
			?TRACE("[~p] SortSeq:[~p]~nMinIndex:[~p]~n", [ServerIndex, SortSeq,MinIndex]),
			MaxIndex = get_max_continuous(SortSeq,MinIndex),
			{MinIndex, MaxIndex};
		[_|_] ->
			{MinIndex,MinIndex}
	end.

get_max_continuous([Head,Next|Tail],Ret) when Ret =< Head->
	if
		Next == Head + 1 ->
			get_max_continuous([Next|Tail], Next);
		true ->
			Head
	end;

get_max_continuous(_, Ret) ->
	Ret.