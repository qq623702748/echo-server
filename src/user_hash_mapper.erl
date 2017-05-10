%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 五月 2017 13:45
%%%-------------------------------------------------------------------
-module(user_hash_mapper).
-author("zhuchaodi").
-include("common.hrl").
%% API
-export([hash/1]).

hash(UserName) ->
	hash_calc(UserName, 0).

hash_calc([Head|Tail], Ret) ->
	hash_calc(Tail, (Ret+Head)*?HASHFACTOR);
hash_calc([], Ret) ->
	Ret.
