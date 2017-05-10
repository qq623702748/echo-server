%%%-------------------------------------------------------------------
%%% @author zhuchaodi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2017 14:50
%%%-------------------------------------------------------------------
-author("zhuchaodi").


-define(LOGINFO(Format), ok).
-define(LOGINFO(Format, Args), ok).

%-define(LOGINFO(Format), io:format("[Log info]" ++ Format)).
%-define(LOGINFO(Format, Args), io:format("[Log info]" ++ Format, Args)).

-define(TRACE(Format), io:format("[Log info]" ++ Format)).
-define(TRACE(Format, Args), io:format("[Log info]" ++ Format, Args)).
