%%%-------------------------------------------------------------------
%%% @author fabsolutely
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2017 19:51
%%%-------------------------------------------------------------------
-module(stack).
-author("fabsolutely").

%% API
-export([new/0, count/1, push/2, pop/1, peek/1]).

new() -> {stack, 0, []}.

count({Count, _Stack}) -> {ok, Count};
count(_) -> {error, its_not_stack}.

push(Element, {stack, Count, Stack}) -> {ok, {stack, Count + 1, [Element] ++ Stack}};
push(_Element, _) -> {error, its_not_stack}.

pop({stack, Count, [Head | Stack]}) -> {ok, Head, {stack, Count - 1, Stack}};
pop({stack, _Count, []}) -> {error, has_no_value};
pop(_) -> {error, its_not_stack}.

peek({stack, Count, [Head | Stack]}) -> {ok, Head, {stack, Count, [Head] ++ Stack}};
peek({stack, _Count, []}) -> {error, has_no_value};
peek(_) -> {error, its_not_stack}.
