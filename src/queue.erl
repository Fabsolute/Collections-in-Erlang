%%%-------------------------------------------------------------------
%%% @author fabsolutely
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2017 21:53
%%%-------------------------------------------------------------------
-module(queue).
-author("fabsolutely").

%% API
-export([new/0, count/1, enqueue/2, dequeue/1, peek/1]).

new() -> {queue, 0, []}.

count({Count, _Queue}) -> {ok, Count};
count(_) -> {error, its_not_queue}.

enqueue(Element, {queue, Count, Queue}) -> {ok, {queue, Count + 1, Queue ++ [Element]}};
enqueue(_Element, _Queue) -> {error, its_not_queue}.

dequeue({queue, Count, [Head | Tail]}) -> {ok, Head, {queue, Count - 1, Tail}};
dequeue({queue, _Count, []}) -> {error, has_no_value};
dequeue(_) -> {error, its_not_queue}.

peek({queue, _Count, [Head | _Tail]}) -> {ok, Head};
peek({queue, _Count, []}) -> {error, has_no_value};
peek(_) -> {error, its_not_queue}.
