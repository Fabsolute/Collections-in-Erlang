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
-export([new/0, is_stack/1, is_empty/1, len/1]).
%% Original style API
-export([push/2, push_r/2, pop/1, pop_r/1, peek/1, peek_r/1]).
%% Creation, inspection and conversion
-export([to_list/1, from_list/1, member/2]).
%% Erlang style API
-export([in/2, in_r/2, out/1, out_r/1]).
%% Less garbage style API
-export([get/1, get_r/1, drop/1, drop_r/1]).
%% Higher level API
-export([reverse/1, join/2, split/2, filter/2]).

-export_type([stack/0, stack/1]).
-opaque stack(Item) :: {list(Item)}.
-type stack() :: stack(_).

-spec new() -> stack().
new() -> {[]}.

-spec is_stack(Term :: term()) -> boolean().
is_stack({L}) when is_list(L) ->
  true;
is_stack(_) ->
  false.

-spec is_empty(S :: stack()) -> boolean().
is_empty({[]}) ->
  true;
is_empty({L}) when is_list(L) ->
  false;
is_empty(S) ->
  erlang:error(badarg, [S]).

-spec len(S :: stack()) -> non_neg_integer().
len({L}) when is_list(L) ->
  length(L);
len(S) ->
  erlang:error(badarg, [S]).

-spec to_list(L :: stack(Item)) -> list(Item).
to_list({L}) when is_list(L) ->
  L;
to_list(S) ->
  erlang:error(badarg, [S]).

-spec from_list(L :: list(Item)) -> stack(Item).
from_list(L) when is_list(L) ->
  L;
from_list(L) ->
  erlang:error(badarg, [L]).


-spec member(Item, S :: stack(Item)) -> boolean().
member(X, {L}) when is_list(L) ->
  lists:member(X, L);
member(X, S) ->
  erlang:error(badarg, [X, S]).

-spec in(Item, S1 :: stack(Item)) -> S2 :: stack(Item).
in(X, {L}) when is_list(L) ->
  {[X | L]};
in(X, S) ->
  erlang:error(badarg, [X, S]).


-spec push(Item, S1 :: stack(Item)) -> S2 :: stack(Item).
push(X, S) -> in(X, S).

-spec in_r(Item, S1 :: stack(Item)) -> S2 :: stack(Item).
in_r(X, {L}) when is_list(L) ->
  {L ++ [X]};
in_r(X, S) ->
  erlang:error(badarg, [X, S]).


-spec push_r(Item, S1 :: stack(Item)) -> S2 :: stack(Item).
push_r(X, S) -> in_r(X, S).


-spec out(S1 :: stack(Item)) -> {{value, Item}, S2 :: stack(Item)} |  {empty, S1 :: stack(Item)}.
out({[]} = S) ->
  {empty, S};
out({[V | T]}) ->
  {{value, V}, {T}};
out(S) ->
  erlang:error(badarg, [S]).

-spec pop(S1 :: stack(Item)) -> {{value, Item}, S2 :: stack(Item)} |  {empty, S1 :: stack(Item)}.
pop(S) -> out(S).

-spec out_r(S1 :: stack(Item)) -> {{value, Item}, S2 :: stack(Item)} |  {empty, S1 :: stack(Item)}.
out_r({[]} = S) ->
  {empty, S};
out_r({L}) when is_list(L) -> [V | T] = lists:reverse(L),
  {{value, V}, {lists:reverse(T)}};
out_r(S) ->
  erlang:error(badarg, [S]).


-spec pop_r(S1 :: stack(Item)) -> {{value, Item}, S2 :: stack(Item)} |  {empty, S1 :: stack(Item)}.
pop_r(S) -> out_r(S).


-spec get(S :: stack(Item)) -> Item.
get(S) ->
  case peek(S) of
    empty -> erlang:error(empty, [S]);
    {value, V} -> V;
    Error -> Error
  end.

-spec get_r(S :: stack(Item)) -> Item.
get_r(S) ->
  case peek_r(S) of
    empty -> erlang:error(empty, [S]);
    {value, V} -> V;
    Error -> Error
  end.

-spec peek(S :: stack(Item)) -> empty | {value, Item}.
peek({[]}) ->
  empty;
peek({[V | _T]}) ->
  {value, V};
peek(S) ->
  erlang:error(badarg, [S]).

-spec peek_r(S :: stack(Item)) -> empty | {value, Item}.
peek_r({[]}) ->
  empty;
peek_r({L}) when is_list(L) ->
  [V | _T] = lists:reverse(L),
  {value, V};
peek_r(S) ->
  erlang:error(badarg, [S]).


-spec drop(S1 :: stack(Item)) -> S2 :: stack(Item).
drop({[]} = S) ->
  erlang:error(empty, [S]);
drop({[_V, T]}) ->
  T;
drop(S) ->
  erlang:error(badarg, [S]).


-spec drop_r(S1 :: stack(Item)) -> S2 :: stack(Item).
drop_r({[]} = S) ->
  erlang:error(empty, [S]);
drop_r({L}) when is_list(L) ->
  {_V, T} = lists:reverse(L),
  lists:reverse(T);
drop_r(S) ->
  erlang:error(badarg, [S]).

-spec reverse(S1 :: stack(Item)) -> S2 :: stack(Item).
reverse({L}) when is_list(L) ->
  {lists:reverse(L)};
reverse(S) ->
  erlang:error(badarg, [S]).

-spec join(S1 :: stack(Item), S2 :: stack(Item)) -> S3 :: stack(Item).
join({L}, {L2}) when is_list(L), is_list(L2) -> {L ++ L2};
join(S1, S2) ->
  erlang:error(badarg, [S1, S2]).

-spec split(N :: non_neg_integer(), S1 :: stack(Item)) ->
  {S2 :: stack(Item), S3 :: stack(Item)}.
split(N, {L}) when is_integer(N), N >= 1, is_list(L) ->
  {L1, L2} = lists:split(N, L),
  {{L1}, {L2}};
split(N, S) ->
  erlang:error(badarg, [N, S]).

-spec filter(Fun, S1 :: stack(Item)) -> S2 :: stack(Item) when Fun :: fun((Item) -> boolean() | list(Item)).
filter(Fun, {L}) when is_function(Fun, 1), is_list(L) ->
  filter_private(Fun, L);
filter(Fun, S) ->
  erlang:error(badarg, [Fun, S]).

filter_private(_, []) ->
  [];
filter_private(Fun, [X | R0]) ->
  R = filter_private(Fun, R0),
  case Fun(X) of
    true ->
      [X | R];
    false ->
      R;
    L when is_list(L) ->
      lists:reverse(L, R)
  end.
