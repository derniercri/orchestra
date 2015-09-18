-module(old_lists).
-export([droplast/1]).

%% @doc Drops the last element of a List. The list should be non-empty, otherwise the function will crash with a function_clause
-spec droplast(List :: [T]) -> [T].
droplast([]) ->
    error(function_clause);
droplast(L) ->
    [_ | Result] = lists:reverse(L),
    lists:reverse(Result).
