-module(olists_test).
-include_lib("eunit/include/eunit.hrl").

error_droplast_test() ->
    ?assertError(function_clause, olists:droplast([]) ).

droplast_test() ->
    ?assertEqual([1,2,3], olists:droplast([1,2,3,4]) ),
    ?assertEqual([], olists:droplast([1]) ).

fold_except2_error_empty_test() ->
    ?assertError(function_clause, olists:foldl_except2(fun(X, _) -> X end, 0, [])).

fold_except2_error_one_elem_test() ->
    ?assertError(function_clause, olists:foldl_except2(fun(X, _) -> X end, 0, [1])).

fold_except2_two_elem_test() ->
   ?assertEqual(0, olists:foldl_except2(fun(X, _) -> X end, 0, [1, 2])).

fold_except2_normal_test() ->
    Fun = fun (X, Acc) -> X + Acc end,
    ?assertEqual(10 , olists:foldl_except2(Fun, 0, [1, 2, 3, 4, 5, 6])).


cmp(A, B) ->
    if
	A > B -> 1;
	A < B -> -1;
	true -> 0
    end.
	     

ordered_insert_empty_test() ->
    L = [],
    L2 = olists:ordered_insert(2, fun cmp/2, L),
    ?assertEqual(L2, [2]).

ordered_insert_test() ->
    L = [1,3,5],
    L2 = olists:ordered_insert(2, fun cmp/2, L),
    ?assertEqual(L2, [1,2,3,5]).

ordered_insert_end_test() ->
    L = [1,3,5],
    L2 = olists:ordered_insert(6, fun cmp/2, L),
    ?assertEqual(L2, [1,3,5,6]).

ordered_insert_unique_empty_test() ->
    L = [],
    L2 = olists:ordered_insert_unique(2, fun cmp/2, L),
    ?assertEqual(L2, [2]).

ordered_insert_unique_test() ->
    L = [1,3,5],
    L2 = olists:ordered_insert_unique(2, fun cmp/2, L),
    ?assertEqual(L2, [1,2,3,5]).

ordered_insert_unique_end_test() ->
    L = [1,3,5],
    L2 = olists:ordered_insert_unique(6, fun cmp/2, L),
    ?assertEqual(L2, [1,3,5,6]).

ordered_insert_unique_exception_test() ->
    L = [1,3,5],
    ?assertException(
       error,
       element_already_present,
       olists:ordered_insert_unique(3, fun cmp/2, L)
      ).


