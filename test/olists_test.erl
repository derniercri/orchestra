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
