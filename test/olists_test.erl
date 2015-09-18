-module(old_list_test).
-include_lib("eunit/include/eunit.hrl").

error_droplast_test() ->
    ?assertError(function_clause, old_lists:droplast([]) ).

droplast_test() ->
    ?assertEqual([1,2,3], old_lists:droplast([1,2,3,4]) ),
    ?assertEqual([], old_lists:droplast([1]) ).
