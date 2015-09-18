-module(olists_test).
-include_lib("eunit/include/eunit.hrl").

error_droplast_test() ->
    ?assertError(function_clause, olists:droplast([]) ).

droplast_test() ->
    ?assertEqual([1,2,3], olists:droplast([1,2,3,4]) ),
    ?assertEqual([], olists:droplast([1]) ).
