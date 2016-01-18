-module(ogb_trees_test).
-include_lib("eunit/include/eunit.hrl").

foldl_empty_test() ->
    ?assertEqual([1],ogb_trees:foldl(fun(_, X, _) -> X end, [1], gb_trees:empty())).


foldl_addition_test() ->
    ?assertEqual([5,4,3,2], ogb_trees:foldl(
			      fun(_, X, Acc) -> [(X + 1)|Acc] end,
			      [],
			      gb_trees:from_orddict([{0,1}, {1,2}, {2,3}, {3,4}])
			     )).

foldl_sum_test() ->
    ?assertEqual(10, ogb_trees:foldl(
		       fun(_, X, Acc) -> (Acc + X) end,
		       0,
		       gb_trees:from_orddict([{0,1}, {1,2}, {2,3}, {3,4}])
		      )).
foldl_max_test() ->
    ?assertEqual({3, 4}, ogb_trees:foldl(
			   fun(K, X, {K2, X2}) -> 
				   if 
				       X2 >= X -> {K2, X2};
				       true -> {K, X}
				   end
			   end,
			   {0,0}, 
			   gb_trees:from_orddict([{0,1}, {1,2}, {3,4}, {2,3}])
			  )).
