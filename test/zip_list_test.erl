-module(zip_list_test).
-include_lib("eunit/include/eunit.hrl").

next_empty_test() ->
    L = zip_list:init(),
    ?assertException(error, badarg, zip_list:next(L)).

next_one_test() ->
    L = zip_list:of_list([1]),
    ?assertException(error, badarg, zip_list:next(
				      zip_list:next(L))).
next_test() ->
    L = zip_list:of_list([1, 2, 3, 4]),
    F = fun (Zip_list, Val) ->
		?assertEqual(zip_list:get_current_element(Zip_list),
			     Val),
		zip_list:next(Zip_list)
	end,
    L2 = F(L, 1),
    L3 = F(L2, 2),
    _ = F(L3, 3).
    
	
