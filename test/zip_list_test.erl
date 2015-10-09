-module(zip_list_test).
-include_lib("eunit/include/eunit.hrl").

next_empty_test() ->
    L = zip_list:init(),
    ?assertException(error, badarg, zip_list:next(L)).


previous_empty_test() ->
    L = zip_list:init(),
    ?assertException(error, badarg, zip_list:previous(L)).

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

previous_test() ->
    L = zip_list:next(
	  zip_list:next(
	    zip_list:next(
	      zip_list:of_list([1, 2, 3, 4])
	     ))),
    
    F = fun (Zip_list, Val) ->
		?assertEqual(zip_list:get_current_element(Zip_list),
			     Val),
		zip_list:previous(Zip_list)
	end,
    L2 = F(L, 4),
    L3 = F(L2, 3),
    _ = F(L3, 2).
    
	
insert_before_test() ->
    L = zip_list:of_list([1,2]),
    L2 = zip_list:previous(
	   zip_list:insert_before(3,L)),
    ?assertEqual(zip_list:get_current_element(L2), 3).

insert_after_test() ->
    L = zip_list:of_list([1,2]),
    L2 = zip_list:next(
	   zip_list:insert_after(3,L)),
    ?assertEqual(zip_list:get_current_element(L2), 3).
    
