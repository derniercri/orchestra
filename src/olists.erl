-module(olists).
-export([droplast/1, foreach_except/2, foreach_except2/2, foldl_except2/3, ordered_insert/3, ordered_insert_unique/3]).

%% @doc Drops the last element of a List. The list should be non-empty, otherwise the function will crash with a function_clause
-spec droplast(List :: [T]) -> [T].
droplast([]) ->
    error(function_clause);
droplast(L) ->
    [_ | Result] = lists:reverse(L),
    lists:reverse(Result).

%% @doc Calls Fun(Elem) for each element Elem in List except the last. This function is used for its side effects and the evaluation order is defined to be the same as the order of the elements in the list.
%%      The list should be non-empty, otherwise the function will crash with a function_clause
-spec foreach_except(Fun :: fun((Elem :: T) -> term()), List :: [T]) -> ok.
foreach_except(_, []) ->
    error(function_clause);
foreach_except(_, [_]) ->
    ok;
foreach_except(Fun, [H | T]) ->
    Fun(H),
    foreach_except(Fun, T).

%% @doc Calls Fun(Elem) for each element Elem in List except the two lasts elements. This function is used for its side effects and the evaluation order is defined to be the same as the order of the elements in the list.
%%      The list should be non-empty, otherwise the function will crash with a function_clause
-spec foreach_except2(Fun :: fun((Elem :: T) -> term()), List :: [T]) -> ok.
foreach_except2(_, []) ->
    error(function_clause);
foreach_except2(_, [_]) ->
    error(function_clause);
foreach_except2(_, [_, _]) ->
    ok;
foreach_except2(Fun, [H | T]) ->
    Fun(H),
    foreach_except(Fun, T).

%% @doc Like lists:foldl but don't apply the function on the two last element of the list
-spec foldl_except2(Fun :: fun((Elem :: T, AccT) -> AccT), Acc0 :: AccT, List :: [T]) -> AccT.
foldl_except2(_, _, []) -> 
    error(function_clause);
foldl_except2(_, _, [_]) -> 
    error(function_clause);
foldl_except2(_, Acc, [_, _]) -> 
    Acc;
foldl_except2(Fun, Acc0, [H | T]) -> 
    Acc = Fun(H, Acc0),
    foldl_except2(Fun, Acc, T).

%% @doc insert a new Element in an ordered list using Function to compare element.
%%      <br/> Function return 1 if A > B , -1 if A < B and 0 i A = B
-spec ordered_insert(Element :: E, list(E), Function :: fun((A :: E, B :: E) -> integer()) ) -> list(E).

ordered_insert(E, Fun, L) ->
    ordered_insert(E, Fun, L, []).
ordered_insert(E, Fun,[], Acc) ->
    lists:reverse([E | Acc]);
ordered_insert(E, Fun,[H | T], Acc) ->
    case Fun(E, H) of
	1 ->
	    ordered_insert(E, Fun, T, [H | Acc]);
	_ -> 
	    lists:reverse(Acc) ++ [E ,H | T]
    end.				       

%% @doc insert a new Element in an ordered list using Function to compare element. if the list already contains the Element, the function raise element_already_present
%%      <br/> Function return 1 if A > B , -1 if A < B and 0 i A = B
-spec ordered_insert_unique(Element :: E, list(E), Function :: fun((A :: E, B :: E) -> integer()) ) -> list(E).

ordered_insert_unique(E, Fun, L) ->
    ordered_insert_unique(E, Fun, L, []).
ordered_insert_unique(E, Fun,[], Acc) ->
    lists:reverse([E | Acc]);
ordered_insert_unique(E, Fun,[H | T], Acc) ->
    case Fun(E, H) of
	1 ->
	    ordered_insert_unique(E, Fun, T, [H | Acc]);
	0 -> error(element_already_present);
	_ -> 
	    lists:reverse(Acc) ++ [E ,H | T]
    end.				       
