-module(ogb_trees).
-export([foldl/3]).

-spec foldl(Fun :: fun((Key :: K, Value :: V, AccT) -> AccT), Acc0 :: AccT, Tree :: gb_trees:tree(K,V)) -> AccT.
foldl(Fun, Acc, Tree) ->
    Iterator = gb_trees:iterator(Tree),
    foldl_aux(Fun, Acc, Iterator).

foldl_aux(Fun, Acc, Iter) -> 
    case gb_trees:next(Iter) of
	none -> Acc;
        {Key, Value, Iter2} ->
	    io:format("1~n"),
	    Acc2 = Fun(Key, Value, Acc),
	    io:format("2~n"),
	    foldl_aux(Fun, Acc2, Iter2)
    end.
