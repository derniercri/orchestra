-module(zip_list).
-export([init/0, of_list/1, of_list/2, get_current_element/1, next/1, previous/1, insert_after/2, insert_before/2, foreach/2, to_list/1]).
-record(zip_list, {before_list, before_size, current, after_size, after_list}).

%% --------------------------------------------
%% Type declaration
%% --------------------------------------------



%% @type zip_list(Element) :: {zip_list, Before_list :: [Element], Before_size :: integer(), Current_element :: Element, After_size :: integer(), After_list :: [Element]}.
%%       tuple which represent a zipper on a list
%%       Values
%%       <ul><li>Before_list : elements already browsed </li>
%%       <li>Before_size : size of Before_list  </li>
%%       <li>Current_element : current element point by the zipper</li>
%%       <li>After_size : size of the After_list</li>
%%       <li>After_list : tail of the list</li></ul>
-type zip_list(Element) :: {zip_list, Before_list :: [Element], Before_size :: integer(), Current_element :: Element, After_size :: integer(), After_list :: [Element]}.

%% --------------------------------------------
%% Init function
%% --------------------------------------------


%% @doc make an empty zip_list
-spec init() -> zip_list(any()).
init() ->
    {zip_list, [], 0, null, 0, []}.

%% @doc create a zip_list from an erlang list of a knowed size
%%      Warning : the size given is not checked
-spec of_list(List :: list(Element), integer()) -> zip_list(Element).
of_list([], _) ->
    init();
of_list([H|T], Size) ->
    {zip_list, [], 0, H, Size - 1, T}.

%% @doc create a zip_list from an erlang list
-spec of_list(List :: list(Element)) -> zip_list(Element).
of_list(List) ->
    of_list(List, length(List)).

%% @doc convert a zip_list to an erlang list
-spec to_list(Zip_list :: zip_list(E)) -> list(E).

to_list(Zip_list) ->
    L1 = Zip_list#zip_list.before_list,
    case Zip_list#zip_list.current of
	null -> lists:reverse(L1);
	C ->
	    L2 = Zip_list#zip_list.after_list,
	    lists:reverse(L1) ++ [C | L2]
    end.
    

%% --------------------------------------------
%% Getter function
%% --------------------------------------------

%% @doc get the curent element point by the zipper
-spec get_current_element(Zip_list :: zip_list(Element)) -> Element.
get_current_element(Zip_list) ->
    Zip_list#zip_list.current.

%% --------------------------------------------
%% Move function
%% --------------------------------------------

%% @doc move of one element in the list
%%      if the list is empty, raise a empty_list error
-spec next(Zip_list :: zip_list(Element)) -> zip_list(Element).
next(Zip_list) ->
    {Current, After_list} = 
	case Zip_list#zip_list.after_list of
	    [] -> case Zip_list#zip_list.current of
		      null -> error(empty_list);
		      _ -> {null, []}
		  end;
	    [H | T] -> {H, T}
	end,
    Before_list = [Zip_list#zip_list.current | Zip_list#zip_list.before_list],
    {zip_list,
     Before_list, 
     Zip_list#zip_list.before_size + 1, 
     Current,
     Zip_list#zip_list.after_size - 1, 
     After_list
    }.
    
%% @doc move back of one element in the list
%%      if the list is empty, raise a begin_list error
-spec previous(Zip_list :: zip_list(Element)) -> zip_list(Element).
previous(Zip_list) ->
    {Current, Before_list} = 
	case Zip_list#zip_list.before_list of
	    [] -> error(begin_list);
	    [H | T] -> {H, T}
	end,
    After_list = [Zip_list#zip_list.current | Zip_list#zip_list.after_list],
    {zip_list,
     Before_list, 
     Zip_list#zip_list.before_size - 1,
     Current,
     Zip_list#zip_list.after_size + 1,
     After_list
    }.



%% --------------------------------------------
%% Insertion function
%% --------------------------------------------

%% @doc insert a new Element before the current.
-spec insert_before(Element :: E, Zip_list :: zip_list(E)) -> zip_list(E).

insert_before(Element, Zip_list) ->
    Before_list = [Element | 
		   Zip_list#zip_list.before_list],
    inc_before_size(
      set_before_list(Zip_list, Before_list)).

%% @doc insert a new Element after the current.
-spec insert_after(Element :: E, Zip_list :: zip_list(E)) -> zip_list(E).

insert_after(Element, Zip_list) ->
    After_list = [Element | 
		   Zip_list#zip_list.after_list],
    inc_after_size(
      set_after_list(Zip_list, After_list)).

%% @doc insert a new Element in an ordered zip_list using Function to compare element.
%%      <br/> Function return 1 if A > B , -1 if A < B and 0 i A = B
%% -spec ordered_insert(Element :: E, Zip_list :: zip_list(E), Function :: fun((A :: Element, B :: Element) -> integer())) -> zip_list(E)


%% ordered_insert(E, Z_l, Fun) ->
    
%% -----------------
%% Fold and Foreach
%% -----------------

-spec foreach(Function :: fun((Element :: E) -> ok), Zip_list :: zip_list(E)) -> ok.
foreach(Fun, Zip_list) ->
    case get_current_element(Zip_list) of
	null -> ok;
	E -> 
	    ok = Fun(E),
	    foreach(Fun, next(Zip_list))
    end.
	    
	

%% -----------------------------------
%% Setter function
%% -----------------------------------
set_before_list(Zip_list, Val) ->
    {zip_list,
     Val,
     Zip_list#zip_list.before_size, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size, 
     Zip_list#zip_list.after_list
    }.

dec_before_size(Zip_list) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size - 1, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size, 
     Zip_list#zip_list.after_list
    }.

inc_before_size(Zip_list) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size + 1, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size, 
     Zip_list#zip_list.after_list
    }.

set_current(Zip_list, Val) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size, 
     Val,
     Zip_list#zip_list.after_size, 
     Zip_list#zip_list.after_list
    }.

dec_after_size(Zip_list) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size - 1, 
     Zip_list#zip_list.after_list
    }.

inc_after_size(Zip_list) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size + 1, 
     Zip_list#zip_list.after_list
    }.

set_after_list(Zip_list, Val) ->
    {zip_list,
     Zip_list#zip_list.before_list,
     Zip_list#zip_list.before_size, 
     Zip_list#zip_list.current,
     Zip_list#zip_list.after_size, 
     Val
    }.


