-module(zip_list).
-export([init/0, of_list/1, of_list/2, get_current_element/1, next/1]).
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
    
-spec next(Zip_list :: zip_list(Element)) -> zip_list(Element).
next(Zip_list) ->
    {Current, After_list} = 
	case Zip_list#zip_list.after_list of
	    [] -> error(badarg);
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
    
