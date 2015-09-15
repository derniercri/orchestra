%% copyright 2015 Derniercri
%% version 1.0.0
%% @title `parallel` provide parallel combinators 

-module(parallel).
-vsn(1). 
-author(["Xavier Van de Woestyne"]).


%% Export rules 
-export([
         list_map/2,
         mapreduce/4
        ]).

%% Parallel combinators for list)
process(Supervisor, Reference, Function, Elt) ->
    Supervisor ! {Reference, (catch Function(Elt))}.
join(0, _, List) -> List;
join(Id, Reference, List) -> 
    receive
        {Reference, Result} -> 
            join(Id-1, Reference, [Result|List])
    end.

%% @doc parallel implementation of Map
list_map(F, List) ->
    Self = self(),
    Reference = erlang:make_ref(),
    G = fun(I) -> spawn(fun() -> process(Self, Reference, F, I) end) end,
    lists:foreach(G, List),
    join(length(List), Reference, []).


%% Map reduce custom implementation using a dictionaries

exec_job(ReducerPid, F, Elt) -> F(ReducerPid, Elt).

reduce(Supervisor, F1, F2, AccA, List) ->
    process_flag(trap_exit, true), %% performance no jutsu
    ReducerPid = self(),
    F = fun(X) -> spawn_link(fun() -> exec_job(ReducerPid, F1, X) end) end,
    lists:foreach(F, List),
    DictStateA = dict:new(),
    DictStateB = merge_keyval(length(List), DictStateA),
    Acc = dict:fold(F2, AccA, DictStateB),
    Supervisor ! {self(), Acc}.

merge_keyval(0, Dict) -> Dict;
merge_keyval(Elt, Dict) -> 
    receive 
        {Key, Value} ->
            case dict:is_key(Key, Dict) of 
                true ->
                    NewDict = dict:append(Key, Value, Dict),
                    merge_keyval(Elt, NewDict);
                false ->
                    NewDict = dict:store(Key, [Value], Dict),
                    merge_keyval(Elt, NewDict)
            end;
        {'EXIT', _, _} -> merge_keyval(Elt-1, Dict)
    end.

mapreduce(F1, F2, Acc, List) ->
    Self = self(),
    Pid = spawn(fun() -> reduce(Self, F1, F2, Acc, List) end),
    receive
        {Pid, Result} -> Result
    end.
