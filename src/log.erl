%% copyright 2015 Derniercri
%% version 1.0.0
%% @title `log` minimalist log engine

-module(log).
-vsn(1). 
-author(["Xavier Van de Woestyne"]).
-include("common.hrl").

%% Export rules 
-export([out/1]).

%% Process a log in debug mode
out(Message) ->
    case (?IS_IN_DEBUG) of 
        true -> io:format("~w~n", [Message]);
        _ -> ok
    end.
