%% Define common Date struct

%% @doc primitives types 
-type mhour() :: 0 .. 23.
-type mmin() :: 0 .. 59.
-type msec() :: 0 .. 59. 
-type mday() :: 1 .. 31. 
-type mmonth() :: 1 .. 12. 
-type myear() :: integer(). 

-type metat() :: {mhour(), mmin(), msec()}. 
-type metad() :: {myear(), mmonth(), mday()}.
-type metadt() :: {metad(), metat()}.

%% Macro definition 
-define(UNIX_TIME, 62167219200).
