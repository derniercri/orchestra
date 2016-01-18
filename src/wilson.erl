%% copyright 2015 Derniercri
%% version 1.0.0
%% @title `wilson` stock management

-module(wilson).
-vsn(1). 
-author(["Xavier Van de Woestyne"]).

%% Export rules 
-export([c/4, t/3, q/3]).


c(T, Cs, Cl, K) -> Cs * ((K * T)/2) + (Cl / T). 
t(Cl, Cs, K) -> math:sqrt( (2*Cl) / (Cs * K) ).
q(Cl, Cs, K) -> math:sqrt( ((2*Cl) * K) / Cs ).
