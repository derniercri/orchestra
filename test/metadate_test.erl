%% copyright 2015 Derniercri
%% version 1.0.0
%% @title `metadate` expressive date Manager

-module(metadate_test).
-vsn(1). 
-author(["Xavier Van de Woestyne"]).
-include_lib("eunit/include/eunit.hrl").
-include("metadate_include.hrl").

%% Sample data 
sample_date1() -> metadate:new(1999, 10, 13).
sample_date2() -> metadate:new(1999, 7, 1).
sample_date3() -> metadate:new(1963, 12, 14, 2, 22, 58).
sample_date4() -> metadate:new(1963, 12, 14, 2, 22, 59).
sample_date5() -> metadate:new(1963, 12, 14, 2, 23, 00).
sample_date0() -> metadate:new(0, 1, 1, 0, 0, 0). 
sample_date6() -> metadate:new(2016, 12, 10).
sample_date7() -> metadate:new(2016, 02, 10).
sample_date8() -> metadate:new(2016, 11, 10).
sample_date9() -> metadate:new(2015, 2, 10).
unix_date() -> metadate:new(1970, 1, 1, 0, 0, 0).

%% Test members
date_test() ->
    ?assertMatch({1999, 10, 13}, metadate:date(sample_date1())),
    ?assertMatch({1999, 7, 1}, metadate:date(sample_date2())),
    ?assertMatch({1963, 12, 14}, metadate:date(sample_date3())).

time_test() ->
    ?assertMatch({0, 0, 0}, metadate:time(sample_date1())),
    ?assertMatch({0, 0, 0}, metadate:time(sample_date0())),
    ?assertMatch({2, 22, 58}, metadate:time(sample_date3())).
    
year_test() ->    
    ?assert(0 == metadate:year(sample_date0())),
    ?assert(1963 == metadate:year(sample_date3())),
    ?assert(1963 == metadate:year(metadate:date(sample_date4()))).

month_test() ->    
    ?assert(1 == metadate:month(sample_date0())),
    ?assert(7 == metadate:month(metadate:date(sample_date2()))).

day_test() ->    
    ?assert(14 == metadate:day(sample_date5())),
    ?assert(13 == metadate:day(metadate:date(sample_date1()))).

hour_test() ->    
    ?assert(2 == metadate:hour(sample_date3())),
    ?assert(0 == metadate:day(metadate:time(sample_date0()))).

min_test() ->    
    ?assert(22 == metadate:minute(sample_date3())),
    ?assert(0 == metadate:minute(metadate:time(sample_date0()))).

sec_test() ->    
    ?assert(58 == metadate:second(sample_date3())),
    ?assert(0 == metadate:second(metadate:time(sample_date0()))).

%% coersion test 

gregorian_test() ->
    ?assert(metadate:to_int(sample_date0()) == 0),
    ?assert(metadate:to_int(unix_date()) == ?UNIX_TIME).

timestamp_test() ->
    ?assert(metadate:to_timestamp(unix_date()) == 0).

%% Date comparison

compare_to_test() ->
    F = metadate:compare_to(sample_date0(), sample_date1()),
    G = metadate:compare_to(sample_date1(), sample_date2()),
    H = metadate:compare_to(sample_date3(), sample_date4()),
    I = metadate:compare_to(sample_date4(), sample_date5()),
    K = metadate:compare_to(sample_date1(), metadate:new(1999, 10, 13)),
    ?assert(F == -1),
    ?assert(G == 1),
    ?assert(H == -1),
    ?assert(I == -1),
    ?assert(K == 0).

leap_test() ->
    ?assert(metadate:is_leap(2012)),
    ?assert(metadate:is_leap(sample_date6())),
    ?assert(metadate:is_leap(metadate:date(sample_date6()))),
    ?assertNot(metadate:is_leap(sample_date1())).

days_of_month_test() ->
    ?assert(metadate:days_of_month(sample_date1()) == 31),
    ?assert(metadate:days_of_month(sample_date7()) == 29),
    ?assert(metadate:days_of_month(sample_date8()) == 30),
    ?assert(metadate:days_of_month(sample_date9()) == 28).
