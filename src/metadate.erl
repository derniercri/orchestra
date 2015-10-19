%% copyright 2015 Derniercri
%% version 1.0.0
%% @title `metadate` expressive date Manager

-module(metadate).
-vsn(1). 
-author(["Xavier Van de Woestyne"]).
-include("metadate_include.hrl").

%% Export rules 
-export([
         now/0,
         new/6, new/3,
         to_int/1,
         date/1, time/1, year/1, month/1, 
         day/1, minute/1, second/1, 
         hour/1,
         to_timestamp/1,
         date_compare_to/2,
         time_compare_to/2,
         compare_to/2,
         is_leap/1,
         days_of_month/1,
         days_sequence_of/1,
         months_sequence/0,
         day_in_week/1,
	 to_string/1
        ]).

%% Date creation

%% @doc Get the current date 
-spec now() -> metadt().
now() -> calendar:local_time().

%% @doc Create a date  
-spec new(Year::myear(), Month::mmonth(), Day::mday()) -> metat().
new(Year, Month, Day) -> new(Year, Month, Day, 0, 0, 0).
new(Year, Month, Day, H, M, S) -> {{Year, Month, Day}, {H, M, S}}.

%% Members data 
%% @doc retreive the date of a meta date.
-spec date(Date::metadt()) -> metad().
date({Date, _}) -> Date.

%% @doc retreive the time of a meta date.
-spec time(Date::metadt()) -> metat().
time({_, Time}) -> Time.

%% @doc retreive the year of a meta date.
-spec year(Date :: metadt() | metad()) -> myear().
year({{Y, _, _}, _}) -> Y;
year({Y, _, _}) -> Y.

%% @doc retreive the month of a meta date.
-spec month(Date :: metadt() | metad()) -> mmonth().
month({{_, M, _}, _}) -> M;
month({_, M, _}) -> M. 

%% @doc retreive the day of a meta date.
-spec day(Date :: metadt() | metad()) -> mday().
day({{_, _, D}, _}) -> D;
day({_, _, D}) -> D. 

%% @doc retreive the hours of a meta date.
-spec hour(Date :: metadt() | metat()) -> mhour().
hour({_, {H, _, _}}) -> H;
hour({H, _, _}) -> H. 

%% @doc retreive the minutes of a meta date.
-spec minute(Date :: metadt() | metat()) -> mmin().
minute({_, {_, M, _}}) -> M;
minute({_, M, _}) -> M. 

%% @doc retreive the seconds of a meta date.
-spec second(Date :: metadt() | metat()) -> msec().
second({_, {_, _, S}}) -> S;
second({_, _, S}) -> S. 

%% Date Coersion

%% @doc coers to seconds 
-spec to_int(Date::metadt()) -> integer().
to_int(Date) -> calendar:datetime_to_gregorian_seconds(Date).    
    
%% @doc retreive timestamp from a date upper of 1970
-spec to_timestamp(Date::metadt()) -> integer().
to_timestamp(Date) -> to_int(Date) - ?UNIX_TIME.

%% @doc convert to string 
-spec to_string(Date :: metadt()) -> string().
to_string({{Y, Mo, D}, {H, Mi, S}}) ->
    integer_to_list(D)
	++ "/"
	++ integer_to_list(Mo)
	++ "/"
	++ integer_to_list(Y)
	++ " at "
	++ integer_to_list(H)
	++ ":"
	++ integer_to_list(Mi)
	++ ":"
	++ integer_to_list(S).
	    

		 


%% Date comparison

% This implementations are naïve but provide very nice 
% perormances (no iteration or high integer calculation)


%% @doc comparison between two metadate
-spec compare_to(Date1::metadt(), Date2::metadt()) -> integer().
compare_to(Date1, Date2) ->
    case date_compare_to(date(Date1), date(Date2)) of 
        0 -> time_compare_to(time(Date1), time(Date2));
        X -> X
    end.

%% @doc comparison between two date 
-spec date_compare_to(Date1::metad(), Date2::metad()) -> integer().
date_compare_to(Date1, Date2) ->
    case {year(Date1), year(Date2)} of 
        {Y, Y} -> 
            case {month(Date1), month(Date2)} of 
                {M, M} ->  
                    case {day(Date1), day(Date2)} of
                        {D, D} -> 0;
                        {Da, Db} when Da > Db -> 1;
                        _ -> -1
                    end;
                {Ma, Mb} when Ma > Mb -> 1; 
                _ -> -1
            end;
        {Ya, Yb} when Ya > Yb -> 1;
        _ -> -1
    end.

%% @doc comparison between two times 
-spec time_compare_to(Date1::metat(), Date2::metat()) -> integer().
time_compare_to(Date1, Date2) ->
    case {hour(Date1), hour(Date2)} of 
        {H, H} -> 
            case {minute(Date1), minute(Date2)} of 
                {M, M} ->  
                    case {second(Date1), second(Date2)} of
                        {S, S} -> 0;
                        {Sa, Sb} when Sa > Sb -> 1;
                        _ -> -1
                    end;
                {Ma, Mb} when Ma > Mb -> 1; 
                _ -> -1
            end;
        {Ha, Hb} when Ha > Hb -> 1;
        _ -> -1
    end.


%% Date information

%% @doc check if the year is leap or not 
-spec is_leap(Date :: metad() | metadt() | myear()) -> boolean().
is_leap(Year) when is_integer(Year)-> calendar:is_leap_year(Year); 
is_leap(Date) -> is_leap(year(Date)).

%% @doc return the number of days of a month
-spec days_of_month(Date :: metad() | metadt()) -> integer().
days_of_month(Date) ->
    {Y, M} = {year(Date), month(Date)},
    calendar:last_day_of_the_month(Y, M).

%% This part don't need test :) 

%% @doc Generate the list of the days in a month
-spec days_sequence_of(Date :: metad() | metadt()) -> list(integer()).
days_sequence_of(Date) ->
    I = days_of_month(Date),
    lists:seq(1, I).

%% @doc Generate the list of the month in a year
-spec months_sequence() -> list(integer()).
months_sequence() -> lists:seq(1, 12).

%% @doc Day of the week 
-spec day_in_week(Date :: metad() | metadt()) -> integer().
day_in_week(Date) ->
    {D, M, Y} = {year(Date), month(Date), day(Date)},
    calendar:day_of_the_week(Y, M, D).

