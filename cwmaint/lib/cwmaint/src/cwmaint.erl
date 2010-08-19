-module(cwmaint).

-export([do_maintenance/0]).

-record(state, {tablename, idlist}).

mylog(_A, _B, _C, _D) ->
    ok.

do_maintenance() ->
    Now = calendar:local_time(),
%    AllRows = [[1,{datetime,{{2007,10,24},{14,5,44}}}],
% [2,{datetime,{{2007,10,24},{14,6,7}}}],
% [3,{datetime,{{2007,10,24},{14,6,9}}}]],
%    process_rows(Now, AllRows).
    mysql:start_link(db, "dbserver", "masteruser", "mast3rus3r", "relay"),
    {data, Result} = mysql:fetch(db, "select orgid from companies"),
    Tablelist = mysql:get_result_rows(Result),
    io:format("Got ~p orgids~n", [length(Tablelist)]),
    process_tables(Now, Tablelist).

process_tables(_Now, []) ->
    ok;
process_tables(Now, List) ->
    [H|T] = List,
    [Orgid|_] = H,
    Table = "activity" ++ integer_to_list(Orgid),
    Select = "select activityid, createdatetime from " ++ Table ++ " order by createdatetime desc limit 1000",
    case (mysql:fetch(db, Select)) of
	{data, MysqlRes} ->
%	    io:format("~p~n", [Result]),
	    AllRows = mysql:get_result_rows(MysqlRes),
%	    io:format("~p~n", [AllRows]),
	    io:format("Table ~p~n", [Table]),
	    #state{tablename = Table, idlist = []},
	    io:format("~p~n", [#state{}]),
	    process_rows(Now,AllRows,Table),
	    process_tables(Now, T);
	{error, _Error} ->
%	    io:format("~p~n", [Error]),
	    process_tables(Now, T)
    end.

process_rows(_Now, [], Table) ->
    ok;
process_rows(Now, AllRows, Table) ->
    [H|T] = AllRows,
    [Activityid|D] = H,
    [D1|_] = D,
    {datetime, Datetime} = D1,
    Age = age(Now, Datetime),
    case Age > 5 of
	true ->
	    io:format("Activityid = ~p, DateTime = ~p", [Activityid, Datetime]),
	    io:format(", Age = ~p days~n", [Age]),
	    process_rows(Now, T, Table);
	false ->
	    process_rows(Now, T, Table)
    end.

age(Now, Then) ->
%    io:format("age received (~p,~p)~n", [Now, Then]),
    SecondsNow = calendar:datetime_to_gregorian_seconds(Now),
    SecondsThen = calendar:datetime_to_gregorian_seconds(Then),
    trunc((SecondsNow-SecondsThen)/(60*60*24)).
