-module(cwmaint).

-export([do_maintenance/0]).

-record(state, {idlist}).

-define(MAX, "1000").
-define(AGE, 30).

mylog(_A, _B, _C, _D) ->
    ok.

do_maintenance() ->
    State = #state{},
    Now = calendar:local_time(),
    mysql:start_link(db, "dbserver", "masteruser", "mast3rus3r", "prorelay"),
    {data, Result} = mysql:fetch(db, "select orgid from companies"),
    Tablelist = mysql:get_result_rows(Result),
    io:format("Got ~p orgids~n", [length(Tablelist)]),
    process_tables(Now, Tablelist, State).

process_tables(_Now, [], _State) ->
    ok;
process_tables(Now, List, State) ->
    [H|T] = List,
    [Orgid|_] = H,
    Table = "activity" ++ integer_to_list(Orgid),
    Select = "select activityid, createdatetime from " ++ Table ++ " order by createdatetime desc limit " ++ ?MAX,
    case (mysql:fetch(db, Select)) of
	{data, MysqlRes} ->
	    Idlist=State#state.idlist,
	    case Idlist of
		undefined ->
		    NewState = State#state{idlist=Orgid};
		_ ->
		    NewState = State#state{idlist=[Orgid | Idlist]}
	    end,
	    io:format("~p~n", [NewState]),
	    AllRows = mysql:get_result_rows(MysqlRes),
	    process_rows(Now, AllRows, Table);
	{error, _Error} ->
	    NewState = State,
	    ok
    end,
    process_tables(Now, T, NewState).

process_rows(_Now, [], Table) ->
    ok;
process_rows(Now, AllRows, Table) ->
    [H|T] = AllRows,
    [Activityid|D] = H,
    [D1|_] = D,
    {datetime, Datetime} = D1,
    Age = age(Now, Datetime),
    case Age > ?AGE of
	true ->
	    io:format("Activityid = ~p, DateTime = ~p", [Activityid, Datetime]),
	    io:format(", Age = ~p days~n", [Age]);
	false ->
	    ok
    end,
    process_rows(Now, T, Table).

age(Now, Then) ->
%    io:format("age received (~p,~p)~n", [Now, Then]),
    SecondsNow = calendar:datetime_to_gregorian_seconds(Now),
    SecondsThen = calendar:datetime_to_gregorian_seconds(Then),
    trunc((SecondsNow-SecondsThen)/(60*60*24)).
