-module(cwmaint).

-export([start/0]).

-record(state, {idlist}).

-define(MAX, "1000").
-define(AGE, 30).

mylog(_A, _B, _C, _D) ->
    ok.

start() ->
    application:start(sasl),
    application:start(mnesia),
    application:start(resource_discovery),
    application:start(simple_cache),
    do_maintenance().

do_maintenance() ->
    mysql:start_link(db, "dbserver", "masteruser", "mast3rus3r", "prorelay"),
    {data, Result} = mysql:fetch(db, "select orgid from companies"),
    Orglist = mysql:get_result_rows(Result),
    io:format("Got ~p orgids~n", [length(Orglist)]),
    State = #state{},
    find_tables(Orglist, State),
    process_tables().

find_tables([], _State) ->
    ok;
find_tables(Orglist, State) ->
    [H|T] = Orglist,
    [Orgid|_] = H,
    Table = "activity" ++ integer_to_list(Orgid),
    Select = "select count(*) from " ++ Table,
    case (mysql:fetch(db, Select)) of
	{data, _MysqlRes} ->
	    Idlist=State#state.idlist,
	    case Idlist of
		undefined ->
		    NewState = State#state{idlist=Orgid};
		_ ->
		    NewState = State#state{idlist=[Orgid | Idlist]}
	    end,
	    simple_cache:insert(orgids, NewState#state.idlist),
	    io:format("~p~n", [NewState]);
	{error, _Error} ->
	    NewState = State,
	    ok
    end,
    find_tables(T, NewState).

process_tables() ->
    case simple_cache:lookup(orgids) of
	{ok, undefined} ->
	    {ok, no_tables};
	{ok, Orgidlist} ->
	    Now = calendar:local_time(),
	    case length(Orgidlist) of
		1 ->
		    Orgid = lists:flatten(Orgidlist),
		    simple_cache:insert(orgids, undefined);
		_ ->
		    [Orgid | T] = Orgidlist,
		    simple_cache:insert(orgids, T)
	    end,
	    process_tables(Now, Orgid);
	{error, not_found} ->
	    {error, no_tables}
    end.

process_tables(_Now, []) ->
    ok;
process_tables(Now, Orgidlist) ->
    [Orgid|T] = Orgidlist,
    Table = "activity" ++ integer_to_list(Orgid),
    Select = "select activityid, createdatetime from " ++ Table ++ " order by createdatetime desc limit " ++ ?MAX,
    case (mysql:fetch(db, Select)) of
	{data, MysqlRes} ->
	    AllRows = mysql:get_result_rows(MysqlRes),
	    process_rows(Now, AllRows, Table);
	{error, _Error} ->
	    ok
    end,
    process_tables(Now, T).
    
process_rows(_Now, [], _Table) ->
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
