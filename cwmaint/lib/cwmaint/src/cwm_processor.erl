%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(cwm_processor).

-behaviour(gen_server).

-include("cwmaint.hrl").

%% API
-export([
	 start_link/0,
	 processOrg/1
	]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

processOrg(OrgID) ->
    gen_server:cast(?SERVER, {processOrg, OrgID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Immediately returns signaling a timeout of 0, using the same trick
%% as in Chapter 3 of "Erlang and OTP in Action" (see listings 3.4 and
%% 3.5) to finish the startup without keeping the caller of init/1
%% waiting: the 0 timeout causes the new gen_server process to drop
%% immediately into the timeout clause of handle_info/2.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, CwmManagerPid} = simple_cache:lookup(cwm_manager),
%    mysql:start_link(db, ?DBSERVER, ?USERNAME, ?PASSWORD, ?DATABASE),
%    mysql:connect(db, ?DBSERVER, undefined, ?USERNAME, ?PASSWORD, ?DATABASE, true),
    gen_server:cast(CwmManagerPid, {processorAvailable, self()}),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({processOrg, OrgID}, State) ->
    io:format("~p Processing org ~p~n", [self(), OrgID]),
    Table = "activity" ++ integer_to_list(OrgID),
    Now = calendar:local_time(),
%    Select = "select activityid, createdatetime from " ++ Table ++ " order by createdatetime desc limit " ++ "1000",
    Select = "select activityid from " ++ Table ++ " where datediff(now(), createdatetime) > " ++ integer_to_list(?AGE),
    case (mysql:fetch(db, Select, 10 * 1000)) of
	{data, MysqlRes} ->
	    AllRows = mysql:get_result_rows(MysqlRes),
	    io:format("~p got ~p rows from ~p~n", [self(), length(AllRows), Table]);
%	    process_rows(Now, AllRows, Table);
	{error, _Error} ->
	    ok
    end,
    
    report(OrgID, 10),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Enter the processing loop
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
report(OrgID, N) ->
    case N > 0 of
	true ->
	    case simple_cache:lookup(cwm_manager) of
		{ok, CwmManagerPid} ->
		    gen_server:cast(CwmManagerPid, {processorAvailable, self(), OrgID});
		{error, not_found} ->
		    io:format("Attempt #~p: Manager not found, retrying...~n", [N]),
                    %% Manager not found: may be in a failover condition.
                    %% Retry N times
		    timer:sleep(2000),
		    report(OrgID, N-1)
	    end;
	false ->
	    false
    end.

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
