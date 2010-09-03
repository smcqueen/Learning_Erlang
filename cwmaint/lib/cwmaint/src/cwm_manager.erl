%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(cwm_manager).

-behaviour(gen_server).

-include("cwmaint.hrl").

%% API
-export([
	 start_link/0,
	 doProcess/0,
	 processorAvailable/1,
	 processorAvailable/2,
	 mylog/4
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

-record(state, {is_manager,
		orgs_to_update,
	        available_processors,
	        busy_processors,
		orgs_done
	       }).

mylog(_A, _B, _C, _D) ->
    ok.

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

doProcess() ->
    gen_server:cast(?SERVER, doProcess).

processorAvailable(Pid) ->
    gen_server:cast(?SERVER, {processorAvailable, Pid}).

processorAvailable(Pid, OrgID) ->
    gen_server:cast(?SERVER, {processorAvailable, {Pid, OrgID}}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    IsManager = checkManager(),
    mysql:start_link(db, ?DBSERVER, ?USERNAME, ?PASSWORD, ?DATABASE),
    {data, Result} = mysql:fetch(db, "show tables like 'activity%'"),
    TableList= mysql:get_result_rows(Result),
    io:format("Got tables ~p~n", [lists:flatten(TableList)]),
    convertTableNames(lists:flatten(TableList)),
    {ok, #state{available_processors = [],
	       orgs_to_update = [],
	       is_manager = IsManager,
	       busy_processors = [],
	       orgs_done = []}}.

convertTableNames([]) ->
    ok;
convertTableNames([TableNameBin | T]) ->
    io:format("~p -> ~p~n", [TableNameBin, binary_to_list(TableNameBin)]),
    convertTableNames(T).

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
handle_cast(doProcess, State) ->
    IsManager = checkManager(),
    case IsManager == State#state.is_manager of
	true ->
	    NewState = State;
	false ->
	    %% A failover has occurred, refresh the processor list
	    SupvPidList = updateSupervisorList(),
	    countProcessors(SupvPidList),
	    ProcessorList = getAllChildren(),
	    NewState = State#state{is_manager = IsManager,
				   available_processors = ProcessorList}
    end,
    NewState2 = process(NewState),
    {noreply, NewState2};
handle_cast(Request, State) ->
    case Request of
	{processorAvailable, Pid} ->
	    io:format("Processor ~p is now available...~n", [Pid]),
            %% If the PID is already in the list, don't re-add it
	    case lists:member(Pid, State#state.available_processors) of
		true ->
		    NewState2 = State;
		false ->
		    AvailableProcessors = State#state.available_processors,
		    BusyProcessors = lists:delete(Pid, State#state.busy_processors),
		    NewState2 = State#state{available_processors = [Pid | AvailableProcessors],
					   busy_processors = BusyProcessors}
	    end;

	{processorAvailable, Pid, Orgid} ->
	    io:format("Processor ~p finished processing Org ~p...~n", [Pid, Orgid]),
            %% If the PID is already in the list, don't re-add it
	    case lists:member(Pid, State#state.available_processors) of
		true ->
		    NewState = State;
		false ->
		    AvailableProcessors = State#state.available_processors,
		    BusyProcessors = lists:delete(Pid, State#state.busy_processors),
		    NewState = State#state{available_processors = [Pid | AvailableProcessors],
					   busy_processors = BusyProcessors}
	    end,
	    NewState2 = NewState#state{orgs_done = [Orgid | NewState#state.orgs_done]}
            %    NewState2 = assignWork(NewState),
    end,
    {noreply, NewState2}.


%handle_cast(_Msg, State) ->
%    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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
%startProcessors(_SupvPid, 0) ->
%    ok;
%startProcessors(SupvPid, N) ->
%    supervisor:start_child(SupvPid, []),
%    startProcessors(SupvPid, N-1).

%startProcessors([]) ->
%    ok;
%startProcessors([SupvPid|T]) ->
%    io:format("simple_cache:lookup(~p) = ~p~n", [SupvPid,
%						 simple_cache:lookup(SupvPid)]),
%    case simple_cache:lookup(SupvPid) of
%	{ok, NumChildren} ->
%	    io:format("Supervisor ~p has ~p children~n", [SupvPid, NumChildren]),
%	    if
%		NumChildren < ?PROCESSORS_PER_SUPERVISOR ->
%		    startProcessors(SupvPid, ?PROCESSORS_PER_SUPERVISOR - NumChildren);
%		true  -> true
%	    end;
%	{error, not_found} ->
%	    simple_cache:insert(SupvPid, 0),
%	    startProcessors(SupvPid, ?PROCESSORS_PER_SUPERVISOR)
%    end,
%    startProcessors(T).

%startProcessors() ->
%    SupervisorList = updateSupervisorList(),
%    countProcessors(SupervisorList),
%    startProcessors(SupervisorList).

getChildrenForSupervisor([], ChildList) ->
    ChildList;
getChildrenForSupervisor([Child|T], ChildList) ->
    {_Id, ChildPid, _Type, _Modules} = Child,
    getChildrenForSupervisor(T, [ChildPid | ChildList]).

getChildrenForSupervisor(SupvPid) ->
    getChildrenForSupervisor(supervisor:which_children(SupvPid), []).

getAllChildren([], ChildList) ->
    ChildList;
getAllChildren([SupvPid|T], ChildList) ->
    SupvChildren = getChildrenForSupervisor(SupvPid),
    NewChildList = SupvChildren ++ ChildList,
    getAllChildren(T, NewChildList).

getAllChildren() ->
    getAllChildren(getSupervisorList(), []).
    
countProcessors([]) ->
    0;
countProcessors([SupvPid|T]) ->
    Children = getChildrenForSupervisor(SupvPid),
    simple_cache:insert(SupvPid, length(Children)),
    countProcessors(T).

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
        false ->
            false;
        true ->
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true ->
                    true;
                false ->
                    false;
                {badrpc, _Reason} ->
                    false
            end
    end.

updateSupervisorList([]) ->
    % Get and return the final (current) supervisor list
    getSupervisorList();
updateSupervisorList([SupvPid|T]) ->
    case is_pid_alive(SupvPid) of
	true ->
	    ok;
	false ->
	    TempList = getSupervisorList(),
%	    io:format("Supervisor at ~p deleted: current list is ~p~n", [SupvPid, TempList]),
	    TempList2 = lists:delete(SupvPid, TempList),
%	    io:format("New list is ~p~n", [TempList2]),
	    simple_cache:insert(supervisorPid, TempList2),
	    simple_cache:delete(SupvPid)
    end,
    updateSupervisorList(T).

updateSupervisorList() ->
   updateSupervisorList(getSupervisorList()). 

getSupervisorList() ->
    case simple_cache:lookup(supervisorPid) of
	{ok, SupvList} ->
	    SupvList;
	{error, not_found} ->
	    {ok, SupvList} = resource_discovery:fetch_resources(cwsupv),
	    simple_cache:insert(supervisorPid, SupvList)
    end,
    SupvList.

%loadBalance([], [], _ChildList) ->
    %% out of orgs and children simultaneously: we're done
%%    io:format("Ran out of orgs and children simultaneously~n"),
%    ok;
%loadBalance([], _ChildListTail, _ChildList) ->
    %% out of orgs: we're done
%%    io:format("Ran out of orgs, still have these children: ~p~n", [ChildListTail]),
%    ok;
%loadBalance(OrgList, [], ChildList) ->
    %% out of children, start over at the top of the childlist
%%    io:format("Ran out of children, still have these orgs: ~p~n", [OrgList]),
%    loadBalance(OrgList, ChildList, ChildList);
%loadBalance([OrgID|OrgListTail], [ChildPid|ChildListTail], ChildList) ->
%    io:format("Assigning OrgID ~p to Child ~p~n", [OrgID, ChildPid]),
%    gen_server:cast(ChildPid, {processOrg, OrgID}),
%    loadBalance(OrgListTail, ChildListTail, ChildList).

pingProcessors([], State) ->
    State;
pingProcessors([ProcID|T], State) ->
    case is_pid_alive(ProcID) of
	true ->
	    NewState = State;
	false ->
	    {ok, NewPID} = supervisor:start_child(cwm_sup2, []),
	    BusyProcessors = lists:delete(ProcID, State#state.busy_processors),
	    AvailableProcessors = [NewPID | State#state.available_processors],
	    NewState = State#state{busy_processors = BusyProcessors,
				   available_processors = AvailableProcessors}
    end,
    pingProcessors(T, NewState).
	    
pingProcessors(State) ->
    BusyProcessors = State#state.busy_processors,
    pingProcessors(BusyProcessors, State).

assignWork([], [], State) ->
    io:format("No work to do...~n"),
    State;
assignWork([], [_H|_T], State) ->
    io:format("No work to do...~n"),    State;
assignWork([_H|_T], [], State) ->
    io:format("No workers available...~n"),
    case length(State#state.busy_processors)>0 of
	false ->
	    NewState = State;
	true ->
	    NewState = pingProcessors(State)
    end,
    NewState;
assignWork([OrgID|OrgListTail], [ProcID|ProcIDTail], State) ->
    io:format("Assigning OrgID ~p to Processor ~p~n", [OrgID, ProcID]),
    gen_server:cast(ProcID, {processOrg, OrgID}),
    NewState = State#state{orgs_to_update = lists:delete(OrgID, State#state.orgs_to_update),
    available_processors = lists:delete(ProcID, State#state.available_processors),
    busy_processors = [ProcID | State#state.busy_processors]},
    timer:sleep(5000),
    NewState2 = assignWork(OrgListTail, ProcIDTail, NewState),
    NewState2.
    

assignWork(State) ->
    NewState = assignWork(State#state.orgs_to_update, State#state.available_processors, State),
    NewState.

getActiveOrgs([], OrgList, State) ->
    State#state{orgs_to_update = OrgList -- State#state.orgs_done,
	   orgs_done = []};
getActiveOrgs([Orgid|T], OrgList, State) ->
    case is_number(Orgid) of
       true -> OrgidT = integer_to_list(Orgid);
       false ->
	    case is_list(Orgid) of
	       true -> [OrgidT|_T2] = Orgid;
	       false -> OrgidT = Orgid
	    end
    end,
    Table = string:concat("activity", integer_to_list(OrgidT)),
    Select = string:concat("select count(*) from ",Table),
    case (mysql:fetch(db, Select)) of
	{data, _MysqlRes} ->
	    NewList = [OrgidT | OrgList];
	{error, _Error} ->
	    NewList = OrgList
    end,
    getActiveOrgs(T, NewList, State).
    
getOrgsToUpdate(State) ->
    {data, Result} = mysql:fetch(db, "select orgid from " ++ ?ORGTYPE),
    OrgIDList = mysql:get_result_rows(Result),
    getActiveOrgs(OrgIDList, [], State).

checkManager() ->
    case simple_cache:lookup(?MODULE) of
	{ok, ManagerPid} ->
	    case is_pid_alive(ManagerPid) of
		true ->
		    IsManager = ManagerPid =:= self();
		false ->
		    simple_cache:insert(?MODULE, self()),
		    IsManager = true
	    end;
	{error, not_found} ->
	    simple_cache:insert(?MODULE, self()),
	    IsManager = true
    end,
    IsManager.

process(State) ->
    io:format("~p:process() - State = ~p~n", [?MODULE, State]),
    case State#state.is_manager of
	false ->
	    io:format("This is not the manager, returning...~n"),
	    State;
	true ->
            %%%===================================================================
            %%% Starts up the supervisor's complement of slaves the first time
            %%% the supervisor is detected.
            %%%===================================================================
%	    SupvPidList = updateSupervisorList(),
%	    countProcessors(SupvPidList),
%	    startProcessors(SupvPidList),

            %%%===================================================================
            %%% Check to see if the list of orgs needing updates has changed
            %%%===================================================================
	    case length(State#state.orgs_to_update) == 0 of
		true ->
		    NewState = getOrgsToUpdate(State),
%		    NewState = State#state{orgs_to_update = Orgs_to_Update},
		    io:format("Got new orgs: ~p~n", [NewState#state.orgs_to_update]);
	        false ->
		    NewState = State
	    end,
	    NewState2 = assignWork(NewState),
	    io:format("State now = ~p~n", [NewState2]),
	    NewState2
    end.


