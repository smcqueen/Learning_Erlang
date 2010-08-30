%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(cwm_master).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 doMaintenance/0,
	 finishedProcessing/2
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

-define(SLAVES_PER_SUPERVISOR, 5).
-define(DELAY_LOOP, 3000).

-record(state, {load_balancer, orgs_to_update}).

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

doMaintenance() ->
    gen_server:cast(?SERVER, doMaintenance).

finishedProcessing(Pid, OrgID) ->
    gen_server:cast(?SERVER, {finishedProcessing, {Pid, OrgID}}).

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
    simple_cache:insert(cwm_master, self()),
    {ok, #state{load_balancer = dict:new(),
	       orgs_to_update = []}}.

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
handle_cast(doMaintenance, State) ->
    maintenance(),
    {noreply, State};
handle_cast({finishedProcessing, {Pid, OrgID}}, State) ->
    io:format("~p Finished processing ~p~n", [Pid, OrgID]),
    {noreply, State}.

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
startSlaves(_SupvPid, 0) ->
    ok;
startSlaves(SupvPid, N) ->
    supervisor:start_child(SupvPid, []),
    startSlaves(SupvPid, N-1).

startSlaves([]) ->
    ok;
startSlaves([SupvPid|T]) ->
    io:format("simple_cache:lookup(~p) = ~p~n", [SupvPid,
						 simple_cache:lookup(SupvPid)]),
    case simple_cache:lookup(SupvPid) of
	{ok, NumChildren} ->
	    io:format("Supervisor ~p has ~p children~n", [SupvPid, NumChildren]),
	    if
		NumChildren < ?SLAVES_PER_SUPERVISOR ->
		    startSlaves(SupvPid, ?SLAVES_PER_SUPERVISOR - NumChildren);
		true  -> true
	    end;
	{error, not_found} ->
	    simple_cache:insert(SupvPid, 0),
	    startSlaves(SupvPid, ?SLAVES_PER_SUPERVISOR)
    end,
    startSlaves(T).

startSlaves() ->
    SupervisorList = updateSupervisorList(),
    countSlaves(SupervisorList),
    startSlaves(SupervisorList).

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
    
countSlaves([]) ->
    0;
countSlaves([SupvPid|T]) ->
    Children = getChildrenForSupervisor(SupvPid),
    simple_cache:insert(SupvPid, length(Children)),
    countSlaves(T).

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
    {ok, SupvList} = simple_cache:lookup(supervisorPid),
    SupvList.

loadBalance([], [], _ChildList) ->
    %% out of orgs and children simultaneously: we're done
%    io:format("Ran out of orgs and children simultaneously~n"),
    ok;
loadBalance([], _ChildListTail, _ChildList) ->
    %% out of orgs: we're done
%    io:format("Ran out of orgs, still have these children: ~p~n", [ChildListTail]),
    ok;
loadBalance(OrgList, [], ChildList) ->
    %% out of children, start over at the top of the childlist
%    io:format("Ran out of children, still have these orgs: ~p~n", [OrgList]),
    loadBalance(OrgList, ChildList, ChildList);
loadBalance([OrgID|OrgListTail], [ChildPid|ChildListTail], ChildList) ->
    io:format("Assigning OrgID ~p to Child ~p~n", [OrgID, ChildPid]),
    gen_server:cast(ChildPid, {processOrg, OrgID}),
%    timer:sleep(3000),
    loadBalance(OrgListTail, ChildListTail, ChildList).

getOrgsToUpdate() ->
    [1,2,3,4,5,6,7,8,9].    

maintenance() ->

    %%%===================================================================
    %%% Starts up the supervisor's complement of slaves the first time
    %%% the supervisor is detected.
    %%%===================================================================
    SupvPidList = updateSupervisorList(),
    countSlaves(SupvPidList),
    startSlaves(SupvPidList),

    %%%===================================================================
    %%% Check to see if the list of orgs needing updates has changed
    %%%===================================================================
    Orgs_to_Update = getOrgsToUpdate(),
%    Old_Orgs_to_Update = State#state.orgs_to_update,
%    case Orgs_to_Update =:= Old_Orgs_to_Update of
%	true -> NewState = State;
%	false ->
%	    NewState = #state{orgs_to_update = Orgs_to_Update,
%			      load_balancer = State#state.load_balancer},
%	    io:format("NewState = ~p~n", [NewState]),

%	    io:format("Supervisor list: ~p~n", [SupvPidList]),
	    ChildList = getAllChildren(),
	    loadBalance(Orgs_to_Update, ChildList, ChildList). 
%    end,

%    timer:sleep(?DELAY_LOOP).
%    loop(NewState).

