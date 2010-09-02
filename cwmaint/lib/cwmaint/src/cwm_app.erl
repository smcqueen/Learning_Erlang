%%%----------------------------------------------------------------
%%% @author Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------,
-module(cwm_app).

-behaviour(application).

-include("cwmaint.hrl").

%% Application callbacks
-export([
	 start/2,
	 stop/1,
	 startChild/0
	]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    io:format("~nThe ContentWatch Maintenance Server is running...~n"),
    ok = ensure_contact(),
    resource_discovery:add_local_resource(cwmaint, node()),
    resource_discovery:add_target_resource_type(cwmaint),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_RESOURCES),
    {ok, CwmaintNodes} = resource_discovery:fetch_resources(cwmaint),
    io:format("cwmaint nodes:~p~n", [CwmaintNodes]),
    case length(CwmaintNodes) > 1 of
	true ->
	    io:format("There is another instance of cwmaint running~n");
	false ->
	    io:format("This is the first cwmaint instance~n")
    end,
    case cwm_sup1:start_link() of
	{ok, SupervisorPid} ->
	    Supervisor2Pid = getSupervisor2Pid(),
	    registerPid(supervisorPid, Supervisor2Pid),
    	    startProcessors(Supervisor2Pid, ?PROCESSORS_PER_SUPERVISOR),
	    timer:sleep(10*1000),
	    cwm_manager:doProcess(),
	    case timer:apply_interval(30*1000, cwm_manager, doProcess, []) of
		{ok, _TRef} ->
		    ok;
		{error, Reason} ->
		    io:format("timer:apply_interval error: ~p~n", [Reason])
%		    {error, Reason}
	    end,
	    {ok, SupervisorPid};
	Error ->
	    Error
    end.



startChild() ->
    cwm_sup2:start_child().

extractPid([]) ->
    0;
extractPid([ChildRec|T]) ->
    {_Id, Pid, Type, _Module} = ChildRec,
    case Type of
	supervisor ->
	    Pid;
	_ ->
	    extractPid(T)
    end.

getSupervisor2Pid() ->
    ChildList = supervisor:which_children(cwm_sup1),
    extractPid(ChildList).

%process(List, F) ->
%    F(List).

%process(List, F, Param) ->
%    F(List, Param).

%startProcessing([], []) ->
%    ok;
%startProcessing([], [_OrgID|_T2]) ->
%    ok;
%startProcessing([_Pid|_T1], []) ->
%    ok;
%startProcessing([Pid|T1], [OrgID|T2]) ->
%    io:format("Calling gen_server:cast(~p, {processOrg, ~p})~n", [Pid, OrgID]),
%    gen_server:cast(Pid, {processOrg, OrgID}),
%    startProcessing(T1, T2).

%getOrgList([]) ->
%    ok;
%getOrgList([Pid|T])->
%    io:format("Org list = ~p~n", [gen_server:call(Pid, get_list)]),
%    getOrgList(T).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_contact() ->
    DefaultNodes = ['contact1@buildserver3', 'contact2@buildserver3', 'contact1@gandalf', 'contact2@gandalf'],
    case get_env(cwmaint, contact_nodes, DefaultNodes) of
	[] ->
	    {error, no_contact_nodes};
	ContactNodes ->
	    io:format("Contact nodes: ~p~n", [ContactNodes]),
	    ensure_contact(ContactNodes)
    end.

ensure_contact(ContactNodes) ->
    Answering = [N || N <- ContactNodes,
		       net_adm:ping(N) =:= pong],
    case Answering of
	[] ->
	    {error, no_contact_nodes_reachable};
	_ ->
	    DefaultTime = 6000,
	    WaitTime = get_env(cwmaint, wait_time, DefaultTime),
	    wait_for_nodes(Answering, WaitTime)
    end.

wait_for_nodes(ContactNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(length(ContactNodes), SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
    case length(nodes()) > MinNodes of
	true ->
	    ok;
	false ->
	    timer:sleep(SliceTime),
	    wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined ->
	    Default;
	{ok, Value} ->
	    Value
    end.

%start_children(_Master, 0) ->
%    ok;
%start_children(Master, Count) ->
%    case cwmaint_sup:start_child(Master) of
%	{ok, ChildPid} ->
%	    case Master of
%		true ->
%		    simple_cache:insert(masterPid, ChildPid);
%		false ->
%		    registerPid(slavePid, ChildPid)
%	    end;
%	{ok, ChildPid, _Info} ->
%	    case Master of
%		true ->
%		    simple_cache:insert(masterPid, ChildPid);
%		false ->
%		    registerPid(slavePid, ChildPid)
%	    end;
%	{error, already_present} ->
%	    io:format("~p: cwmaint_sup:start_child(~p) returned {error, already_present}", [?MODULE, Master]);
%	{error, {already_started, ChildPid}} ->
%	    io:format("~p: cwmaint_sup:start_child(~p) returned {error, {already_started, ~p}}", [?MODULE, Master, ChildPid])
%    end,
%    start_children(false, Count-1).

registerPid(Key, Pid) ->
    case simple_cache:lookup(Key) of
	{ok, Pidlist} ->
	    simple_cache:insert(Key, [Pid | Pidlist]);
	_ ->
	    simple_cache:insert(Key, [Pid])
    end.

startProcessors(_SupvPid, 0) ->
    ok;
startProcessors(SupvPid, N) ->
    supervisor:start_child(SupvPid, []),
    startProcessors(SupvPid, N-1).
