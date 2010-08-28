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
	 getList/0,
	 startLoop/0
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

getList() ->
    gen_server:call(?SERVER, getList).

startLoop() ->
    gen_server:cast(?SERVER, startLoop).


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
handle_call(getList, _From, State) ->
    {reply, {ok, [1,2,3,4,5,6]}, State}.

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
handle_cast(startLoop, State) ->
    loop(),
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
startSlaves([]) ->
    ok;
startSlaves([SupvPid|T]) ->
    {ok, NumChildren} = simple_cache:lookup(SupvPid),
    io:format("Supervisor ~p has ~p children~n", [SupvPid, NumChildren]),
    if
	NumChildren < 5 ->
	    io:format("~p~n", [supervisor:start_child(SupvPid, [])]);
	true  -> true
       end,
    startSlaves(T).

startSlaves() ->
    {ok, SupervisorList} = simple_cache:lookup(supervisorPid),
    startSlaves(SupervisorList).

countSlaves([]) ->
    0;
countSlaves([SupvPid|T]) ->
    try
	Children = supervisor:which_children(SupvPid),
	simple_cache:insert(SupvPid, length(Children))
    catch
	%%% TODO: replace this call with a call that will drop the
	%%% SupvPid from the list
	_:_ -> simple_cache:delete(SupvPid)
    end,
    countSlaves(T).

countSlaves() ->
    {ok, SupervisorList} = simple_cache:lookup(supervisorPid),
    countSlaves(SupervisorList).

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

loop() ->
    io:format("~p: in main loop...~n", [?MODULE]),
    timer:sleep(10000),
    countSlaves(),
    startSlaves(),
%    io:format("Attempting to start cwm_slaves on each supervisor found...~n"),
%    startSlaves(SupervisorList),
    loop().
