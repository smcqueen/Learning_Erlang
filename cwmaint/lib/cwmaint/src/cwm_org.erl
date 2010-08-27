%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(cwm_org).

-behaviour(gen_server).

-include("cwmaint.hrl").

%% API
-export([
	 start_link/1,
	 get_list/0,
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

-record(state, {master}).

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
start_link(IsMaster) ->
%    io:format("~p(~p): Entering start_link(~p)~n", [?MODULE, self(), IsMaster]),
    gen_server:start_link(?MODULE, [IsMaster], []).

get_list() ->
%    io:format("~p(~p): Entering get_list()~n", [?MODULE, self()]),
    gen_server:call(self(), get_list).

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
init([IsMaster]) ->
%    io:format("~p(~p): Entering init([~p])~n", [?MODULE, self(), IsMaster]),
    {ok, #state{master = IsMaster}, 0}.

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
handle_call(get_list, _From, State) ->
%    io:format("~p(~p): Entering handle_call(get_list, _From, ~p)~n", [?MODULE, self(), State]),
    case State#state.master of
	true ->
	    {reply, {ok, [1,2,3,4,5]}, State};
	false ->
	    {reply, {error, not_master}, State}
    end.


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
    io:format("Processing org ~p~n", [OrgID]),
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
%    io:format("~p(~p): Entering handle_info(timeout, ~p)~n", [?MODULE, self(), State]),
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
%% 1. Get a list of orgids from the database
%% 2. Find out which ones have activity tables
%% 3. Insert a key, value into the simple_cache with the list of orgids
%% 4. Call cwmaint_sup:start_child(?ACTCHILD)to process an orgid (each
%%    ACTCHILD will call cwmaint_sup:start_child again 
%% 5. Sleep for ?DELAY_TIME seconds and then do it all over again.
%% @spec
%% @end
%%--------------------------------------------------------------------
loop() ->
    %% get list of orgids

    %% get activity tables

    %% call simple_cache:insert

    %% generate ACTCHILD process

    timer:sleep(10000),

    loop().
