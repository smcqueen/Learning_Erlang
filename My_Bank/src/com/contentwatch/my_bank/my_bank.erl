%%% -------------------------------------------------------------------
%%% Author  : smcqueen
%%% Description :
%%%
%%% Created : Aug 4, 2010
%%% -------------------------------------------------------------------
-module(my_bank).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start()	-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()	-> gen_server:call(?MODULE, stop).
new_account(Who)		-> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount)	-> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount)	-> gen_server:call(?MODULE, {remove, Who, Amount}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, ets:new(?MODULE, [])}.	% "State" is the ets table, which shows up as "Tab" below

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({new, Who}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of
				[]	-> ets:insert(Tab, {Who, 0}),
					   {welcome, Who};
				[_]	->	{Who, you_already_are_a_customer}
			end,
	{reply, Reply, Tab};
handle_call({add, Who, Amount}, _From, Tab)	->
	Reply = case ets:lookup(Tab, Who) of
				[]	->	not_a_customer;
				[{Who, Balance}]	->	
					NewBalance = Balance + Amount,
					ets:insert(Tab, {Who, NewBalance}),
					{thanks, Who, your_balance_is, NewBalance}
			end,
	{reply, Reply, Tab};
handle_call({remove, Who, Amount}, _From, Tab) ->
	Reply = case ets:lookup(Tab, Who) of
				[]	->	not_a_customer;
				[{Who, Balance}] when Amount =< Balance ->
						NewBalance = Balance - Amount,
						ets:insert(Tab, {Who, NewBalance}),
						{thanks, Who, your_balance_is, NewBalance};
				[{Who, Balance}] ->
						{sorry, Who, you_only_have, Balance, in_the_bank}
			end,
	{reply, Reply, Tab};
handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

