%%%----------------------------------------------------------------
%%% @author  Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------
-module(cwmaint_sup).

-behaviour(supervisor).

-include("cwmaint.hrl").

%% API
-export([start_link/1,
	 start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Master) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Master]).

start_child(ChildSpec) ->
    io:format("~p: Calling supervisor:start_child(~p)~n", [?MODULE, ChildSpec]),
    io:format("~p~n", [supervisor:start_child(?MODULE, ChildSpec)]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Master]) ->
    %% We wouldn't normally have actual code in the supervisor
    io:format("cwmaint_srv starting up...~n"),
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

		
    case Master of
	true ->
	    io:format("This is the master instance; get the list of orgs~n"),
	    {ok, {SupFlags, [?ORGCHILD]}};
	false ->
	    io:format("This is not the master instance; check the cache for a list of orgs~n"),
	    {ok, {SupFlags, [?ACTCHILD]}}
    end.
		

%%%===================================================================
%%% Internal functions
%%%===================================================================


