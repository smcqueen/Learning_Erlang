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
	 start_child/1,
	 start_child/0
	]).

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
    io:format("~p: Entering start_link(~p)~n", [?MODULE,Master]),
    io:format("~p: Calling supervisor:start_link/3~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Master]),
    io:format("~p: Calling supervisor:start_child(~p, [~p])~n",[?MODULE, ?MODULE, Master]),
    supervisor:start_child(?MODULE, [Master]),
    io:format("~p: Calling supervisor:start_child(~p, [])~n",[?MODULE, ?MODULE]),
    supervisor:start_child(?MODULE, []),
    io:format("~p: Calling supervisor:start_child(~p, [])~n",[?MODULE, ?MODULE]),
    supervisor:start_child(?MODULE, []).

%% Master is true | false
start_child(Master) ->
    io:format("~p: Calling supervisor:start_child(~p, [~p])~n", [?MODULE, ?MODULE, Master]),
    supervisor:start_child(?MODULE, [Master]).

start_child() ->
    supervisor:start_child(?MODULE, []).
    
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
init(Master) ->
    io:format("~p: Entering init(~p)~n", [?MODULE, Master]),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

%    case Master of
%	true ->
%	    io:format("This is the master instance; get the list of orgs~n"),
	    {ok, {SupFlags, [{cwm_org, {cwm_org, start_link, []},
		temporary, brutal_kill, worker, [cwm_org]}]}}.
%	false ->
%	    io:format("This is not the master instance; check the cache for a list of orgs~n"),
%	    {ok, {SupFlags, [?ACTCHILD]}}
%    end.
		

%%%===================================================================
%%% Internal functions
%%%===================================================================


