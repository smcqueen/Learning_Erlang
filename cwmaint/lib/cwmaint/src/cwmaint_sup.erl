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
	 start_child/0,
	 start_child/1
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
%    io:format("~p: Entering start_link()~n", [?MODULE]),
%    io:format("~p: Calling supervisor:start_link(~p, ~p, [~p])~n", [?MODULE,
%								 {local, ?SERVER},
%								 ?MODULE,
%								 Master]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Master]).

start_child() ->
%    io:format("~p: Entering start_child()~n", [?MODULE]),
    supervisor:start_child(?SERVER, []).
    
start_child(IsMaster) ->
%    io:format("~p: Entering start_child(~p)~n", [?MODULE, IsMaster]),
    supervisor:start_child(?SERVER, [IsMaster]).

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
init(_Args) ->
%    io:format("~p: Entering init()~n", [?MODULE]),
    Server = {cwm_org, {cwm_org, start_link, []},
	      temporary, brutal_kill, worker, [cwm_org]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
		

%%%===================================================================
%%% Internal functions
%%%===================================================================


