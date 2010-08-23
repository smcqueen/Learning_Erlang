%%%----------------------------------------------------------------
%%% @author  Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------
-module(gws_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/4, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1155).

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
start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE,
				      [Callback, IP, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

start_child(Server) ->
    supervisor:start_child(Server, []).

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
init([Callback, IP, Port, UserArgs]) ->
    BasicSockOpts = [binary,              % Binary data, not strings
		     {active, false},     % Socket opened in passive mode
		     {packet, http_bin},  % Expecting http formatted data,
		                          % meaning that the socket will parse
		                          % the text for us and send us
		                          % messages to handle
		     {reuseaddr, true}],  % Allow port reuse sooner than
                                          % otherwise would be true
    SockOpts = case IP of
		   undefined -> BasicSockOpts;
		   _         -> [{ip, IP} | BasicSockOpts]
	       end,
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = {gws_server, {gws_server, start_link,
			   [Callback, LSock, UserArgs]},
	      temporary, brutal_kill, worker, [gws_server]},
    RestartStrategy = {simple_one_for_one,
		       1000,
		       3600},
    {ok, {RestartStrategy, [Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


