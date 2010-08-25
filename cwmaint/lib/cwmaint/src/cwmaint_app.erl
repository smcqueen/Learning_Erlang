%%%----------------------------------------------------------------
%%% @author Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------,
-module(cwmaint_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(WAIT_FOR_RESOURCES, 2500).

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
	    Master = false,
	    io:format("There is another instance of cwmaint running~n");
	false ->
	    Master = true,
	    io:format("This is the first cwmaint instance~n")
    end,
    case cwmaint_sup:start_link(Master) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

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
    DefaultNodes = ['contact1@gandalf', 'contact2@gandalf'],
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

