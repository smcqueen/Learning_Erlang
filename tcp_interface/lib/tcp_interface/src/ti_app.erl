%%%----------------------------------------------------------------
%%% @author Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------,
-module(ti_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1155).

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
    Port = case application:get_env(tcp_interface, port) of
	       {ok, P} -> P;
	       undefined -> ?DEFAULT_PORT
	   end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case ti_sup:start_link(LSock) of
        {ok, Pid} ->
	    ti_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
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


