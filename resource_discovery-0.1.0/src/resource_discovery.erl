%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 10 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(resource_discovery).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 fetch_resource_types/0,
	 trade_resources/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {target_resource_types,
	        local_typed_resources,
	        typed_resources
	       }).

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
    {ok, #state{target_resource_types = [],
	        local_typed_resources       = dict:new(),
	        typed_resources             = dict:new()}}.

%%--------------------------------------------------------------------
%% @doc Add a target resouce (one I'm looking for)
%% @spec
%% @end
%%--------------------------------------------------------------------
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

%%--------------------------------------------------------------------
%% @doc Add to the list of resources I currently have
%% @spec
%% @end
%%--------------------------------------------------------------------
add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

%%--------------------------------------------------------------------
%% @doc Fetch a list of resources
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

%%--------------------------------------------------------------------
%% @doc Fetch a list of all resource types
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch_resource_types() ->
    gen_server:call(?SERVER, fetch_resource_types).

%%--------------------------------------------------------------------
%% @doc Trigger resource trading
%% @spec
%% @end
%%--------------------------------------------------------------------
trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

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
handle_call({fetch_resources, Type}, _From, State) ->
    {reply, dict:find(Type, State#state.typed_resources), State};
handle_call(fetch_resource_types, _From, State) ->
    {reply, dict:fetch_keys(State#state.typed_resources), State}.

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
handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Instance}}, State) ->
    LocalResources = State#state.local_typed_resources,
    NewLocalResources = add_resource(Type, Instance, LocalResources),
    {noreply, State#state{local_typed_resources = NewLocalResources}};
handle_cast(trade_resources, State) ->
    LocalResources = State#state.local_typed_resources,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
	      gen_server:cast({?SERVER, Node},
			      {trade_resources, {node(), LocalResources}})
      end,
      AllNodes),
    {noreply, State};
handle_cast({trade_resources, {ReplyTo, RemoteResources}},
	    #state{local_typed_resources       = LocalResources,
		   target_resource_types = TargetTypes,
		   typed_resources             = Resources} = State) ->
    ResourceList = resources_for_types(TargetTypes, RemoteResources),
    NewResources = add_resources(ResourceList, Resources),
    case ReplyTo of
	noreply ->
	    ok;
	_  ->
	    gen_server:cast({?SERVER, ReplyTo},
	                    {trade_resources, {noreply, LocalResources}})
    end,
    {noreply, State#state{typed_resources = NewResources}}.

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

add_resource(Type, Resource, Dict) ->
    case dict:find(Type, Dict) of
	{ok, ResourceList} ->
	    NewList = [Resource | lists:delete(Resource, ResourceList)],
            dict:store(Type, NewList, Dict);
	error ->
	    dict:store(Type, [Resource], Dict)
    end.

add_resources([{Type, Identifier} | T], Dict) -> 
    add_resources(T, add_resource(Type, Identifier, Dict));
add_resources([], Dict) ->
    Dict.

resources_for_types(Types, Resources) ->
    Fun = fun(Type, Acc) ->
		  case dict:find(Type, Resources) of
		      {ok, List} ->
			  [{Type, Instance} || Instance <- List] ++ Acc;
		      error ->
			  Acc
		  end
	  end,
    lists:foldl(Fun, [], Types).
