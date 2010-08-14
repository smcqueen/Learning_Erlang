-module(sc_store).

-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1,
	 list_keys/0
	]).

-define(TABLE_ID, ?MODULE).
-define(WAIT_FOR_TABLES, 5000).
-define(TABLE, key_to_pid).
-record(?TABLE, {key, pid}).

init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
    dynamic_db_init(lists:delete(node(), CacheNodes)).

list_keys() ->
    mnesia:dirty_all_keys(?TABLE).

insert(Key, Pid) ->
    mnesia:dirty_write(#?TABLE{key = Key, pid = Pid}).

lookup(Key) ->
    case mnesia:dirty_read(?TABLE, Key) of
	[{?TABLE, Key, Pid}] ->
	    case is_pid_alive(Pid) of
		true -> {ok, Pid};
		false -> {error, not_found}
	    end;
	[] ->
	    {error, not_found}
    end.

delete(Pid) ->
    try
	[#key_to_pid{} = Record] = mnesia:dirty_index_read(?TABLE,Pid, #?TABLE.pid),
	mnesia:dirty_delete_object(Record)
    catch
	_C:_E ->
	    ok
    end.

dynamic_db_init([]) ->
    mnesia:create_table(
      ?TABLE,
      [{index, [pid]},
       {attributes, record_info(fields, ?TABLE)}]);
dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok, [Node]} ->
	    mnesia:add_table_copy(schema, node(), ram_copies),
	    mnesia:add_table_copy(key_to_pid, node(), ram_copies),
	    Tables = mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
	_ ->
	    add_extra_nodes(T)
    end.

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
