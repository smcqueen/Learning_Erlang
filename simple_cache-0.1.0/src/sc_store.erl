-module(sc_store).

-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1
	]).

-define(TABLE_ID, ?MODULE).
-define(WAIT_FOR_TABLES, 5000).

-record(key_to_pid, {key, pid}).

init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    mnesia:create_table(
      key_to_pid,
      [{index, [pid]},
       {attributes, record_info(fields, key_to_pid)}]).

insert(Key, Pid) ->
    mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

lookup(Key) ->
    case mnesia:dirty_read(key_to_pid, Key) of
	[{keu_to_pid, Key, Pid}] ->
	    {ok, Pid};
	[] ->
	    {error, not_found}
    end.

delete(Pid) ->
    try
	[#key_to_pid{} = Record] = mnesia:dirty_index_read(key_to_pid,Pid, #key_to_pid.pid),
	mnesia:dirty_delete_object(Record)
    catch
	_C:_E ->
	    ok
    end.
