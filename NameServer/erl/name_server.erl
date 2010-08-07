%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>

-module(name_server).

-export([start/0, store/2, lookup/1, stop/0]).

start() ->
	register(?MODULE, spawn(fun() -> loop() end)).

store(Key, Value) ->
	rpc({store, Key, Value}).

lookup(Key) ->
	rpc({lookup, Key}).

stop() ->
	rpc({stop}).

rpc(Q) ->
	?MODULE ! {self(), Q},
	receive
		{?MODULE, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {store, Key, Value}} ->
			put(Key, {ok, Value}),
			From ! {?MODULE, true},
			loop();
		{From, {lookup, Key}} ->
			From ! {?MODULE, get(Key)},
			loop();
		{_From, {stop}} ->
			true;
		stop ->
			true
	 end.
									
