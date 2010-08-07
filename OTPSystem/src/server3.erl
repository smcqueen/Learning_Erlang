%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to server1
-module(server3).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([rpc/2, start/2, swap_code/2]).

%%
%% API Functions
%%

start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, Response} 	-> Response
	end.

%%
%% Local Functions
%%

loop(Name, Mod, OldState) ->
	receive
		{From, {swap_code, NewCallBackMod}} ->
			From ! {Name, ack},
			loop(Name, NewCallBackMod, OldState);
		{From, Request} ->
			{Response, NewState} = Mod:handle(Request, OldState),
			From ! {Name, Response},
			loop(Name, Mod, NewState)
	end.
