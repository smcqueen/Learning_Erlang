%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to server1
-module(server1).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([rpc/2, start/2]).

%%
%% API Functions
%%

start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, Response} -> Response
	end.

%%
%% Local Functions
%%

loop(Name, Mod, State) ->
	receive
		{From, Request} ->
			{Response, State1} = Mod:handle(Request, State),
			From ! {Name, Response},
			loop(Name, Mod, State1)
	end.