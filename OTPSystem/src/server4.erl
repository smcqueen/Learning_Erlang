%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to server1
-module(server4).

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
		{Name, crash} 			-> exit(rpc);
		{Name, ok, Response} 	-> Response
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
			try Mod:handle(Request, OldState) of
				{Response, NewState} ->
					From ! {Name, ok, Response},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					log_the_error(Name, Request, Why),
					%% send a message to cause the client to crash
					From ! {Name, crash},
					%% loop with the original state
					loop(Name, Mod, OldState)
			end
	end.

log_the_error(Name, Request, Why) ->
	io:format("Server ~p request ~p~n"
			 "caused exception ~p~n",
			 [Name, Request, Why]).
