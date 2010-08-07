%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to server5
-module(server5).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([rpc/2, start/0]).

%%
%% API Functions
%%

start()	->	spawn(fun()	->	wait() end).

rpc(Pid, Q)	->
	Pid ! {self(), Q},
	receive
		{Pid, Reply}	->	Reply
	end.

%%
%% Local Functions
%%

wait()	->
	receive
		{become, F}	->	F()
	end.