%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to my_fac_server
-module(my_fac_server).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([loop/0]).

%%
%% API Functions
%%

loop()	->
	receive
		{From, {fac, N}}	->
			From ! {self(), fac(N)},
			loop();
		{become, Something}	->
			Something()
	end.

%%
%% Local Functions
%%

fac(0)	->	1;
fac(N)	->	N * fac(N-1).