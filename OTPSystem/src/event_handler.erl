%% Author: smcqueen
%% Created: Aug 2, 2010
%% Description: TODO: Add description to event_handler
-module(event_handler).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([event/2, add_handler/2, make/1]).

%%
%% API Functions
%%

make(Name) -> 
	register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

add_handler(Name, Fun) -> Name ! {add, Fun}.

%% generate an event
event(Name, X) -> Name ! {event, X}.


%%
%% Local Functions
%%

my_handler(Fun) ->
	receive
		{add, Fun1} ->
			my_handler(Fun1);
		{event, Any} ->
			(catch Fun(Any)),
			my_handler(Fun)
	end.

no_op(_) -> void.