%% Author: smcqueen
%% Created: Aug 2, 2010
%% Description: TODO: Add description to motor_controller
-module(motor_controller).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add_event_handler/0]).

%%
%% API Functions
%%

add_event_handler() ->
	event_handler:add_handler(errors, fun controller/1).

%%
%% Local Functions
%%

controller(too_hot) ->
	io:format("Turn off the motor~n");
controller(X) ->
	io:format("~w ignored event : ~p~n", [?MODULE, X]).