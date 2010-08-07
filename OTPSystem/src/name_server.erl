%% Author: smcqueen
%% Created: Aug 4, 2010
%% Description: TODO: Add description to name_server
-module(name_server).
-import(server2, [rpc/2]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, whereis/1, add/2, init/0]).

%%
%% Client Routines
%%

add(Name, Place) 	-> rpc(name_server, {add, Name, Place}).
whereis(Name)		-> rpc(name_server, {whereis, Name}).

%%
%% Callback Routines
%%

init()	-> dict:new().

handle({add, Name, Place}, Dict)	-> {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict)		-> {dict:find(Name, Dict), Dict}.