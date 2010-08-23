%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@gandalf>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 22 Aug 2010 by Stan McQueen <smcqueen@gandalf>
%%%-------------------------------------------------------------------
-module(gws_server).

-export([start/0, init/1]).


start() ->
    spawn(gws_server, init, [self()]).

init(From) ->
    loop(From).

loop(From) ->
    receive
	_ ->
	    loop(From)
    end.
