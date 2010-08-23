%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@gandalf>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 22 Aug 2010 by Stan McQueen <smcqueen@gandalf>
%%%-------------------------------------------------------------------
-module(gws_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-define(SERVER, ?MODULE).

-record(state {lsock, socket, request_line, headers=[],
	       body = <<>>, content_remaining=0,
	       callback, user_data, parent}).

%%--------------------------------------------------------------------
%% API


