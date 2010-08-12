%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@gandalf>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 10 Aug 2010 by Stan McQueen <smcqueen@gandalf>
%%%-------------------------------------------------------------------
-module(create_tables).

-export([init_tables/0]).

-record(user, {id, name}).

-record(project, {title, description}).

-record(contributor, {user_id, project_title}).

init_tables() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
    mnesia:create_table(project, [{attributes, record_info(fields, project)}]),
    mnesia:create_table(contributor, [{attributes, record_info(fields, contributor)}]),
    mnesia:info().
