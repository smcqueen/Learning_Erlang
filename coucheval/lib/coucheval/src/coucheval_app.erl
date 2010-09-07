%%%----------------------------------------------------------------
%%% @author Stan McQueen <stan.mcqueen@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Stan McQueen
%%%----------------------------------------------------------------,
-module(coucheval_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, startup/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    couchbeam:start(),
    Db = couchbeam_server:start_connection_link(),
    io:format("~p~n", [couchbeam_server:info(Db)]),
    Coucheval = couchbeam_server:create_db(Db, "coucheval"),
    io:format("~p~n", [couchbeam_server:info(Coucheval)]),
    DesignDoc = {[
         {<<"_id">>, <<"_design/couchbeam">>},
         {<<"language">>,<<"javascript">>},
         {<<"views">>,
             {[{<<"test">>,
                 {[{<<"map">>,
                     <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
                 }]}
             }]}
         }]},
    Doc1 = {[{<<"_id">>,<<"test">>},{<<"type">>,<<"test">>}]},
    Doc2 = {[{<<"_id">>,<<"test2">>},{<<"type">>,<<"test">>}]},
    Doc3 = {[{<<"_id">>,<<"test3">>},{<<"type">>,<<"test">>}]},
    couchbeam_db:save_doc(Coucheval, DesignDoc),
    couchbeam_db:save_doc(Coucheval, Doc1),
    couchbeam_db:save_doc(Coucheval, Doc2),
    couchbeam_db:save_doc(Coucheval, Doc3),
    View = couchbeam_db:view(Coucheval, "_design/couchbeam/test").
    
%    couchbeam_server:delete(Db, "coucheval"),
%    case coucheval_sup:start_link() of
%        {ok, Pid} ->
%            {ok, Pid};
%        Error ->
%            Error
%    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

startup() ->
    couchbeam:start(),
    Db = couchbeam_server:start_connection_link(),
    io:format("~p~n", [couchbeam_server:info(Db)]),
    Coucheval = couchbeam_server:open_db(Db, "coucheval"),
    io:format("~p~n", [couchbeam_server:info(Coucheval)]),
    View = couchbeam_db:view(Coucheval, "couchbeam/test"),
    io:format("~p~n", [couchbeam_view:count(View)]),
    io:format("~p~n", [couchbeam_view:fetch(View)]).

temp() ->
    Db = couchbeam_server:start_connection_link(),
    DesignDoc = {[
         {<<"_id">>, <<"_design/couchbeam">>},
         {<<"language">>,<<"javascript">>},
         {<<"views">>,
             {[{<<"test">>,
                 {[{<<"map">>,
                     <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
                 }]}
             }]}
         }]},
Doc1 = {[{<<"_id">>,<<"test">>},{<<"type">>,<<"test">>}]},
Doc2 = {[{<<"_id">>,<<"test2">>},{<<"type">>,<<"test">>}]},
Doc3 = {[{<<"_id">>,<<"test3">>},{<<"type">>,<<"test">>}]},
    couchbeam_db:save_doc(Db, DesignDoc),
    couchbeam_db:save_doc(Db, Doc1),
    couchbeam_db:save_doc(Db, Doc2),
    couchbeam_db:save_doc(Db, Doc3).
