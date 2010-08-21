%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2010 by Stan McQueen <smcqueen@ubuntu910.bluffdale.iaccess.com>
%%%-------------------------------------------------------------------
-module(ti_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-record(state, {lsock}).

%%--------------------------------------------------------------------
%% @doc
%% The start_link/1 function #1 is how the supervisor starts each
%% handler process, passing on the listening socket. This gets
%% propagated via gen_server:start_link/3 to the gen_server callback
%% init/1
%% @spec
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%--------------------------------------------------------------------
%% @doc
%% Stores the socket in the server state and then returns signaling a
%% timeout of 0, using the same trick as in chapter 3 of "Erlang and
%% OTP in Action" (see listings 3.4 and 3.5) to finish the startup
%% without keeping the caller of init/1 waiting: the zero timeout
%% makes the new gen_server process drop immediately into the
%% timeout clause of handle_info/2
%% @spec
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @doc
%% At this point, the handler process has detached from the process
%% that called ti_server:start_link/1 and is running concurrently with
%% any previously started handlers that haven’t already finished.
%% The handler immediately calls gen_tcp:accept/1 on the listening
%% socket, which will block until the next incoming TCP connection.
%% (It’s because of this blocking call that you need to ensure that
%% nobody else is currently waiting for this process, or you’d be
%% holding them up as well.)
%%
%% When accept() returns (this could happen almost immediately,
%% on a heavily loaded server, or after many months if the interface
%% is rarely used), the first thing to do is to ask the supervisor
%% to start another handler by calling ti_sup:start_child().
%% The new handler—a clone of this one—will immediately start waiting
%% for the next connection on the listening socket, while the current
%% handler process can get on with handling the connection that it
%% has accepted.
%%
%% Because the listening socket was opened in active mode
%% (see ti_app.erl), and because the dedicated socket returned by
%% accept() will inherit this setting, all incoming data on the
%% dedicated socket will be sent directly and automatically to the
%% handler process as a message on the form {tcp, Socket,RawData}.
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    ti_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
%%--------------------------------------------------------------------
%% @doc
%% First, the incoming string is split at the first [ character.
%% If the input does contain a [, this will be the first character in
%% the RawArgList part, otherwise RawArgList will be empty (and parsing
%% will fail later). The Function variable should contain the function
%% name, that is, everything to the left of the [ character.
%%
%% The RawArgList half of the string should look like a normal Erlang
%% list (according to the protocol you defined in the previous section).
%% This means that it can be passed through the Erlang tokenizer
%% erl_scan (after appending a period character), producing a list of
%% tokens. The token list can then be passed to the Erlang parser
%% erl_parse, which should result in the real argument list as a single
%% Erlang term. This makes it easy to use the apply/3 BIF for making a
%% dynamic function call to the named function in the simple_cache module.
%% Finally, the result is written back on the TCP socket for the client
%% to read.

%% @spec
%% Protocol:
%% Call -> Function ArgList
%% Function -> "insert" | "lookup" | "delete"
%% ArgList -> "[" "]" | "[" Terms "]"
%% Terms -> Term | Term "," Terms

%% @end
%%--------------------------------------------------------------------
handle_data(Socket, RawData, State) ->
    try
	{Function, RawArgList} = lists:splitwith(fun (C) -> C =/= $[ end, RawData),
	io:format("Function = ~p~nRawArgList = ~p~n", [Function, RawArgList]),
	{ok, Toks, _Line} = erl_scan:string(RawArgList ++ ".", 1),
	io:format("Toks = ~p~n", [Toks]),
	{ok, Args} = erl_parse:parse_term(Toks),
	Result = apply(simple_cache, list_to_atom(Function), Args),
	gen_tcp:send(Socket, io_lib:fwrite("OK:~p.~n", [Result]))
    catch
	_Class:Err ->
	    gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
    end,
    State.
