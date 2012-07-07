%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_profiler.erl                                      %%%
%%% Version     : 0.3.0/beta                                                %%%
%%% Description : Erlang VoltDB driver query profiler                       %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 13 Jan 2013                                               %%%
%%% Changed     : 02 Feb 2013                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   This driver is being contributed to VoltDB by Eonblast Corporation.   %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%    Erlvolt 0.3.0/alpha - Erlang VoltDB client API.                      %%%
%%%                                                                         %%%
%%%    This file is part of VoltDB.                                         %%%
%%%    Copyright (C) 2008-2013 VoltDB, LLC http://www.voltdb.com            %%%
%%%    Author H. Diedrich <hd2012@eonblast.com> http://www.eonblast.com     %%%
%%%                                                                         %%%
%%% Permission is hereby granted, free of charge,  to any person obtaining  %%%
%%% a copy  of this  software  and  associated  documentation  files  (the  %%%
%%% "Software"),  to deal in the  Software without restriction,  including  %%%
%%% without limitation the rights to use,  copy,  modify,  merge, publish,  %%%
%%% distribute,  sublicense,  and/or sell copies of the  Software,  and to  %%%
%%% permit persons to whom the Software is furnished to do so,  subject to  %%%
%%% the following conditions:                                               %%%
%%%                                                                         %%%
%%% The  above  copyright  notice  and  this  permission  notice  shall be  %%%
%%% included in all copies or substantial portions of the Software.         %%%
%%%                                                                         %%%
%%% THE  SOFTWARE  IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY  OF ANY KIND,  %%%
%%% EXPRESS OR IMPLIED,  INCLUDING  BUT  NOT LIMITED  TO THE WARRANTIES OF  %%%
%%% MERCHANTABILITY, FITNESS  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. %%%
%%% IN NO EVENT SHALL  THE AUTHORS  BE LIABLE  FOR  ANY CLAIM,  DAMAGES OR  %%%
%%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT  OR OTHERWISE,  %%%
%%% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR  %%%
%%% OTHER DEALINGS IN THE SOFTWARE.                                         %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% USAGE                                                                   %%%
%%%                                                                         %%%
%%% You can run a sample using the 'Hello' tutorial-server discussed        %%%
%%% in the VoltDB manual and present in every VoltDB distribution.          %%%
%%%                                                                         %%%
%%% Start that server from your voltdb installation with:                   %%%
%%%                                                                         %%%
%%%     $ cd voltdb/doc/tutorial/helloworld                                 %%%
%%%     $ ./run.sh                                                          %%%
%%%                                                                         %%%
%%% Then run the hello world example, using make from the driver root:      %%%
%%%                                                                         %%%
%%%     $ make hello                                                        %%%
%%% or                                                                      %%%
%%%     $ make                                                              %%%
%%%     $ cd examples                                                       %%%
%%%     $ erlc -I ../include -o ../ebin +debug_info hello_plus.erl          %%%
%%%     $ erl -pa ../ebin -s hello_plus run -s init stop -noshell           %%%
%%%                                                                         %%%
%%% You will see this response, 'Hello, world!' in Swedish:                 %%%
%%%                                                                         %%%
%%%     Hej världen!                                                        %%%
%%%                                                                         %%%
%%% The hello world source is found in examples/hello_plus.erl              %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% See README.md or .html for instructions,  examples/ for more examples.  %%%
%%% See doc/BENCHMARKS.md or .html for a description of driver benchmarks.  %%%
%%%                                                                         %%%
%%% For getting started with VoltDB,see: voltdb/doc/GettingStarted.pdf or   %%%
%%% online:  http://voltdb.com/downloads/documentation/GettingStarted.pdf.  %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(erlvolt_profiler).
-behaviour(gen_server).

-vsn("0.3.0/beta").
-author("H. Diedrich <hd2012@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3.0/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([reset/0,test/0,ts/1,abri/1,dummy/0,dummy/1]).
-ifdef(profile).
-export([pending/0, queued/0,
        count_pending/0, count_success/0, count_success/1, count_failure/0, count_failure/1,
        count_queued/0, count_unqueued/0, count_unqueued/1,
        dump/1, waitpending/1, waitqueued/1]).
-endif.

-include("../include/erlvolt.hrl").


% -record(state, {now,lap}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @spec start_link() -> 'ignore' | {'error',any()} | {'ok',pid()}
start_link() ->
    ?TRACE("#3   erlvolt_profiler:start_link/0"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec test() -> any()
test() ->
    gen_server:call(?MODULE, test, infinity),
    call(test).

-ifdef(profile).

%% @spec reset() -> ok
reset() ->
    cast(reset).

%% @spec count_pending() -> ok
count_pending() ->
    cast({count_pending}).

%% @spec count_success() -> ok
count_success() ->
    cast({count_success}).

%% @spec count_success(T::time()) -> ok
count_success(T) ->
    cast({count_success,T}).

%% @spec count_failure() -> ok
count_failure() ->
    cast({count_failure}).

%% @spec count_failure(T::time()) -> ok
count_failure(T) ->
    cast({count_failure,T}).

%% @spec count_queued() -> ok
count_queued() ->
    cast({count_queued}).

%% @spec count_unqueued() -> ok
count_unqueued() ->
    cast({count_unqueued}).

%% @spec count_unqueued(T::time()) -> ok
count_unqueued(T) ->
    cast({count_unqueued,T}).

%% @spec dump(any()) -> ok
dump(ClientID) ->
    cast({dump, ClientID}).

%% @spec pending() -> integer()
pending() ->
    call({pending}).

%% @spec queued() -> integer()
queued() ->
    call({queued}).

-else.

%% @doc reset function dummy when profiler is not compiled in
%% @spec reset() -> ok
reset() ->
    ok.

-endif.

%% @spec dummy() -> ok
dummy() ->
    io:format("."),
    ok.

%% @spec dummy(any()) -> ok
dummy(_) ->
    io:format("."),
    ok.

%% the stateful loop functions of the gen_server never
%% want to call exit/1 because it would crash the gen_server.
%% instead we want to return error tuples and then throw
%% the error once outside of the gen_server process
%% @spec call(Msg::tuple()) -> ok | integer()
call(Msg) ->
    case gen_server:call(?MODULE, Msg, infinity) of
        {error, Reason} ->
            exit(Reason);
        Result ->
            Result
    end.

-ifdef(profile).
%% @spec cast(Msg::tuple()) -> ok | integer()
cast(Msg) ->
    gen_server:cast(?MODULE, Msg).
-endif.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%% @spec init([]) -> {'ok',#state{}}
init([]) ->
    ?TRACE("#3a  erlvolt_profiler:init/1"),
    {ok, new_state()}.

new_state() ->
    {#erlvolt_profile{t0=now()}, #erlvolt_profile{t0=now()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @spec handle_call(any(),any(),any()) -> {'reply',any(),any()}
handle_call(test, _, State) ->
    {reply, ok, State};

handle_call({pending}, _From, {L0,_}=State) ->
    {reply, L0#erlvolt_profile.x, State};

handle_call({queued}, _From, {L0,_}=State) ->
    {reply, L0#erlvolt_profile.q, State};

handle_call(Msg, From, State) ->
    io:format("Unhandled call from ~p to profiler: ~p~n", [From, Msg]),
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @spec handle_cast(any(),{#profile{},#profile{}}) -> {'noreply',{#profile{},#profile{}}}
handle_cast({dump, ClientID}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},{erlvolt_profile,_,T1,_,C1,_,_,_,_,_,_,_}}) ->

    Tn = now(),
    TD = trunc(timer:now_diff(Tn,T0) / 1000),
    TDs = TD / 1000,
    A = trunc(C/TD*1000),
    TD1 = timer:now_diff(Tn,T1), % µs
    A1 = if TD1 > 0 -> trunc((C-C1)/TD1*1000000); true -> 0 end,
    ALc = if AL == nil -> "n/a"; S > 0 ->  try io_lib:format("~6.3fms", [AL / S]) catch _:_ -> "n/A" end; true -> "N/a" end,
    QLm = trunc(QL / 1000),
    if (QLm > 0); (Q > 0) ->
        io:format("Client ~5s: at ~6.3fsec: lap ~8s T/sec,  total ~8s T/sec, success: ~7s, fails: ~s, pending: ~7s, avglat: ~s, maxlat: ~4Bms, queued: ~5s, maxwait: ~5sms~n",
            [ClientID, TDs, ts(A1), ts(A), ts(S), ts(E), ts(X), ALc, XL, ts(Q), ts(QLm)]);
    true ->
        io:format("Client ~5s: at ~6.3fsec: lap ~8s T/sec,  total ~8s T/sec, success: ~7s, fails: ~s, pending: ~7s, avglat: ~s, maxlat: ~4Bms~n",
            [ClientID, TDs, ts(A1), ts(A), ts(S), ts(E), ts(X), ALc, XL])
    end,

    {noreply, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,0,Q,0},{erlvolt_profile,P,Tn,N,C,S,E,X,0,0,Q,0}}};

handle_cast(reset, _) ->
    {noreply, new_state()};

handle_cast({count_pending}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C,S,E,X+1,AL,XL,Q,QL},L1}};

handle_cast({count_success}, {{erlvolt_profile,P,T0,N,C,S,E,X,_AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C+1,S+1,E,X-1,nil,XL,Q,QL},L1}};

handle_cast({count_success, Waited}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C+1,S+1,E,X-1,if is_number(AL) -> AL+Waited; true -> nil end,max(XL,Waited),Q,QL},L1}};

handle_cast({count_failure}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C+1,S,E+1,X-1,AL,XL,Q,QL},L1}};

handle_cast({count_queued}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q+1,QL},L1}};

handle_cast({count_unqueued}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q-1,QL},L1}};

handle_cast({count_unqueued, Waited}, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q,QL},L1}) ->
    {noreply, {{erlvolt_profile,P,T0,N,C,S,E,X,AL,XL,Q-1,max(QL,Waited)},L1}};

handle_cast(Msg, State) ->
    io:format("Unhandled cast to profiler: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @spec handle_info(any(),any()) -> {'noreply',any()}
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @spec terminate(any(),any()) -> 'ok'
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @spec code_change(any(),any(),any()) -> {'ok',any()}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Helper functions
%%--------------------------------------------------------------------

-ifdef(profile).

%% Wait until less than Target calls are queued up for sending to the server
%% @spec waitqueued(pid(),100) -> 'ok'.
waitqueued(Target) ->
    % io:format("queued: ~p~n", [erlvolt_profiler:queued()]),
    case erlvolt_profiler:queued() > Target of
        true ->
            timer:sleep(1),
            waitqueued(Target);
        false ->
            ok
    end.

%% Wait until less than Target responses are pending at the server
%% @spec waitpending(pid(),100) -> 'ok'.
waitpending(Target) ->
    % io:format("pending: ~p~n", [erlvolt_profiler:pending()]),
    case erlvolt_profiler:pending() > Target of
        true ->
            timer:sleep(1),
            waitpending(Target);
        false ->
            ok
    end.

-endif.

%% @private thousands separator
ts(N) when is_integer(N) ->
    L = integer_to_list(N),
    [ [D|case (I-2) rem 3 of 2 -> ","; _ -> "" end] ||  {I,D} <- lists:zip(lists:seq(length(L),1,-1), L)];

ts(X) ->
    X.


%% @private suppress trailing ".0"
abri(N) ->
    if N == trunc(N) ->
        integer_to_list(trunc(N));
    true ->
        float_to_list(N)
    end.
