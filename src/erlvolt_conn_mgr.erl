%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_conn_mgr.erl                                      %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver connection manager                   %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 13 Apr 2012                                               %%%
%%% Changed     : 02 Feb 2013                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   This driver is being contributed to VoltDB by Eonblast Corporation.   %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%    Erlvolt 0.3/beta    - Erlang VoltDB client API.                      %%%
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

-module(erlvolt_conn_mgr).
-behaviour(gen_server).

-vsn("0.3/beta").
-author("H. Diedrich <hd2012@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([test/0, add_pool/1, drain_pool/2, close_pool/3, pools/0,
        add_connections/2,
        get_connections/1, get_connection/1,
        get_slot/1, get_slot_blocking/2, create_slot_warrant/1,
        pass_slot/1, replace_connection/2]).

-include("../include/erlvolt.hrl").

-record(state, {pools}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @spec start_link() -> 'ignore' | {'error',any()} | {'ok',pid()}
start_link() ->
    ?TRACE("#3   erlvolt_conn_mgr:start_link/0"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec pools() -> any()
pools() ->
    gen_server:call(?MODULE, pools, infinity).

%% @spec add_pool(any()) -> any()
add_pool(Pool) ->
    call({add_pool, Pool}).

%% @spec test() -> any()
test() ->
    gen_server:call(?MODULE, test, infinity),
    call(test).

%% @spec drain_pool(any(), integer()) -> list()
drain_pool(PoolId, DrainWait) ->
    call({drain_pool, PoolId, self(), DrainWait}),
    % block calling process until drained.
    receive
        R -> R
    end.

%% @spec close_pool(any(), integer(), integer()) -> list()
close_pool(PoolId, DrainWait, CloseWait) ->
    call({close_pool, PoolId, self(), DrainWait, CloseWait}),
    % block calling process until closed.
    receive
        R -> R
    end.

%% @spec add_connections(any(),maybe_improper_list()) -> any()
add_connections(PoolId, Conns) when is_list(Conns) ->
    call({add_connections, PoolId, Conns}).

%% @spec get_connection(any()) -> any()
get_connection(PoolId) ->
    call({get_connection, PoolId}).

%% @spec get_connections(any()) -> any()
get_connections(PoolId) ->
    call({get_connections, PoolId}).

%% @doc
%% returns slot() | unavailable | {error, pool_not_found}
%% slot() is a slot warrant, a record describing a slot.
%% @spec get_slot(any()) -> any()
get_slot(PoolId)->
    call({get_slot, PoolId}).

%% @doc
%% returns slot() | {error, pool_not_found}
%% slot() is a slot warrant, a record describing a slot.
%% @spec get_slot_blocking(any()) -> any()
get_slot_blocking(PoolId, QueueTimeout)->
    ?TRACE("#11   erlvolt_conn_mgr:get_slot_blocking/1"),
    
    case call({get_slot_or_queue, PoolId}) of
        queued ->
            
            ?ERLVOLT_PROFILER_COUNT_QUEUED(),
            QueueTime = now(),
            ?TRACE("~p is queued", [self()]),
            receive
                {slot,Slot} when is_record(Slot, erlvolt_slot) ->
                    
                    _T = timer:now_diff(now(), QueueTime),
                    ?TRACE("~p gets a slot after waiting (~s)µs in queue", [self(),erlvolt_profiler:ts(_T)]),
                    ?ERLVOLT_PROFILER_COUNT_UNQUEUED(_T),
                    Slot;
                Other ->
                    
                    ?TRACE("~p gets something other than a slot while waiting in queue: ~w", [self(), _Other]),
                    _T = timer:now_diff(now(), QueueTime),
                    ?ERLVOLT_PROFILER_COUNT_UNQUEUED(_T),
                    ?ERLVOLT_PROFILER_COUNT_PENDING(),
                    ?ERLVOLT_PROFILER_COUNT_FAILURE(),
                    exit({undue_message_received, Other})
                after QueueTimeout ->
                    
                    ?TRACE("~p gets no connection and times out after ~sµs-> probably EXIT", [self(),erlvolt_profiler:ts(T)]),
                        _T = timer:now_diff(now(), QueueTime),
                    ?ERLVOLT_PROFILER_COUNT_UNQUEUED(_T),
                    ?ERLVOLT_PROFILER_COUNT_PENDING(),
                    ?ERLVOLT_PROFILER_COUNT_FAILURE(),
                    call({unqueue, PoolId}),
                    % This is to avert a race condition where the
                    % message might be sent after timeout was initiated.
                    receive
                        {slot,Slot} when is_record(Slot, erlvolt_slot) ->
                             
                            _T2 = timer:now_diff(now(), QueueTime),
                            ?TRACE("~p gets a slot last minute after waiting (~s)µs in queue", [self(),erlvolt_profiler:ts(_T)]),
                            ?ERLVOLT_PROFILER_COUNT_UNQUEUED(_T2),
                            Slot;
                        _Other ->
                            
                            ?TRACE("~p gets something other than a slot AFTER waiting in queue: ~w", [self(), _Other]),
                            _T2 = timer:now_diff(now(), QueueTime),
                            ?ERLVOLT_PROFILER_COUNT_UNQUEUED(_T2),
                            ?ERLVOLT_PROFILER_COUNT_PENDING(),
                            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
                            exit(undue_message_received_after_queue)
                        after 0 ->
                            % Only now are we sure that the wait failed.
                            ?ERLVOLT_PROFILER_COUNT_PENDING(),
                            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
                            queue_timeout
                    end
            end;

        queue_full ->
            ?ERLVOLT_PROFILER_COUNT_PENDING(),
            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
            queue_full;
        {error, What} ->
            
            {error, What};
        Slot ->
            
            Slot
    end.

%% @spec pass_slot(any()) -> any()
pass_slot(Slot)->
    call({pass_slot, Slot}).

%% @spec replace_connection(any(),any()) -> any()
replace_connection(OldConn, NewConn) ->
    call({replace_connection, OldConn, NewConn}).

%% Throw errors that happened inside the gen server.
%% They should not be thrown in the gen server to no
%% crash it.
%% @spec call('test' | {'add_pool' | 'close_pool' | 'get_connection' | 'get_connections' | 'get_slot' | 'get_slot_or_queue' | 'pass_slot' | 'unqueue',any()} | {'add_connections',any(),maybe_improper_list()} | {'close_connections',any(),integer()} | {'replace_connection',any(),any()}) -> any()
call(Msg) ->
    case gen_server:call(?MODULE, Msg, infinity) of
        {error, Reason} ->
            exit(Reason);
        Result ->
            Result
    end.

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
init([]) -> % 
    ?TRACE("#3a  erlvolt_conn_mgr:init/1"),
    {ok, #state{pools=[]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @spec handle_call(any(),any(),any()) -> {{'error','pool_not_found'},#state{}} | {'reply',any(),any()}
handle_call(test, _, State) ->
    {reply, ok, State};

handle_call(pools, _From, State) ->
    {reply, State#state.pools, State};

handle_call({add_pool, Pool}, _From, State) ->
    ?TRACE("#9   erlvolt_conn_mgr:handle_call {add_pool..} - puts pool into mgr's state"),
    case extract_pool(Pool#pool.pool_id, State#state.pools) of
        {_, _} ->
            {reply, {error, pool_already_exists}, State};
        %% the OK case:
        missing ->
            {reply, ok, State#state{pools = [Pool|State#state.pools]}}
    end;

handle_call({drain_pool, PoolId, ReportTo, DrainWait}, _From, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, _} ->
            Conns = queue:to_list(Pool#pool.available),
            Blocker = spawn(fun() -> ReportTo ! (catch [ receive R -> R end || _ <- Conns ]) end),
            [ spawn(fun() -> Blocker ! erlvolt_conn:drain_connection(Conn, DrainWait) end) || Conn <- Conns ],
            {reply, draining, State};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({close_pool, PoolId, ReportTo, DrainWait, CloseWait}, _From, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            % At this point the pool is out of the main Pool list and cannot be found
            % anymore by other functions. Also, we are in the gen server process here.
            [erlvolt_conn:blunt_waiting(Pid) || Pid <- queue:to_list(Pool#pool.waiting)],
            Conns = queue:to_list(Pool#pool.available),
            Blocker = spawn(fun() -> ReportTo ! (catch [ receive R -> R end || _ <- Conns ]) end),
            [ spawn(fun() -> Blocker ! erlvolt_conn:close_connection(Conn, DrainWait, CloseWait) end) || Conn <- Conns ],
            {reply, closing, State#state{pools=OtherPools}};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({add_connections, PoolId, Conns}, _From, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            OtherConns = Pool#pool.available,
            State1 = State#state{
                pools = [Pool#pool{available = queue:join(queue:from_list(Conns), OtherConns)}|OtherPools]
            },
            {reply, ok, State1};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({get_connection, PoolId}, _From, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, _OtherPools} ->
            {{value, C},_} = queue:out(Pool#pool.available), % 
            {reply, C, State};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({get_connections, PoolId}, _From, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, _OtherPools} ->
            {reply, queue:to_list(Pool#pool.available), State};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

%% find the next available connection in the pool identified by PoolId
%% returns slot() | unavailable | {error, pool_not_found}
handle_call({get_slot, PoolId}, {_From, _Mref}, State) ->
    
    case find_free_slot(State, PoolId) of
        {Slot, State1} when is_record(Slot, erlvolt_slot) ->
            {reply, Slot, State1};
        {Error, State1} ->
            ?ERLVOLT_PROFILER_COUNT_PENDING(),
            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
            {reply, Error, State1}

            % note: Slot can be slot() | unavailable | {error, pool_not_found}
    end;

%% It's necessary to have the queuing happen inside a gen server call.
%% Therefore, get_slot and get_slot_or_queue are in two.
%% returns slot() | queued | {error, pool_not_found}
handle_call({get_slot_or_queue, PoolId}, {From, _Mref}, State) ->
    case find_free_slot(State, PoolId) of
        {unavailable, _} ->
            % queue the process with the pool wait queue
            {Pool, OtherPools} = extract_pool(PoolId, State#state.pools),
            MaxQ = Pool#pool.queue_size,
            case queue:len(Pool#pool.waiting) < MaxQ of
                true ->
                    %% QUEUE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    Pool1 = Pool#pool{ waiting = queue:in(From, Pool#pool.waiting) },
                    %% QUEUE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    {reply, queued, State#state{pools=[Pool1|OtherPools]}};
                false ->
                    {reply, queue_full, State}
            end;
        {Slot, State1} ->
            {reply, Slot, State1}
            % note: Slot can be slot() | {error, pool_not_found}
    end;

%% This prevents a rare race condition: that the receive time out
%% happens right the moment that a message is sent to the waiting process.
%% To deal with contention, this has to be done in the gen_server.
handle_call({unqueue, PoolId}, {From, _Mref}, State) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            %% Remove From from the wait queue
            QueueNow = queue:filter(
                fun(Pid) -> Pid =/= From end,
                Pool#pool.waiting),
            PoolNow = Pool#pool{ waiting = QueueNow },
            {reply, ok, State#state{pools=[PoolNow|OtherPools]}};
        missing ->
            {reply, {error, pool_not_found}, State}
    end;

%% "Give a slot back" after using it
handle_call({pass_slot, Slot}, _From, State) ->

    PoolId = Slot#erlvolt_slot.pool_id,
    ConnId = Slot#erlvolt_slot.connection_id,

    % get the pool that this connection belongs to
    case extract_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            %% check if any processes are waiting for a connection
            case queue:out(Pool#pool.waiting) of

                %% if wait queue is empty, "give the slot back"
                %% --------------------------------------------
                {empty,_} ->
                    %% find connection in available queue
                    case extract_connection(ConnId, Pool#pool.available) of
                        {Conn, Front, Rear} ->
                            %% simply decrement the counter
                            Conn1 = Conn#erlvolt_connection{ pending = Conn#erlvolt_connection.pending - 1},
                            %% pack back into the state
                            Queue1 = queue:join(queue:in(Conn1,Front),Rear),
                            Pool1  = Pool#pool{ available = Queue1 },
                            State1 = State#state{ pools = [Pool1|OtherPools] },
                            {reply, ok, State1};
                        missing ->
                            % 
                            {reply, connection_not_found, State}
                    end;

                %% if wait queue is populated, give the slot on
                %% --------------------------------------------
                {{value, Pid}, OtherWaiting} ->
                    %% remove the head of the queue and send it the slot.
                    %% Update the pool & queue in state once the head has been removed.
                    Pool1 = Pool#pool{ waiting = OtherWaiting },
                    State1 = State#state{ pools = [Pool1|OtherPools] },
                    erlang:send(Pid, {slot, Slot}),
                    {reply, ok, State1}
            end;
        %% pool not found
        missing ->
            {{error, pool_not_found}, State}
    end;

handle_call({replace_connection, _OldConn, _NewConn}, _From, _State) ->
    exit(not_implemented),
    ok;

handle_call(_, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @spec handle_cast(any(),any()) -> {'noreply',any()}
handle_cast(_Msg, State) ->
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
%%% Internal functions
%%--------------------------------------------------------------------

%% @private find and return a pool by pool id, taking it out of the list
%% @spec extract_pool(any(),maybe_improper_list()) -> 'missing' | {#pool{},any()}
extract_pool(PoolId, Pools) ->
    extract_pool(PoolId, Pools, []).

%% @spec extract_pool(any(),maybe_improper_list(),[any()]) -> 'missing' | {#pool{},any()}
extract_pool(_, [], _) -> missing;

extract_pool(PoolId, [#pool{pool_id = PoolId} = Pool|Tail], OtherPools) ->
    {Pool, lists:append(OtherPools, Tail)};

extract_pool(PoolId, [Pool|Tail], OtherPools) ->
    extract_pool(PoolId, Tail, [Pool|OtherPools]).

%% @private find a connection by connection id, taking it out of the queue
%% return {Conn, QueueBefore, QueueAfter} | missing
%% @spec extract_connection(any(),queue()) -> 'missing' | {#erlvolt_connection{},queue(),queue()}
extract_connection(Id, Q) ->
    extract_connection(Id, Q, queue:new()).

%% @spec extract_connection(any(),queue(),queue()) -> 'missing' | {#erlvolt_connection{},queue(),queue()}
extract_connection(Id, Unseen, Seen) ->
    case queue:out(Unseen) of
        {{value, #erlvolt_connection{id=Id}=Conn}, Remaining} ->
            {Conn, Seen, Remaining};
        {{value, Conn}, Remaining} ->
            extract_connection(Id, Remaining, queue:in(Conn, Seen));
        {empty, _} ->
            missing
    end.

%% @private get a free slot from this pool, by pool id.
%% be sure that the pool exists and splice it back in the state after.
%% -> slot() | unavailable | {error,pool_not_found}
%% @spec find_free_slot(#state{},any()) -> {'unavailable' | {'error','pool_not_found'} | #erlvolt_slot{}, #state{}}
find_free_slot(State, PoolId) when is_record(State, state) ->
    case extract_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            case find_free_slot(Pool) of
                {Slot, Pool1} ->
                    {Slot, State#state{ pools = [Pool1|OtherPools]}}
                    % note, Slot can be "unavailable".
            end;
        % bad pool
        missing ->
            {{error, pool_not_found}, State}
    end.

%% @private get a free slot from the given pool.
%% @spec find_free_slot(#pool{}) -> {'unavailable' | #erlvolt_slot{}, #pool{}}
find_free_slot(Pool) when is_record(Pool, pool) ->
    {Slot, ConnQ} = find_free_slot_round_robin(Pool#pool.available, queue:new()),
    {Slot, Pool#pool{available = ConnQ}}.
    % note, Slot can be "unavailable".

%% @private round robin connection search through the pool for an alive connection with a free slot.
%% @spec find_free_slot_round_robin(queue(),queue()) -> {'unavailable' | #erlvolt_slot{}, queue()}
find_free_slot_round_robin(Next, Seen) ->
    
    
    case queue:out(Next) of
        {{value, Conn}, Unseen} ->
            case Conn#erlvolt_connection.alive and % 
                 (Conn#erlvolt_connection.pending < Conn#erlvolt_connection.slots) of
                true ->
                    % ok: found a connection that has open slots
                    
                    Slot = create_slot_warrant(Conn),
                    Conn1 = Conn#erlvolt_connection{ pending = Conn#erlvolt_connection.pending + 1},
                    ConnQ = queue:in(Conn1, queue:join(Unseen, Seen)),
                    {Slot, ConnQ};
                _ ->
                    % full / dead
                    
                    find_free_slot_round_robin(Unseen, queue:in(Conn, Seen))
            end;
        {empty, _} ->
            {unavailable, Seen}
    end.

%% @private make a record that describes a slot.
%% The slots are a virtual count. A slot warrant is not really the slot itself but
%% symbolizes your hold on it.
%% @spec create_slot_warrant(#erlvolt_connection{}) -> #erlvolt_slot{}
create_slot_warrant(C) when is_record(C, erlvolt_connection) ->

    Now = erlang:now(),
    Free = C#erlvolt_connection.slots - C#erlvolt_connection.pending,
    #erlvolt_slot{
        id = make_id(Now),
        connection_id = C#erlvolt_connection.id,
        connection_pid = C#erlvolt_connection.pid,
        pool_id = C#erlvolt_connection.pool_id,
        granted = Now,   % debug info
        left = Free,     % debug info
        sent = false,    % ?
        pending = false, % ?
        done = false
    }.

%% @spec make_id({non_neg_integer(),non_neg_integer(),non_neg_integer()}) -> <<_:96>>
make_id(Now) -> % this can save one call of now() which may reduce time skew one day.
    {MegaSecs, Secs, MicroSecs} = Now,
    <<MegaSecs:32, Secs:32, MicroSecs:32>>.
