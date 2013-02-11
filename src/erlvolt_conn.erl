%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_conn.erl                                          %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver connection module                    %%%
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

-module(erlvolt_conn).

-vsn("0.3/beta").
-author("H. Diedrich <hd2012@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

-export([
    start_link/1,
    start_connection/13,
    execute/7,
    open_connections/1,
    open_connections/2,
    open_connection/4,
    reset_connection/3,
    drain_connection/2,
    close_connection/3,
    blunt_waiting/1,
    get_conn_mgr/0
    ]).

-include("erlvolt.hrl").
-include("erlvolt_internal.hrl").

%% @private This is the start function of the supervised child process.
%% It watches the socket and handles the async writes and reads.
%% Answers from the VoltDB server must be read with priority because
%% the VoltDB server may close the connection, should it get the
%% impression that it is not listened to.
%:% -------------------------------------------------------------------
%:% On the role of a supervised processes start function, from
%:% the Erlang/OTP docs:
%:% StartFunc defines the function call used to start the child
%:% process. It should be a module-function-arguments tuple {M,F,A}
%:% used as apply(M,F,A). The start function must create and link
%:% to the child process, and should return {ok,Child} or
%:% {ok,Child,Info} where Child is the pid of the child process
%:% and Info an arbitrary term which is ignored by the supervisor.
%:% The start function can also return ignore if the child process
%:% for some reason cannot be started, in which case the child
%:% specification will be kept by the supervisor (unless it is a
%:% temporary child) but the non-existing child process will be
%:% ignored. If something goes wrong, the function may also return
%:% an error tuple {error,Error}.
%:% http://www.erlang.org/doc/man/supervisor.html
%:% -------------------------------------------------------------------
%% @spec start_link([any()]) -> {'ok',pid()}

start_link(Args) ->
    ?TRACE("#7   erlvolt_conn:start_link/1 - start a new dynamic connection child"),
    Pid = spawn_link(?MODULE, start_connection, Args),
    {ok, Pid}.

%% @private Establish the server connection and enter the permanent socket service loop.
%% This function is the entry function of a new connection handler process, created by
%% erlvolt_conn:start_link/1, above. %
start_connection(PoolId, ConnId, Host, Port, User, Password, Database, Nagle, SendBuffer, ReceiveBuffer, SendTimeout, BlockingOpen, BlockedPid) ->

    ?TRACE("#7a  erlvolt_conn:start_connection/6"),
    Sock =
        try
            S = erlvolt_wire:create_connection(Host, Port, User, Password, Database, Nagle, SendBuffer, ReceiveBuffer, SendTimeout, BlockingOpen),
            % Notify waiting blocked process
            case BlockingOpen of
                blocking ->
                    BlockedPid ! {self(), loggedin};
                _ ->
                    ok
            end,
            ?TRACE("#7b  erlvolt_wire:create_connection call completed"),
            S
        catch
            What:Why ->
                erlvolt:error("Can't connect to ~p:~p as ~p/~p~n~p", [Host, Port, User, Password, Why]),
                % Notify blocked user process
                case BlockingOpen of
                    blocking ->
                        BlockedPid ! {self(), failed, What, Why};
                    _ ->
                        ok
                end,
                timer:sleep(5000), % Delay supervised restart
                exit({connection_failed, What, Why})
        end,
    ?TRACE("#7c  erlvolt_conn:start_connection now entering blocking receive loop"),
    tx_loop({PoolId, ConnId, Sock, nil, infinity, nil, pid_encode_overhead()},{0,0}).

%% @private the permanent socket service loop of a connection. It receives orders from
%% within the driver to call remote procedures on the server; and accepts the server
%% responses back (VoltDB does not acknowledge transmissions, only sends results,
%% errors or capacity signals back). This loop is supervised and restarted if it crashes.
%% It is started exclusively by start_connection/6, above.
%% @spec tx_loop({atom(),atom(),inet:socket(),non_neg_integer(),non_neg_integer(),binary()}) -> 'closed' | 'ok'
tx_loop({PoolId, ConnId, Socket, Drainer, DrainWait, Proceed, Overhead}=Anchor,{CallNo, RespNo}=Counter) ->
    ?TRACE("#7d  erlvolt_conn:tx_loop/2 (re-)entering blocking receive loop"),
    receive

        % Send a procedure call. CallId needs only be locally unique
        % ----------------------------------------------------------
        {call, awaitsendack, ProcName, Args, CallerPid, Deliver} ->

            ?TRACE("#15  erlvolt_conn:tx_loop/2 (call) sends ~p w/args ~p", [ProcName,Args]),
            CallId = encode_pid(Deliver, Overhead),
            ?ERLVOLT_PROFILER_COUNT_PENDING(),
            % alternate: CallId = <<ConnId:32, CallNo:32>>, % 2^32 = ~4G*
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            SentAck = erlvolt_wire:call_procedure(ConnId, Socket, ProcName, Args, CallId),
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            % Note: SentAck is not from the server but only acks the sending went ok.
            case SentAck of
                ok ->
                    CallerPid ! ok;
                _ ->
                    CallerPid ! {send_error, SentAck, CallId, ProcName, Args} %  {error,Reason} but never 'ok'
            end,
            tx_loop(Anchor, {CallNo+1, RespNo});

        % Send a procedure call. CallId needs only be locally unique
        % ----------------------------------------------------------
        {call, fireandforget, ProcName, Args, CallerPid, Deliver} ->

            ?TRACE("#15B  erlvolt_conn:tx_loop/2 (cast) sends args ~w", [Args]),
            CallId = encode_pid(Deliver, Overhead),
            ?ERLVOLT_PROFILER_COUNT_PENDING(),
            % alternate: CallId = <<ConnId:32, CallNo:32>>, % 2^32 = ~4G*
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            SentAck = erlvolt_wire:call_procedure(ConnId, Socket, ProcName, Args, CallId),
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            % Note: SentAck is not from the server but only acks the sending went ok.
            case SentAck of
                ok ->
                    nil;
                _ ->
                    CallerPid ! {send_error, SentAck, CallId, ProcName, Args} %  {error,Reason} but never 'ok'
            end,
            tx_loop(Anchor, {CallNo+1, RespNo});

        % Send a procedure call. CallId needs only be locally unique
        % ----------------------------------------------------------
        {call, blowout, ProcName, Args, _CallerPid, Deliver} ->

            ?TRACE("#15B  erlvolt_conn:tx_loop/2 (cast) sends args ~w", [Args]),
            CallId = encode_pid(Deliver, Overhead),
            ?ERLVOLT_PROFILER_COUNT_PENDING(),
            % alternate: CallId = <<ConnId:32, CallNo:32>>, % 2^32 = ~4G*
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            erlvolt_wire:call_procedure(ConnId, Socket, ProcName, Args, CallId),
            %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            % Ignore SentAck from the sending.
            tx_loop(Anchor, {CallNo+1, RespNo});

        % Receive a server response: a result, error or signal
        % ----------------------------------------------------------
        {tcp,Socket,ResultBin} ->

            ?TRACE("#18  erlvolt_conn:tx_loop/2 receives server response: ~n~p", [ResultBin]),
            consume(ResultBin, Overhead),
            tx_loop(Anchor, {CallNo, RespNo+1});

        {tcp_closed, Socket} ->

            ?TRACE("~p Socket closed~n", [self()]),
            % 
            exit(erlvolt_socket_lost);

        {tcp_error, Socket, Reason} ->

            io:format("~p Socket error ~p~n", [self(), Reason]);
            % 

        {dump} ->
            io:format("Connection Receiver ~p alive.~n", [self()]),
            tx_loop(Anchor, Counter);

        % drain the connection
        % ----------------------------------------------------------
        {drain_only, RealDrainer, RealDrainWait} ->
            ?TRACE("#24x erlvolt_conn:tx_loop/2 proceeds to drain-only connection"),
            tx_loop({PoolId, ConnId, Socket, RealDrainer, RealDrainWait, drain_only, Overhead}, Counter);

        % close the connection
        % ----------------------------------------------------------
        {close, RealDrainer, RealDrainWait} ->
            ?TRACE("#24a erlvolt_conn:tx_loop/2 proceeds to drain connection for closing"),
            tx_loop({PoolId, ConnId, Socket, RealDrainer, RealDrainWait, close, Overhead}, Counter)

        % Note: DrainWait starts over each time something else is handled. It
        % designates a span of inactivity of this process, not a pure time span.
        after DrainWait ->
            ?TRACE("#25a  erlvolt_conn:tx_loop/2 drain times out as it should"),
            case Proceed of
                drain_only ->
                    ?TRACE("#25x  erlvolt_conn:tx_loop/2 done draining, continues"),
                    Drainer ! drained,
                    tx_loop({PoolId, ConnId, Socket, nil, infinity, nil, Overhead}, Counter);
                close ->
                    ?TRACE("#25b  erlvolt_conn:tx_loop/2 proceeds with closing socket"),
                    erlvolt_wire:close(Socket),
                    ?TRACE("#25c  erlvolt_conn:tx_loop/2 the connection process ends"),
                    Drainer ! closed
            end
    end.

%% @spec consume(binary(), binary()) -> 'ok'
consume(<<>>, _) ->
    ok;

consume(ResultBin, Overhead) ->
    ?TRACE("#19a  erlvolt_conn:consume/2 entered", []),
    {Result,Rest} = erlvolt_wire:erl_response(ResultBin),
    dispatch(Result, Overhead),
    consume(Rest, Overhead).

%% @private get a result received from the servers to the waiting initiator process.
%% This function is called from within the tx_loop/2 that communicates with the server.
%% It parses the calling processes Pid out of the lower lever voltresponse record.
%% However, the complete voltresponse record is handed over to the waiting process.
%% The Pid can belong to any process that called into the gen server erlvolt_conn_mgr
%% but usually will be a 'monitored worker process' from erlvolt:monitored_execute().
%% @spec dispatch({voltresponse, any()}) -> ok
dispatch(Result, Overhead) ->

    ?TRACE("#19b  erlvolt_conn:dispatch/1 identify waiting process and hand result over"),
    % ?TRACE("Result: ~w", [Result]),

    { voltresponse,
       { _Protocol,
         ClientData,
         _Status,
         _StatusString,
         _AppStatus,
         _AppStatusString,
         _SerializedException,
         _RoundTripTime
       },
       _Tables
    } = Result,

    Pid = decode_pid(ClientData, Overhead),


    %% This can go through to the caller of execute().
    %% *||*******************************************
    %% But the Pid could also be a waiting worker.
    
    Pid ! {result, Result}.

%% Open all connections of a pool.
%% @spec open_connections(#pool{}) -> #pool{}
open_connections(Pool) ->
    ?TRACE("#5   erlvolt_conn:open_connections/1"),
    open_connections(Pool, blocking, Pool#pool.hosts).

%% @spec open_connections(#pool{}) -> #pool{}
open_connections(Pool, Blocking) ->
    ?TRACE("#5   erlvolt_conn:open_connections/1"),
    open_connections(Pool, Blocking, Pool#pool.hosts).

%% @spec open_connections(#pool{},[{any(),any()}]) -> #pool{}
open_connections(Pool, _, []) ->
    Pool;

open_connections(Pool, Blocking, [{Host,Port}|MoreHosts]) ->
    case erlvolt_conn:open_connection(Pool, Host, Port, Blocking) of
        Conn when is_record(Conn, erlvolt_connection) ->
            case whereis(masterconn) of
                undefined ->
                    register(masterconn, Conn#erlvolt_connection.pid);
                _ ->
                    nil
            end,
            Available = queue:in(Conn, Pool#pool.available),
            open_connections(Pool#pool{available = Available}, Blocking, MoreHosts);
        Error ->
            Error
    end.

%% Open a connection and create a connection tx process that
%% tends to the async traffic at the socket.
%% @spec open_connection(#pool{},any(),any()) -> #erlvolt_connection{}
open_connection(#pool{pool_id=PoolId, user=User, password=Password, service=Database, slots=Slots, nagle=Nagle, send_buffer=SendBuffer, receive_buffer=ReceiveBuffer, send_timeout=SendTimeout}, Host, Port, Blocking) ->
    ?TRACE("#6   erlvolt_conn:open_connection/3"),
    
    ConnId = erlang:now(), % guaranteed to be unique in this VM
    case supervisor:start_child(erlvolt_sup_conn, [[PoolId, ConnId, Host, Port, User, Password, Database, Nagle, SendBuffer, ReceiveBuffer, SendTimeout, Blocking, self()]]) of
        {ok, Pid} ->
            Connection =
                #erlvolt_connection{
                    id = ConnId,
                    pid = Pid,
                    pool_id = PoolId,
                    slots = Slots,
                    nagle = Nagle,
                    send_buffer = SendBuffer,
                    receive_buffer = ReceiveBuffer,
                    send_timeout = SendTimeout,
                    pending = 0,
                    alive = true},

            % Block until successfully connected and logged in.
            case Blocking of
                blocking ->
                    receive
                        {Pid, loggedin} ->
                            Connection;
                        {Pid, failed, What, Why} ->
                            {erlvolt_connection_failed, What, Why}
                        after 20000 -> % will usually throw error below
                            throw (erlvolt_connection_timeout)
                    end;
            % non-blocking
                _ ->
                    Connection
            end;
        Else ->
            erlvolt:error("Failed to start connection process."),
            exit({erlvolt_connection_start_error, Else})
    end.

%% @private stub
%% @spec reset_connection(any(),any(),any()) -> none()
reset_connection(_Pool, _Slot, _StayLocked) ->
    exit(not_implemented_5).

%% @private Signal the main tx_loop of the connection to drain
%% the socket. Blocking, waits for the reply from the tx_loop.
%% Returns that replay. This function is spawned into its own
%% process per connection so that it's not receiving messages
%% meant for the connection manager.
%% @spec drain_connection(#erlvolt_connection{}) -> any()
drain_connection(Conn, DrainWait) ->
    ?TRACE("*** drain-only connection ~p.~n", [Conn]),
    Conn#erlvolt_connection.pid ! {drain_only, self(), DrainWait},
    receive
        R  ->
            R
    end.

%% @private Signal the main tx_loop of the connection to drain and close
%% the socket. Blocking, waits for the reply from the tx_loop.
%% Returns ok or the reply from tx_loop or exits with connection_close_timeout.
%% @spec close_connection(#erlvolt_connection{}) -> any()
close_connection(Conn, DrainWait, CloseWait) ->
    ?TRACE("*** drain and close connection ~p.~n", [Conn]),
    Conn#erlvolt_connection.pid ! {close, self(), DrainWait},
    receive
        R  ->
            R
        after CloseWait ->
            connection_close_timeout
            % 
    end.

%% 
%% moment that the pool is shut down. This is pretty much taken care of
%% by the timeout in the send loop that must be passed before the closing
%% happens, but will not while the queue keeps filling up any empty
%% slot. It could happen with a stalling server thought that the queue
%% is still filled but the timeout for the drain expires.
%% @spec blunt_waiting(any()) -> 'ok'
blunt_waiting(_Pid) ->
    ok.

%% @private execute a stored procedure or ad-hoc query.
%% This function is called by the monitored worker process. It communicates with
%% the socket-dedicated connection process ('tx process').
%% @spec execute([binary() | maybe_improper_list() | #erlvolt_slot{},...], synchronous, SentTimeout, ReceiveTimeout) -> any()
execute(Slot, ProcName, Args, synchronous, SendTimeout, ReceiveTimeout, Deliver)
    when is_record(Slot, erlvolt_slot) and is_list(Args) and is_pid(Deliver) ->

    ?TRACE("#14A erlvolt_conn:execute/1 synchronous"),

    %% Send Request
    %% Note: execute blocks until it receives the SEND-ack from the tx process.
    case execute(Slot, ProcName, Args, asynchronous, SendTimeout, awaitsendack, Deliver) of

        %% sending succeeded (this ok is the ok from erlvolt_wire:call_procedure/5)
        ok ->
            %% Wait for the server response forked over from tx_loop() and dispatch()
            %% (This processes Pid is sent to the server and parsed out by dispatch())
            receive
                Response ->
                    ?TRACE("erlvolt_conn:execute received server response"),
                    Response
                after ReceiveTimeout ->
                    erlvolt:error("erlvolt_conn:execute timed out waiting for server response"),
                    ?ERLVOLT_PROFILER_COUNT_PENDING(),
                    ?ERLVOLT_PROFILER_COUNT_FAILURE(),
                    exit(erlvolt_no_response)
            end;

        %% problem sending. Will be: {error, Reason}, coming from erlvolt_wire:call_procedure/5
        Else ->
            erlvolt:error("erlvolt_conn:execute could not execute: ~w", [Else]),
            Else
    end;

%% @private execute a stored procedure or ad-hoc query.
%% This function is called by the monitored worker process or by the user process.
%% It communicates with
%% the socket-dedicated connection process. This function returns the
%% result from  erlvolt_wire:call_procedure/5, ok | {error, Reason} or
%% exits with missing_internal_sent_ok.
%% @spec execute([binary() | maybe_improper_list() | #erlvolt_slot{},...]) -> any()

execute(Slot, ProcName, Args, asynchronous, SendTimeout, awaitsendack, Deliver)
    when is_record(Slot, erlvolt_slot), is_list(Args), (is_integer(SendTimeout) orelse SendTimeout == infinity), is_pid(Deliver) ->

    ?TRACE("#14B erlvolt_conn:execute asynchronous, blocking"),
    ?TRACE("execute binary: ~w w/~w", [ProcName, Args]),
    ?TRACE("using connection: ~w", [Slot#erlvolt_slot.connection_id]),

    %% Send Request to Connection Process
    %% Processed in tx_loop(), above.
    %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    CPid = Slot#erlvolt_slot.connection_pid,
    CPid ! {call, awaitsendack, ProcName, Args, self(), Deliver},
    %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    %% Receive Send-Ack from connection socket process, and return it.
    %% note: this is NOT ANY response from the VoltDB server. It is only
    %% the ok for the {call..}. More to the point the return value
    %% proper from erlvolt_wire:call_procedure/5, ok | {error, Reason}.
    receive

        SentAck ->
            SentAck

        after SendTimeout ->
            erlvolt:error("Missing internal sent ack"),
            exit(erlvolt_missing_internal_sent_ack)
    end;

execute(Slot, ProcName, Args, asynchronous, _SendTimeout, Care, Deliver)
    when is_record(Slot, erlvolt_slot), is_list(Args), (Care == fireandforget orelse Care == blowout),
    is_pid(Deliver) ->

    ?TRACE("#14C erlvolt_conn:execute/1 asynchronous, ~p", [Care]),
    ?TRACE("execute binary: ~w w/~w", [ProcName, Args]),
    ?TRACE("using connection: ~w", [Slot#erlvolt_slot.connection_id]),

    %% Send Request to Connection Process
    %% Processed in tx_loop(), above.
    %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    CPid = Slot#erlvolt_slot.connection_pid,
    CPid ! {call, Care, ProcName, Args, Deliver, Deliver}.
    %% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    %% Ignore any errors during sending.
    %% For fireandforget Errors may come right
    %% to this process whenever they come in.

%% @spec pid_encode_pattern() -> binary()
pid_encode_overhead() ->
    % use current Pid as template
    PidBin = term_to_binary(self()),
    Size = byte_size(PidBin) - 9,
    % parse out the insignificant bytes. Should be all but 2,
    % in reality seem to be all but 7. Two are known zeros.
    <<Overhead:Size/binary, 0, 0, _:7/binary>> = PidBin,
    Overhead.

%% @private make a compressed binary of the PID. The string representation
%% might be too long ("<0.100.0>"). It can only have 8 bytes for VoltDB.
%% @spec encode_pid(pid(), binary()) -> binary()
encode_pid(Pid, KnownOverhead) ->
    % parse out the 15 relevant bits from the external term format
    % This will fail if the overhead changes, which it should not.
    % However, different sizes of have been seen in normal rsp. CT run.
    S = byte_size(KnownOverhead),
    <<KnownOverhead:S/binary, 0, 0, Relevant:7/binary>> = term_to_binary(Pid),
    % This would mostly be 6 (sic) zeros and 2 relevant bytes.
    <<0, Relevant/binary>>.

%% @spec decode_pid(<<_:64>>, binary()) -> pid()
decode_pid(<<PidBin:8/binary>>, KnownOverhead) ->
    % parse out the 7 (should be 2) significant bytes
    <<0, Relevant:7/binary>> = PidBin,
    % merge the 7 (2) significant bytes into the known hull
    Term = <<KnownOverhead/binary, 0, 0, Relevant/binary>>,
    % convert, assert type
    case binary_to_term(Term) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            
            exit(erlvolt_pid_decode_failure)
    end.

%% @spec get_conn_mgr() -> 'undefined' | pid() | port()
get_conn_mgr() ->
    Mgr = whereis(erlvolt_conn_mgr),
    Mgr /= undefined orelse
        exit({failed_to_find_conn_mgr,
            "Failed to find Connection Manager when opening connection. " ++
            "Make sure crypto is started and erlvolt.app is in the Erlang path."}),
    Mgr.


%% 8.10  PID_EXT
%% 1    N       4   4       1
%% 103  Node    ID  Serial  Creation
%% Encode a process identifier object (obtained from spawn/3 or friends).
%% The ID and Creation fields works just like in REFERENCE_EXT, while the Serial field
%% is used to improve safety. In ID, only 15 bits are significant; the rest should be 0.
%% From: http://erlang.org/doc/apps/erts/erl_ext_dist.html

%% *) 4 billion ids before revolving means that there should not be any call back
%% left in the queue from 4 billion calls before, which should mean it's safe for
%% the timeouts.
