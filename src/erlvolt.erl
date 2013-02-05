%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt.erl                                               %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Main Erlang VoltDB driver module                          %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 13 Apr 2012                                               %%%
%%% Changed     : 04 Feb 2013                                               %%%
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
%%% For getting started with VoltDB, see: voltdb/doc/GettingStarted.pdf or  %%%
%%% online, http://voltdb.com/downloads/documentation/GettingStarted.pdf.   %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(erlvolt).

-vsn("0.3/beta").
-author("H. Diedrich <hd2012@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

%% @doc The main Erlvolt module.
%%
%% Erlvolt is implemented as an Erlang
%% <b>application</b>. The term has a special meaning in Erlang, see
%% [http://www.erlang.org/doc/design_principles/applications.html]
%%
%% This module exports functions to:
%% <li><b>start</b> and <b>stop</b> the driver (the 'application') and </li>
%% <li><b>execute</b> queries, as stored procedures, or 'ad-hoc'.</li>
%%
%% start(), stop(), modules() are pure one-line 'fascades':
%% ```
%%  start() -> application:start(erlvolt).
%%  stop() -> application:stop(erlvolt).
%%  modules() -> erlvolt_app:modules().
%% '''
%%
%% The implementation of `call_procedure()' with its various options (See
%% README.md) makes for the bulk of this module.
%%
%% The pool-related functions execute brief operations using the primitive
%% functions exported by `erlvolt_conn_mgr'.
%%
%% Result-related getter functions are also pure forwarders to the respective
%% functions exported by `erlvolt_wire'. Note that many guards hit not
%% before erlvolt_wire, to not double them and to keep the facade functions
%% here type-agnostic.
%%
%% @end doc: /hd 4 feb 13

-export([   start/0, stop/0,
            trace/1, trace/2,
            error/1, error/2, error/3,
            add_pool/2, add_pool/3,
            close_pool/1, close_pool/3,
            drain_pool/1, drain_pool/2,
            get_connections/1,
            modules/0,
            call_procedure/2, call_procedure/3, call_procedure/4,
            get_one/1,
            get_field/2,
            get_integer/2,
            get_integer/3,
            get_integer_or_null/2,
            get_integer_or_null/3,
            get_string/2,
            get_string/3,
            get_string_or_null/2,
            get_string_or_null/3,
            get_table/2,
            get_table_list/1,
            get_row/2,
            get_row_list/1,
            get_record/3,
            get_status/1,
            get_statusstring/1

        ]).

% for record and constant defines
-include("../include/erlvolt.hrl").

%% @doc Start the Erlvolt application.
%%
%% Simply calls `application:start(erlvolt).'
%%
%% === From the Erlang Manual ===
%% If the application is not already loaded, the application controller will
%% first load it using application:load/1. It will check the value of the
%% applications key, to ensure that all applications that should be started
%% before this application are running. The application controller then
%% creates an application master for the application. The application master
%% is the group leader of all the processes in the application. The
%% application master starts the application by calling the application
%% callback function start/2 in the module, and with the start argument,
%% defined by the mod key in the .app file.
%%
%% application:start(Application) is the same as calling
%% application:start(Application, temporary). If a temporary application
%% terminates, this is reported but no other applications are terminated.
%%
%% See [http://www.erlang.org/doc/design_principles/applications.html]
%% @end doc: /hd 4 feb 13
%%
%% @spec start() -> 'ok' | {'error', any()}
start() ->
    application:start(erlvolt).

%% @doc Stop the Erlvolt application.
%%
%% Simply calls `application:stop(erlvolt).'
%%
%% === From the Erlang Manual ===
%% It is always possible to stop an application explicitly by calling
%% application:stop/1. Regardless of the mode, no other applications will be
%% affected.
%%
%% See [http://www.erlang.org/doc/design_principles/applications.html]
%% @end doc: /hd 4 feb 13
%%
%% @spec stop() -> 'ok' | {'error',any()}
stop() ->
    application:stop(erlvolt).

%% @doc Returns the list of Erlvolt modules.
%%
%% === Sample ===
%% ```
%%  $ erl
%%  1> crypto:start().
%%  2> application:start(erlvolt).
%%  3> erlvolt:modules().
%%  [erlvolt,erlvolt_conn,erlvolt_conn_mgr]
%% '''
%% === Implementation ===
%%
%% Simply a call to `erlvolt_app:modules()'.
%% @private
%% @end doc: /hd 4 feb 13
%%
%% @spec modules() -> list()
modules() ->
    erlvolt_app:modules().


%% @doc Synchronous call to the connection manager to add a pool.
%%
%% === Implementation ===
%%
%% Creates a pool record, opens the connections and calls
%% erlvolt_conn_mgr:add_pool() to make the pool known to the pool management.
%% erlvolt_conn_mgr:add_pool() is translated into a blocking gen-server call.
%% @end doc: /hd 4 feb 13
%%
%% @spec add_pool(PoolId, Hosts) -> Result
%%      PoolId = any()
%%      Hosts = [{string(),Port}]
%%      Port = integer()
%%      Result = {error, pool_already_exists} | ok
add_pool(PoolId, Hosts) ->
    add_pool(PoolId, Hosts, []).

%% @spec add_pool(PoolId, Hosts, Opts) -> Result
%%      PoolId = any()
%%      Hosts = [{string(),Port}]
%%      Port = integer()
%%      Result = {error, pool_already_exists} | ok
%%      Opts = proplist()
add_pool(PoolId, Hosts, Opts) ->
    ?TRACE("#4   erlvolt:add_pool/8"),
    Pool = #pool{
        pool_id = PoolId,
        hosts = Hosts,
        user = proplists:get_value(user, Opts, "user"),
        password = proplists:get_value(password, Opts, "password"),
        service = proplists:get_value(service, Opts, "database"),
        queue_size = proplists:get_value(queue_size, Opts, 10000),
        slots = proplists:get_value(slots, Opts, 100),
        nagle = proplists:get_value(nagle, Opts, true),
        send_buffer = proplists:get_value(send_buffer, Opts, 100000),
        receive_buffer = proplists:get_value(receive_buffer, Opts, 100000),
        send_timeout = proplists:get_value(send_timeout, Opts, 10000)
    },
    % 
    Blocking = case proplists:get_value(blocking, Opts, true) of true -> blocking; _ -> nonblocking end,
    case erlvolt_conn:open_connections(Pool, Blocking) of
        OpenedPool when is_record(OpenedPool, pool) ->
            try
                erlvolt_conn_mgr:add_pool(OpenedPool)
            catch
                exit:{noproc,{gen_server,call,_}}=Why ->
                    What=exit,
                    erlvolt:error("Connection manager not running: ~p.~n~p.", [What,Why]),
                    exit({What,Why});
                What:Why ->
                    erlvolt:error("Could not add pool to connection manager state: ~p.~n~p.", [What,Why], notrace),
                    % 
                    exit({What,Why})
            end;
        Error ->
            erlvolt:error("Could not create connection pool:~n~p.", [Error], notrace),
            % 
            throw({connection_failed, Error})
    end.


%% @doc Synchronous command to connection manager to drain a pool.
%% The connection process of every connection in the pool is monitored for
%% having more than 500 msec continuous idle time, whereafter it is considered
%% drained. The idle wait is also restarted by sends.
%%
%% This function blocks. It can block forever if sending and receiving does not
%% stop. It returns a list of atoms `drained', one for each connection.
%%
%% This function is not leading to a close. It needs not be called before close_pool(),
%% which drains the connections implicitly before closing.
%% @end doc: /hd 4 feb 13
%% @spec drain_pool(PoolId) -> [ drained, ... ] | {error, pool_not_found}
%%      PoolId = any()
drain_pool(PoolId) ->
    drain_pool(PoolId, 500).

%% @doc Synchronous command to connection manager to drain a pool.
%% @see drain_pool/1
%% @end doc: /hd 4 feb 13
%% @spec drain_pool(PoolId, DrainWait) -> [ drained, ... ] | {error, pool_not_found}
%%      PoolId = atom()
%%      DrainWait = integer()
drain_pool(PoolId, DrainWait) ->
    erlvolt_conn_mgr:drain_pool(PoolId, DrainWait).

%% @doc Synchronous command to connection manager to close a pool.
%% The connection process of every connection in the pool is 'drained': i.e.
%% monitored for having more than 500 msec continuous idle time, whereafter it is
%% considered drained. The idle wait is also restarted by sends.
%%
%% This function blocks. It can block up to 1000 msec if sending and receiving
%% does not stop, i.e. the connections cannot be drained.
%%
%% On success, the function returns a list of atoms `closed', one for each connection.
%% @end doc: /hd 4 feb 13
%% @spec close_pool(PoolId) -> [ closed, ... ] | connection_close_timeout | {error, pool_not_found}
%%      PoolId = any()
close_pool(PoolId) ->
    close_pool(PoolId, 500, 1000).

%% @doc Synchronous command to connection manager to close a pool.
%% @see close_pool/1
%% @end doc: /hd 4 feb 13
%% @spec close_pool(PoolId, DrainWait, CloseWait) -> [ closed, ... ] | connection_close_timeout | {error, pool_not_found}
%%      PoolId = atom()
%%      DrainWait = integer()
%%      CloseWait = integer()
close_pool(PoolId, DrainWait, CloseWait) ->
    erlvolt_conn_mgr:close_pool(PoolId, DrainWait, CloseWait).

%% @doc Execute a stored procedure or ad-hoc query.
%% Same as `call_procedure(PoolId, ProcName, [], [])'.
%% @see call_procedure/4
%% @end doc: /hd 4 feb 13
%% @spec call(PoolId | ConnRobin, ProcName) -> result() | asynch_ok() | call_error()
%%      PoolId = atom()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = atom()
call_procedure(PoolId, ProcName) ->
    call_procedure(PoolId, ProcName, [], []).

%% @doc Execute a stored procedure or ad-hoc query.
%% Same as `call_procedure(PoolId, ProcName, Args, [])'.
%% @see call_procedure/4
%% @end doc: /hd 4 feb 13
%%
%% @spec call(PoolId | ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts) -> result() | asynch_ok() | call_error()
%%      PoolId = atom()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = atom()
%%      Args = [any()]
%%      Query = binary() | string()
%%
call_procedure(PoolId, ProcName, Args) when is_list(Args) ->
    call_procedure(PoolId, ProcName, Args, []).

%% @doc Execute a stored procedure or ad-hoc query.
%% The first parameter is either a pool id or a 'connection round robin' tuple received
%% from get_connection_list/1. The second parameter is the procedure name, are the
%% string "@AdHoc". The third are arguments to the procedure, or a query string, for
%% ad-hoc execution (that is VoltDB parlance for a normal query that is not a stored
%% procedure and that can be magnitudes slower). The fourth are options:
%% force | queue | drop: determine whether the call management's peak buffer,
%% a call `queue` is used or not; the call management can also be used without
%% the queue and set to `drop` calls that can't be executed immediately; or the
%% call management can be circumvented using `force`, in which case a strict
%% round robin is applied for choosing the cluster node to send a call to.
%% monitored | direct: a call can be executed through a `monitored`,
%% specially spawned workhorse process, which shields the user process from
%% problems in the driver; or it can be executed in `direct` fashion were the
%% internal driver functions are executed by the user process itself.
%% synchronous | asynchronous: with `synchronous` execution,
%% `call_procedure()` blocks until it receives the answer from the server and
%% returns the actual result; with `asynchronous` execution, the call returns
%% immediately and the result is sent to the calling processes' message box.
%% awaitsendack | fireandforget | blowout: For fine tuning, the
%% acknowledgement level can be used to make call execution 'one-way'. These
%% options only apply to  `asynchronous` execution. The setting of
%% `awaitsendack` means that call_procedure() returns after it got the ok from
%% the socket that the call was successfully sent. This is meaningful, and can
%% cause a long wait, because of **backpressure**: a VoltDB cluster can stop
%% reading from the socket temporarily in a sign that it is at capacity. A less
%% thorough setting is `fireandforget`, which will return immediately from the
%% send and will have any problems from the socket sent to the user processes'
%% mailbox, just as any error coming from the server, But all non-error results
%% coming from the server are silently dropped. Finally, `blowout` suppresses
%% any feedback, be it from the sending itself or any responses or errors from
%% the server.
%% The default setting is `[queue, monitored, synchronous]'.
%%
%% === Implementation ===
%%
%% This is a wrapping function that mainly determines what variant of
%% call_procedure/8 to call, depending on the combination of options turned in.
%%
%% @end doc: /hd 4 feb 13
%% @spec call(PoolId | ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts) -> result() | asynch_ok() | call_error()
%%      PoolId = any()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%
call_procedure(Link, ProcName, Param, Opts) when is_list(Param) andalso is_list(Opts) ->

    %% Parameter merging and defaults
    Manage = case proplists:is_defined(force, Opts) of
        true-> force;
        false-> case proplists:is_defined(drop, Opts) of
            true -> drop;
            false -> queue
        end
    end,
    Monitored = case proplists:is_defined(direct, Opts) of
        true -> direct;
        false -> monitored
    end,
    Synch = case proplists:is_defined(asynchronous, Opts) of
        true -> asynchronous;
        false -> synchronous
    end,
    Care = case proplists:is_defined(blowout, Opts) of
        true -> blowout;
        false -> case proplists:is_defined(fireandforget, Opts) of
            true-> fireandforget;
            false -> awaitsendack % can also be for asynch! For backpressure
        end
    end,

    %% switch to respective execution path
    call_procedure(Link, ProcName, Param, Opts, Manage, Monitored, Synch, Care).

%*****************************************************************************%
%                          Internal Call Functions                            %
%*****************************************************************************%

%% @doc Call a procedure, using connection copies round robin, bypassing the
%% connection manager, using a worker process inside the driver.
%% The call can be synchronous or asynchronous. I.e. the call can return
%% a result, or an error, or the result will come to the mailbox of calling process.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts, force, monitored, Sync, Care) -> result() | asynch_ok() | call_error()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Opts = proplist()
%%      Sync = synchronous | asynchronous
%%      Care = awaitsendack | fireandforget | blowout
call_procedure({ConnList, Round}, ProcName, Param, Opts, force, monitored, Sync, Care) when is_list(ConnList), is_list(Param), is_list(Opts) ->
    ?TRACE("#10A  erlvolt:call on conn list, round ~p, force, monitored, ~p", [Round, Sync]),
    ?TRACE("call: ~w w/~w opts ~w",[ProcName, Param, Opts]),
    N = Round rem length(ConnList) + 1,
    Conn = lists:nth(N, ConnList),
    Slot = erlvolt_conn_mgr:create_slot_warrant(Conn),
    SendTimeout = proplists:get_value(send_timeout, Opts, 10000),
    ReceiveTimeout = proplists:get_value(send_timeout, Opts, 10000),
    call_procedure_monitored(Slot, ProcName, Param, Sync, Care, SendTimeout, ReceiveTimeout);

%% @doc Call a procedure, using connection copies round robin, bypassing the
%% connection manager, NOT using a worker process inside the driver but the
%% original, calling user microprocess. Blocks and returns the call's result, or an error.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts, force, direct, synchronous, _) -> result() | call_error()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Opts = proplist()
call_procedure({ConnList, Round}, ProcName, Param, Opts, force, direct, synchronous, _) when is_list(ConnList), is_list(Param), is_list(Opts) ->
    ?TRACE("#10B  erlvolt:call on conn list, round ~p, force, direct, synchronous", [Round]),
    N = Round rem length(ConnList) + 1,
    Conn = lists:nth(N, ConnList),
    Slot = erlvolt_conn_mgr:create_slot_warrant(Conn),
    SendTimeout = proplists:get_value(send_timeout, Opts, 10000),
    ReceiveTimeout = proplists:get_value(send_timeout, Opts, 10000),
    erlvolt_conn:execute(Slot, ProcName, Param, synchronous, SendTimeout, ReceiveTimeout, self());

%% @doc Call a procedure, using connection copies round robin, bypassing the
%% connection manager, NOT using a worker process inside the driver but the
%% original, calling user microprocess. Returns when call is sent to socket.
%% Result will come to mailbox of calling process.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts, force, direct, asynchronous, _) -> asynch_ok() | call_error()
%%      ConnRobin = {[ erlvolt_connection() ], Round::integer()}
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Opts = proplist()
call_procedure({ConnList, Round}, ProcName, Param, Opts, force, direct, asynchronous, Care) when is_list(ConnList), is_list(Param), is_list(Opts) ->
    ?TRACE("#10C erlvolt:call on conn list, round ~p, force, direct, asynchronous", [Round]),
    ?TRACE("call: ~w w/~w opts ~w",[ProcName, Param, Opts]),
    N = Round rem length(ConnList) + 1,
    Conn = lists:nth(N, ConnList),
    Slot = erlvolt_conn_mgr:create_slot_warrant(Conn),
    SendTimeout = proplists:get_value(send_timeout, Opts, 10000),
    erlvolt_conn:execute(Slot, ProcName, Param, asynchronous, SendTimeout, Care, self());

%% @doc Call a procedure, using a pool and the connection manager;
%% *queueing* the call when no slot is immediately available; and using a worker
%% process inside the driver.
%% The call can be synchronous or asynchronous. I.e. the call can return
%% a result, or an error, or the result will come to the mailbox of calling process.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts, queue, monitored, Sync, Care) -> result() | asynch_ok() | call_error()
%%      PoolId = any()
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Opts = proplist()
%%      Sync = synchronous | asynchronous
%%      Care = awaitsendack | fireandforget | blowout
call_procedure(PoolId, ProcName, Param, Opts, queue, monitored, Sync, Care) when is_list(Param) ->
    ?TRACE("#10D erlvolt:call on pool id, queue, monitored, ~p", [Sync]),
    ?TRACE("call: ~w w/~w opts ~w",[ProcName, Param, Opts]),
    QueueTimeout = proplists:get_value(queue_timeout, Opts, 10000),
    SendTimeout = proplists:get_value(send_timeout, Opts, 10000),
    ReceiveTimeout = proplists:get_value(send_timeout, Opts, 10000),
    ?TRACE("call_procedure: getting slot for pool id ~w (~p)",[PoolId, Sync]),
    case erlvolt_conn_mgr:get_slot_blocking(PoolId, QueueTimeout) of
        Slot when is_record(Slot, erlvolt_slot) ->
            ?TRACE("call_procedure: got slot for pool id ~w: ~w", [PoolId, Slot#erlvolt_slot.id]),
            ?TRACE("call_procedure: executing ~w w/~w for pool id ~w (~p)",[ProcName, Param, PoolId, Sync]),
            call_procedure_monitored(Slot, ProcName, Param, Sync, Care, SendTimeout, ReceiveTimeout);
        Fail ->
            throw(Fail)
    end;

%% @doc Call a procedure, using a pool and the connection manager;
%% *rejecting* the call when no slot is immediately available; and using a worker
%% process inside the driver when it is.
%% The call can be synchronous or asynchronous. I.e. the call can return
%% a result, or an error, or the result will come to the mailbox of calling process.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(ConnRobin, ProcName | "@AdHoc", Args | [Query], Opts, drop, monitored, Sync, Care) -> result() | asynch_ok() | call_error()
%%      PoolId = any()
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Opts = proplist()
%%      Sync = synchronous | asynchronous
%%      Care = awaitsendack | fireandforget | blowout
call_procedure(PoolId, ProcName, Param, Opts, drop, monitored, Sync, Care) when is_list(Param) ->
    ?TRACE("#10E erlvolt:call on pool id, drop, monitored, ~p", [Sync]),
    ?TRACE("call: ~w w/~w opts ~w",[ProcName, Param, Opts]),
    ?TRACE("call_procedure: getting slot for pool id ~w (~p)",[PoolId, Sync]),
    case erlvolt_conn_mgr:get_slot(PoolId) of
        Slot when is_record(Slot, erlvolt_slot) ->
            ?TRACE("call_procedure: got slot for pool id ~w: ~w", [PoolId, Slot#erlvolt_slot.id]),
            ?TRACE("call_procedure: executing ~w w/~w for pool id ~w (~p)",[ProcName, Param, PoolId, Sync]),
            SendTimeout = proplists:get_value(send_timeout, Opts, 10000),
            ReceiveTimeout = proplists:get_value(send_timeout, Opts, 10000),
            call_procedure_monitored(Slot, ProcName, Param, Sync, Care, SendTimeout, ReceiveTimeout);
        Fail ->
            ?TRACE("call_procedure: no slot free right now (~p), canceling call for pool id ~w", [Fail, PoolId]),
            Fail
    end;

%% @doc Call a procedure with wrong combinations of options. Get an error.
%% @end doc: /hd 4 feb 13
%% @spec call_procedure(Link, ProcName, Args | [Query], Opts, Manage, Monitor, Sync, Care) -> bad_options
%%      Link = any()
%%      ProcName = any()
%%      Args = any()
%%      Query = any()
%%      Opts = any()
%%      Manage = any()
%%      Monitor = any()
%%      Sync = any()
%%      Care = any()
call_procedure(Link, ProcName, Param, Opts, Manage, Monitor, Synch, Care) ->
    erlvolt:error("call_procedure: bad options combination: ~p, ~p, ~p, ~p, ~p <- ~p, ~p, ~p.",
         [Link, ProcName, Param, Opts, Manage, Monitor, Synch, Care]),
    bad_options.

%% @doc Execute a stored procedure or ad-hoc query by means of a worker process
%% that executes functions within the driver beyond this point. This shields
%% the calling user process from problems in the driver.
%%
%% call_procedure_monitored is oblivious to the fact wether the send call is blocking
%% or not. This function is called by the original user process. It spawns a 'membrane'
%% worker fun that does nothing but call erlvolt_conn_execute/6 or /7 and send the
%% results from that back to the original process that is waiting for the answer
%% all in this function.
%%
%% @private
%% @end doc: /hd 4 feb 13
%% @spec call_procedure_monitored(Slot, ProcName | "@AdHoc", Args | [Query], Sync, Care, SendTimeout, ReceiveTimeout) -> result() | asynch_ok() | call_error() | exit()
%%      Slot = #erlvolt_slot{}
%%      ProcName = binary() | string()
%%      Args = [any()]
%%      Query = binary() | string()
%%      Sync = synchronous | asynchronous
%%      Care = awaitsendack | fireandforget | blowout
%%      SentTimeout = integer() | infinity
%%      ReceiveTimeout = integer() | infinity
call_procedure_monitored(Slot, Proc, Param, Sync, Care, SendTimeout, ReceiveTimeout) when is_record(Slot, erlvolt_slot) ->

    ?TRACE("#12  erlvolt:call_procedure_monitored/3"),

    %% spawn a new process to do work.
    %% -------------------------------
    %% then monitor that process until it dies, is done or times out.
    %% The worker stays alive until it got a response from the server.
    %% It takes the brunt of driver errors and keeps that away from
    %% the calling user process and the connection socket process.
    Parent = self(),
    WPid = spawn(
        fun() ->
            receive start ->
                ?TRACE("#13  spawned worker starts blocking execute"),
                Result = case Sync of
                    synchronous ->
                        erlvolt_conn:execute(Slot, Proc, Param, Sync, SendTimeout, ReceiveTimeout, self());
                    asynchronous ->
                        erlvolt_conn:execute(Slot, Proc, Param, Sync, SendTimeout, Care, Parent)
                end,
                ?TRACE("#13a  spawned worker done with blocking execute"),
                ?TRACE("#13b  spawned worker sends to parent ~w the result ~w", [Parent, Result]),
                Parent ! {self(), result, Result},
                ?TRACE("#13d  spawned worker ends now")
            end
        end),
    Mref = erlang:monitor(process, WPid),
    WPid ! start,
    ?TRACE("#12a spawned worker messaged to start"),

    ?TRACE("#12b entering monitored receive block"),

    %% receive the sent-ok and results
    %% -------------------------------
    %% those are two steps. the response from the server
    %% is expected within milliseconds usually.
    Return = receive
        {'DOWN', Mref, process, WPid, {_, closed}} -> % 
            exit(not_implemented_1);

        {'DOWN', Mref, process, WPid, normal} ->
            ?TRACE("#20 call_procedure_monitored: DOWN normal -> exit~n", []);

        {'DOWN', Mref, process, WPid, _Reason} -> % 
            ?TRACE("call_procedure_monitored: DOWN ~p -> exit~n", [_Reason]),
            exit({not_implemented_1B,_Reason});

        {WPid, sent, ok} ->
            %% child process sent data ok
            %% on arrival the result will be messaged from connection manager to WPid.
            ?TRACE("#17  erlvolt:call_procedure_monitored receive: sent"),
            ?TRACE("call_procedure_monitored: sent message ok -> demonitor ~p, unlock connection ~p slot ~p, return 'sent'", [Mref, Slot#erlvolt_slot.connection_id, Slot#erlvolt_slot.id]),
            erlang:demonitor(Mref, [flush]),
            erlvolt_conn_mgr:pass_slot(Slot),
            sent;

        {WPid, sent, {send_error, Reason}} ->
            %% child process encountered error (e.g. backpressure) when/instead of sending
            %% there will likely be no result coming in - but it can happen
            ?TRACE("call_procedure_monitored: send-error ~p -> demonitor ~p, unlock connection ~p slot ~p, return error", [Reason, Mref, Slot#erlvolt_slot.connection_id, Slot#erlvolt_slot.id]),
            erlang:demonitor(Mref, [flush]),
            erlvolt_conn_mgr:pass_slot(Slot),
            {send_error, Reason};

        {WPid, result, Result} ->
            %% if the process returns data, unlock the
            %% connection and collect the normal 'DOWN'
            %% message send from the child process
            ?TRACE("#21   erlvolt:call_procedure_monitored receive: result"),
            ?TRACE("call_procedure_monitored: got result -> demonitor ~w, unlock connection ~w slot ~w, return result", [Mref, Slot#erlvolt_slot.connection_id, Slot#erlvolt_slot.id]),
            erlang:demonitor(Mref, [flush]),
            erlvolt_conn_mgr:pass_slot(Slot),
            
            Result

        after ReceiveTimeout ->
            %% if we timeout waiting for the process to return,
            %% then reset the connection and throw a timeout error
            ?TRACE("call_procedure_monitored: TIMEOUT -> demonitor, reset slot, exit~n", []),
            erlang:demonitor(Mref),
            exit({erlvolt_monitored_receive_timeout, ReceiveTimeout, {}})
    end,

    %% In case the down was sent before the demonitor call happened.
    receive
        {'DOWN', Mref, process, WPid, normal} ->
            nil;
        {'DOWN', Mref, process, WPid, Reason1} ->
            exit(worker_crashed_at_shutdown, Reason1)
        after 0 -> nil
    end,

    Return.

%%
get_connections(PoolID) ->
    erlvolt_conn_mgr:get_connections(PoolID).


%*****************************************************************************%
%                              Debug Functions                                %
%*****************************************************************************%

%%--------------------------------------------------------------------
%% @doc trace to screen, special made for max readability of internal sequence
%% during driver initialization and a procedure call.
%% @spec trace(any()) -> 'void' | ok
trace(_S) ->
    ?TRACE(_S, []).

%% @spec trace([any()],[any()]) -> 'ok'
%%--------------------------------------------------------------------
%% @doc trace to screen, special made for max readability of internal sequence
%% during driver initialization and a procedure call.
%% @spec trace(any()) -> 'void' | ok
trace([$#|_]=F,A) ->
    io:format("*** ~.10w | " ++ F ++ " ~n", [self() | A]);

trace([C|_]=F,A) when C == $I; C == $V ->
    io:format("*** ................................................................................~n", []),
    io:format("*** ~.10w | " ++ F ++ " ~n", [self() | A]);

trace(F,A) ->
    io:format("*** ~.10w |       " ++ F ++ " ~n", [self() | A]).

%%--------------------------------------------------------------------
%% @doc extra visible error dump
%% @spec error(list()) -> 'ok'
error(S) ->

    erlvolt:error(S, [], trace).

%% @doc extra visible error dump, with strack trace
%% @spec error(list(), list(any())) -> 'ok'
error(F,A) ->

    error(F,A, trace).

%% @doc extra visible error dump, with or w/o stack trace
%% @spec error(list(), list(any()), trace | notrace) -> 'ok'
error(F,A, Trace) ->
    F1 = re:replace(F,"~n","~n###                           ",[global,{return,list}]),
    case Trace of
        trace ->
            io:format("\\///////////////////////////////////////////////////////////////////////////////////~n" ++
                      "/// ~.10w | ###  ERROR: " ++ F1 ++ "~n" ++
                      "///                   Process: ~p~n" ++
                      "///                   Trace: ~p~n" ++
                      "////////////////////////////////////////////////////////////////////////////////////~n",
            [self()] ++ A ++ [self(), erlang:get_stacktrace()]);
        notrace ->
            io:format("\\///////////////////////////////////////////////////////////////////////////////////~n" ++
                      "/// ~.10w | ###  ERROR: " ++ F1 ++ "~n" ++
                      "////////////////////////////////////////////////////////////////////////////////////~n",
            [self()] ++ A)
    end.



%*****************************************************************************%
%                          Result Access Functions                            %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% @doc Get the single value expected as return out of the Volt table format.
%% @spec get_one(voltresponse() | {result, voltresponse()}) -> any()
get_one({result, { voltresponse, _, _ } = R}) ->

    get_one(R);

get_one({ voltresponse, _, [ Table | _ ] }) ->

    {voltrow, [One]} = erlvolt_wire:get_row(Table, 1),
    One.

%*****************************************************************************%
%% These are all pure facades. The guards are implemented 'further down' with %
%%  knowledge of the structures used in the implementing module erlvolt_wire. %
%%%-------------------------------------------------------------------------%%%

%%%----------------------------------------------------------------------------
%%% @doc Get the status number from the result.
%% @spec get_status(voltresponse() | {result,voltresponse()}) -> integer()

get_status(ResultOrResponse) ->

    erlvolt_wire:get_status(ResultOrResponse).

%%%----------------------------------------------------------------------------
%%% @doc Get the status string from the result.
%% @spec get_statusstring(voltresponse() | {result,voltresponse()}) -> list()

get_statusstring(ResultOrResponse) ->

    erlvolt_wire:get_statusstring(ResultOrResponse).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row, by index number. First == 1.
%%% @spec get_field(voltrow(), Pos::pos_integer()) -> term()

get_field(Row, Pos) ->

    erlvolt_wire:get_field(Row, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by index number. First == 1.
%%% @spec get_string(voltrow(), Pos::pos_integer()) -> list()

get_string(RowOrRecord, Pos) ->

    erlvolt_wire:get_string(RowOrRecord, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by column name; error when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_string(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_string(Row, Table, Name) ->

    erlvolt_wire:get_string(Row, Table, Name).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by index number; null when Pos bad.
%%% @spec get_string_or_null(voltrow(), Pos::pos_integer()) -> 'null' | list()

get_string_or_null(RowOrRecord, Pos) ->

    erlvolt_wire:get_string_or_null(RowOrRecord, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by column name; null when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_string_or_null(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_string_or_null(Row, Table, Name) ->

    erlvolt_wire:get_string_or_null(Row, Table, Name).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by index number. First == 1.
%%% @spec get_integer(voltrow(), Pos::pos_integer()) -> list()

get_integer(RowOrRecord, Pos) ->

    erlvolt_wire:get_integer(RowOrRecord, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by index number; null when Pos bad.
%%% @spec get_integer_or_null(voltrow(), Pos::pos_integer()) -> 'null' | list()

get_integer_or_null(RowOrRecord, Pos) ->

    erlvolt_wire:get_integer_or_null(RowOrRecord, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by column name; error when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_integer(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_integer(Row, Table, Name) ->

    erlvolt_wire:get_integer(Row, Table, Name).

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by column name; null when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_integer_or_null(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_integer_or_null(Row, Table, Name) ->

    erlvolt_wire:get_integer_or_null(Row, Table, Name).

%%%----------------------------------------------------------------------------
%%% @doc Get a table from the result list by list position. First == 1.
%% @spec get_table(voltresponse() | {result,voltresponse()}, pos_integer()) -> volttable()

get_table(ResultOrResponse, Pos) ->

    erlvolt_wire:get_table(ResultOrResponse, Pos).

%%% @doc Get all tables out of a given response.
%%% @spec get_table_list(volttable()) -> [voltrow()]

get_table_list(ResultOrResponse) ->

    erlvolt_wire:get_table_list(ResultOrResponse).

%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table, by index number. First == 1.
%% @spec get_row(volttable(),pos_integer()) -> voltrow()

get_row(Table, Pos) ->

    erlvolt_wire:get_row(Table, Pos).

%%%----------------------------------------------------------------------------
%%% @doc Get all rows out of a given table.
%%% @spec get_row_list(volttable()) -> [voltrow()]

get_row_list(Table) ->

    erlvolt_wire:get_row_list(Table).


%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table as record, by index number. First == 1.
%%% @spec get_record(volttable(), RecordTag, Pos::pos_integer()) -> record()

get_record(Table, RecordTag, Pos) ->

    erlvolt_wire:get_record(Table, RecordTag, Pos).
