%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_conn.erl                                          %%%
%%% Version     : 0.3.0/beta                                                %%%
%%% Description : Erlang VoltDB driver joint supervisor module              %%%
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

-module(erlvolt_sup).
-behaviour(supervisor).

-vsn("0.3.0/beta").
-author("H. Diedrich <hd2012@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3.0/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

-export([start_link/0, init/1]).

%%% @spec start_link() -> ignore | {error,any()} | {ok,pid()}
start_link() ->                      % called by erlvolt_app:start/2
    erlvolt:trace("#2   erlvolt_sup:start_link/0"),
    supervisor:start_link({local, erlvolt_sup_profiler}, ?MODULE, erlvolt_sup_profiler),
    erlvolt_profiler:test(),
    supervisor:start_link({local, erlvolt_sup_conn_mgr}, ?MODULE, erlvolt_sup_conn_mgr),
    erlvolt_conn_mgr:test(),
    supervisor:start_link({local, erlvolt_sup_conn}, ?MODULE, erlvolt_sup_conn).


%:% init must return the supervisor spec and nested into it, the child spec(s).
%%% Supervisor of the global connection manager
%%% @spec init('erlvolt_sup_conn' | 'erlvolt_sup_conn_mgr') -> {'ok',{{'one_for_one',integer(),integer()} | {'simple_one_for_one',integer(),integer()},[any(),...]}}
init(erlvolt_sup_conn_mgr) ->
    erlvolt:trace("#2a  erlvolt_sup:init(erlvolt_conn_mgr)"),
    {ok, {{one_for_one, 10, 10}, [
        % Spec of the /one/ Connection Slot Manager
        { erlvolt_conn_mgr,
          {erlvolt_conn_mgr, start_link, []},
          permanent,
          5000,
          worker,
          [erlvolt_conn_mgr]}
    ]}};

%%% Supervisor of the individual connection send/receive workers
init(erlvolt_sup_conn) ->
    erlvolt:trace("#2b  erlvolt_sup:init(erlvolt_sup_conn)"),
    {ok, {{simple_one_for_one, 10, 10}, [
        % Spec of the /many, dynamic/ socket controlling Connection main loops
        { erlvolt_conn,
          {erlvolt_conn, start_link, []},
          transient,
          5000,
          worker,
          [erlvolt_conn]}
    ]}};

%:% init must return the supervisor spec and nested into it, the child spec(s).
%%% Supervisor of the global connection manager
%%% @spec init('erlvolt_sup_conn' | 'erlvolt_sup_conn_mgr') -> {'ok',{{'one_for_one',integer(),integer()} | {'simple_one_for_one',integer(),integer()},[any(),...]}}
init(erlvolt_sup_profiler) ->
    erlvolt:trace("#2c  erlvolt_sup:init(erlvolt_profiler)"),
    {ok, {{one_for_one, 10, 10}, [
        % Spec of the /one/ Profiler
        { erlvolt_profiler,
          {erlvolt_profiler, start_link, []},
          permanent,
          5000,
          worker,
          [erlvolt_profiler]}
    ]}}.


%:% Child Spec:
%:% {Id, StartFunc, Restart, Shutdown, Type, Modules}
%:%     Id = term()
%:%     StartFunc = {M, F, A}
%:%         M = F = atom()
%:%         A = [term()]
%:%     Restart = permanent | transient | temporary
%:%     Shutdown = brutal_kill | integer()>0 | infinity
%:%     Type = worker | supervisor
%:%     Modules = [Module] | dynamic
%:%     Module = atom()
