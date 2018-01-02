%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt.hrl                                               %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver data structures and macros           %%%
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
%%%    Copyright (C) 2008-2018 VoltDB Inc. http://www.voltdb.com            %%%
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

%%%----------------------------------------------------------------------------
%%% Response Patterns: Macros
%%%----------------------------------------------------------------------------

-define(VOLT_OK, {result, {voltresponse, {0, _, 1, <<>>, 128, <<>>, <<>>, _}, []}}).
-define(VOLT_ERROR_MESSAGE(T), {result,{voltresponse,{_,_,_,<<T>>,_,_,_,_},[]}}).
-define(VOLT_EMPTY_RESPONSE, {result, {voltresponse, {0, _, 1, <<>>, 128, <<>>, <<>>, _}, [{volttable,_,_,[]}]}}).

%%%----------------------------------------------------------------------------
%%% Procedure Call Status Code
%%%----------------------------------------------------------------------------

% A Byte value specifying the success or failure of a remote stored procedure call.
% * SUCCESS            =  1
% * USER_ABORT         = -1
% * GRACEFUL_FAILURE   = -2
% * UNEXPECTED_FAILURE = -3
% * CONNECTION_LOST    = -4

%%%----------------------------------------------------------------------------

-define(VOLT_SUCCESS,             1).
-define(VOLT_USER_ABORT,         -1).
-define(VOLT_GRACEFUL_FAILURE,   -2).
-define(VOLT_UNEXPECTED_FAILURE, -3).
-define(VOLT_CONNECTION_LOST,    -4).

%%%----------------------------------------------------------------------------
%%% Optional Verbose Debug Traces: Macros
%%%----------------------------------------------------------------------------

-define(TRACE(S), void).
-define(TRACE(F,S), void).
%-define(TRACE(S), erlvolt:trace(S)).
%-define(TRACE(F,S), erlvolt:trace(F,S)).

%%%----------------------------------------------------------------------------
%%% Optional Profiler: Macros
%%%----------------------------------------------------------------------------

-ifdef(profile).

-define(ERLVOLT_PROFILER_COUNT_PENDING(), erlvolt_profiler:count_pending()).
-define(ERLVOLT_PROFILER_COUNT_SUCCESS(), erlvolt_profiler:count_success()).
-define(ERLVOLT_PROFILER_COUNT_SUCCESS(T), erlvolt_profiler:count_success(T)).
-define(ERLVOLT_PROFILER_COUNT_FAILURE(), erlvolt_profiler:count_failure()).
-define(ERLVOLT_PROFILER_COUNT_FAILURE(T), erlvolt_profiler:count_failure(T)).
-define(ERLVOLT_PROFILER_COUNT_QUEUED(), erlvolt_profiler:count_queued()).
-define(ERLVOLT_PROFILER_COUNT_UNQUEUED(), erlvolt_profiler:count_unqueued()).
-define(ERLVOLT_PROFILER_COUNT_UNQUEUED(T), erlvolt_profiler:count_unqueued(T)).
-define(ERLVOLT_PROFILER_DUMP(ClientID), erlvolt_profiler:dump(ClientID)).
-define(ERLVOLT_PROFILER_DUMP_FUNCTION, dump).
-define(ERLVOLT_PROFILER_PENDING(), erlvolt_profiler:pending()).
-define(ERLVOLT_PROFILER_QUEUED(), erlvolt_profiler:queued()).
-define(ERLVOLT_PROFILER_WAITQUEUED(N), erlvolt_profiler:waitqueued(N)).
-define(ERLVOLT_PROFILER_WAITPENDING(N), erlvolt_profiler:waitpending(N)).
-define(ERLVOLT_PROFILER_CR, "~n").
-define(ERLVOLT_PROFILER_NCR, "").

-else.

-define(ERLVOLT_PROFILER_COUNT_PENDING(), void).
-define(ERLVOLT_PROFILER_COUNT_SUCCESS(), void).
-define(ERLVOLT_PROFILER_COUNT_SUCCESS(T), void).
-define(ERLVOLT_PROFILER_COUNT_FAILURE(), void).
-define(ERLVOLT_PROFILER_COUNT_FAILURE(T), void).
-define(ERLVOLT_PROFILER_COUNT_QUEUED(), void).
-define(ERLVOLT_PROFILER_COUNT_UNQUEUED(), void).
-define(ERLVOLT_PROFILER_COUNT_UNQUEUED(T), void).
-define(ERLVOLT_PROFILER_DUMP(ClientID), void).
-define(ERLVOLT_PROFILER_DUMP_FUNCTION, dummy).
-define(ERLVOLT_PROFILER_PENDING(), void).
-define(ERLVOLT_PROFILER_QUEUED(), void).
-define(ERLVOLT_PROFILER_WAITQUEUED(N), void).
-define(ERLVOLT_PROFILER_WAITPENDING(N), void).
-define(ERLVOLT_PROFILER_CR, "").
-define(ERLVOLT_PROFILER_NCR, "~n").

-endif.

