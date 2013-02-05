%%%-------------------------------------------------------------------------%%%
%%% File        : examples/hello_plus.erl                                   %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver robust 'Hello, world!' example file  %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 28 Jan 2013                                               %%%
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
%%% You can run this sample using the 'Hello' tutorial-server discussed     %%%
%%% in the VoltDB manual and present in every VoltDB distribution.          %%%
%%%                                                                         %%%
%%% Start that server from your voltdb installation with:                   %%%
%%%                                                                         %%%
%%%     $ cd voltdb/doc/tutorial/helloworld                                 %%%
%%%     $ ./run.sh                                                          %%%
%%%                                                                         %%%
%%% Then run this hello world example, using make from the driver root:     %%%
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
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%       For a barebones 'hello world' see examples/hello.erl.             %%%
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

-module(hello_plus).
-export([run/0]).
-import(erlvolt).
-include("erlvolt.hrl").

run() ->

    %%%
    %%% Start driver
    %%%

    crypto:start(),
    application:start(erlvolt),

    %%%
    %%% Connection Pool Parameter
    %%%

    Hosts    = [{"localhost", 21212}],

    try

        %%%
        %%% Connect to the database
        %%%

        erlvolt:add_pool(hello_pool, Hosts, [blocking]),

        %%%
        %%% Load sample data into the database
        %%%

        erlvolt:call_procedure(hello_pool, "Insert", ["Hello",  "World", "English"]),
        erlvolt:call_procedure(hello_pool, "Insert", ["Hej", "världen", "Swedish"]),

        %%%
        %%% Synchronous Query
        %%%

        Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"]),

        %%%
        %%% Result
        %%%

        case Result of

            %% Expected result e.g.:
            %% {result,
            %%   {voltresponse,
            %%        {0, <<"<0.45.0>">>, 1, <<>>, 128, <<>>, <<>>, 1},
            %%        [{volttable,
            %%            [<<"HELLO">>, <<"WORLD">>],
            %%            "\t\t",
            %%            [{voltrow,
            %%                [<<"Hej">>, <<"världen">>]}]}]}}
            %%
            %% @type voltresponse() =
            %%
            %%     { voltresponse,
            %%       { Protocol,
            %%         ClientData,
            %%         Status,
            %%         StatusString,
            %%         AppStatus,
            %%         AppStatusString,
            %%         SerializedException,
            %%         RoundTripTime },
            %%       [ volttable() ]
            %%     }.
            %% @end

            ?ERLVOLT_ERROR_MESSAGE("Procedure Select was not found") ->

                io:format("~nRunning the right database? (cd voltdb/doc/tutorials/hello && ./run.sh)~n~n");

            {result, _ } ->

                %%%
                %%% Result
                %%%

                Table = erlvolt:get_table(Result, 1),
                Row = erlvolt:get_row(Table, 1),
                Hello = erlvolt:get_string(Row, Table, "HELLO"),
                World = erlvolt:get_string(Row, Table, "WORLD"),

                io:format("~n~s ~s!~n~n", [Hello, World]);

            %% unexpected
            Other ->
                  io:format("Unexpected result ~p.", [Other]),
                  exit(hello_bad_result)
        end,

        %%%
        %%% Close database connection
        %%%

        erlvolt:close_pool(hello_pool)

    catch

        %%%
        %%% Exceptions
        %%%

        throw:{ connection_failed, _}=_Why ->
            io:format("~n------------------------------------------------------------------------------------~n"),
            io:format("Failed to open server connection.~nIs the VoltDB server running and accessible?"),
            io:format("~n------------------------------------------------------------------------------------~n");

        What:Why ->
            io:format("~n ~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.
