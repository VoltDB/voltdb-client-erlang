%%%-------------------------------------------------------------------------%%%
%%% File        : examples/parallel.erl                                     %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver parallel 'Hello, world!' example     %%%
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
%%%     $ make parallel                                                     %%%
%%% or                                                                      %%%
%%%     $ make                                                              %%%
%%%     $ cd examples                                                       %%%
%%%     $ erlc -I ../include -o ../ebin +debug_info parallel.erl            %%%
%%%     $ erl -pa ../ebin -s parallel run -s init stop -noshell             %%%
%%%                                                                         %%%
%%% You will see this response, 'Hello, world!' in a couple of languages:   %%%
%%%                                                                         %%%
%%%     Hello Sample                                                        %%%
%%%     ----------------------------------------------------------------    %%%
%%%     I.   start                                                          %%%
%%%     III. insert samples                                                 %%%
%%%     IV.  select                                                         %%%
%%%     V.   receive                                                        %%%
%%%     ----------------------------------------------------------------    %%%
%%%     .... Hello World!                                                   %%%
%%%     .... Hej vŠrlden!                                                   %%%
%%%     .... Hola mundo!                                                    %%%
%%%     .... Hallo Welt!                                                    %%%
%%%     .... Hello wereld!                                                  %%%
%%%     .... Salut tout le monde!                                           %%%
%%%     .... Ciao mondo!                                                    %%%
%%%     .... Go' vIvan!                                                     %%%
%%%     .... Aiya arda!                                                     %%%
%%%     .... :-) (+)!                                                       %%%
%%%     ----------------------------------------------------------------    %%%
%%%     VI.  close pool                                                     %%%
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

-module(parallel).
-export([run/0]).
-import(erlvolt).
-include("erlvolt.hrl").

run() ->

    %%%
    %%% Start driver
    %%%

    io:format("~n~nParallel Sample~n"),
    io:format("-----------------------------------------------------------------------~n"),

    io:format("I.   start~n"),
    crypto:start(),
    application:start(erlvolt),

    %%%
    %%% Connect to the database
    %%%

    erlvolt:add_pool(hello_pool, [{"localhost", 21212}], []),

    %%%
    %%% Load sample data into the database
    %%%

    io:format("III. insert samples~n"),
    erlvolt:call_procedure(hello_pool, "Insert", ["Hello", "World", "English"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Hej", "världen", "Swedish"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Hola", "mundo", "Spanish"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Hallo", "Welt", "German"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Hello", "wereld", "Dutch"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Salut", "tout le monde", "French"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Ciao", "mondo", "Italian"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Go'", "vIvan", "Klingon"]),
    erlvolt:call_procedure(hello_pool, "Insert", ["Aiya", "arda", "Quenya"]),
    erlvolt:call_procedure(hello_pool, "Insert", [":-)", "(+)", "Smiley"]),

    %%%
    %%% Asynchronous Query
    %%%

    Languages = ["English","Swedish","Spanish","German","Dutch","French","Italian","Klingon","Quenya","Smiley"],

    io:format("IV.  select~n"),

    [ ok = erlvolt:call_procedure(hello_pool, "Select", [Language], [asynchronous])
    || Language <- Languages ],


    %%%
    %%% Results
    %%%

    io:format("V.   receive~n"),
    io:format("-----------------------------------------------------------------------~n"),

    [ receive
        ?ERLVOLT_ERROR_MESSAGE("Procedure Select was not found") ->
            io:format("~nRunning the right database? (cd voltdb/doc/tutorials/hello && ./run.sh)~n~n");

        Result ->
            Table = erlvolt:get_table(Result, 1),
            Row = erlvolt:get_row(Table, 1),
            Hello = erlvolt:get_string(Row, Table, "HELLO"),
            World = erlvolt:get_string(Row, Table, "WORLD"),
            io:format(".... ~s ~s!~n", [Hello, World])
      end
    || _ <- Languages ],
    io:format("-----------------------------------------------------------------------~n"),

    %%%
    %%% Close database connection
    %%%

    io:format("VI.  close pool~n"),
    erlvolt:close_pool(hello_pool).
