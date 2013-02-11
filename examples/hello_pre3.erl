%%%-------------------------------------------------------------------------%%%
%%% File        : examples/hello.erl                                        %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver minimal 'Hello, world!' example file %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 28 Apr 2012                                               %%%
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
%%%     $ make hello-barebones-pre3 # note the 'barebones'                  %%%
%%% or                                                                      %%%
%%%     $ make                                                              %%%
%%%     $ cd examples                                                       %%%
%%%     $ erlc -I ../include -o ../ebin +debug_info hello_pre3.erl          %%%
%%%     $ erl -pa ../ebin -s hello_pre3 run -s init stop -noshell           %%%
%%%                                                                         %%%
%%% You will see this response, 'Hello, world!' in Swedish:                 %%%
%%%                                                                         %%%
%%%     Hej världen!                                                        %%%
%%%                                                                         %%%
%%% For a slightly more comples example see examples/hello_plus.erl.        %%%
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

-module(hello_pre3).
-export([run/0]).

-include("erlvolt.hrl").

-author("H. Diedrich <hd2010@eonblast.com>").

run() ->

    %%%
    %%% Start driver
    %%%

    crypto:start(),
    application:start(erlvolt),

    %%%
    %%% Connect
    %%%

    erlvolt:add_pool(hello_pool, [{"localhost", 21212}]),

    %%%
    %%% Load sample data into the database
    %%%

    erlvolt:call_procedure(hello_pool, "Insert", ["Hej", "världen", "Swedish"]),

    %%%
    %%% Query
    %%%

    Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"]),
    %% note: the stored procedure is called 'Select'.

    %%%
    %%% Result
    %%%

    Table = erlvolt:get_table(Result, 1),
    Row = erlvolt:get_row(Table, 1),
    Hello = erlvolt:get_string(Row, Table, "HELLO"),
    World = erlvolt:get_string(Row, Table, "WORLD"),

    io:format("~n~s ~s!~n~n", [Hello, World]),

    %%%
    %%% Close
    %%%

    erlvolt:close_pool(hello_pool).

