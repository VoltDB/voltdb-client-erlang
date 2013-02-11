%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_internal.hrl                                      %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver internal records header              %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 05 Feb 2013                                               %%%
%%% Changed     : 05 Feb 2013                                               %%%
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


%%%----------------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------------


-record(pool, {
    pool_id,
    size,
    user,
    password,
    hosts,
    service,
    timeout,
    queue_size,
    slots,
    nagle,
    send_buffer,
    receive_buffer,
    send_timeout,
    available = queue:new(),
    waiting = queue:new()
    }).

-record(erlvolt_connection, {
    id,
    pid,
    pool_id,
    slots,
    nagle,
    send_buffer,
    receive_buffer,
    send_timeout,
    pending = 0,
    alive = true
    }).

-record(erlvolt_slot, {
    id,
    connection_id,
    connection_pid,
    pool_id,
    granted = erlang:now(),
    left = undefined,
    sent = false,
    pending = false,
    done = false
    }).

 -record(erlvolt_profile, {
    p  = 0,
    t0 = 0,
    n  = 0,
    c  = 0,
    s  = 0,
    e  = 0,
    x  = 0,
    al  = 0,
    xl  = 0,
    q  = 0,
    ql  = 0
    }).
