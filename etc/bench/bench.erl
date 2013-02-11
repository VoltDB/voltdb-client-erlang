%%%-------------------------------------------------------------------------%%%
%%% File        : etc/bench/bench.erl                                       %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver 'Hello' and 'Voter' benchmark        %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 14 Jan 2013                                               %%%
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
%%% USAGE - MAKE                                                            %%%
%%%                                                                         %%%
%%% You can run this sample using the 'Voter' database and server that is   %%%
%%% present in every VoltDB distribution and is a VoltDB benchmark staple.  %%%
%%% For an explanation of the 'TV voter' scenario, see comments below.      %%%
%%%                                                                         %%%
%%% To run it, start the Voter server from your voltdb installation with:   %%%
%%%                                                                         %%%
%%%     $ cd voltdb/examples/voter                                          %%%
%%%     $ ./run.sh                                                          %%%
%%%                                                                         %%%
%%% Then, in a different terminal, from the driver root:                    %%%
%%%                                                                         %%%
%%%     $ make clean fast bench                                             %%%
%%%  or                                                                     %%%
%%%     $ make clean profile bench                                          %%%
%%%  or                                                                     %%%
%%%     $ make clean profile benches                                        %%%
%%%  etc.                                                                   %%%
%%%                                                                         %%%
%%%  You can of course leave the middle parts, 'clean fast' out for repeat  %%%
%%%  runs. VoltDB host settings are read from etc/bench/bench.config        %%%
%%%                                                                         %%%
%%% 'fast' builds  HiPE native compiled beams,  without debug information,  %%%
%%% which makes for  some tens of percents higher throughput  compared to   %%%
%%% the default compilation ('all') without HiPE & with debug information.  %%%
%%%                                                                         %%%
%%% 'profile' compiles similar but with the profiler enabled, which makes   %%%
%%% gives you rolling performance stats every second.                       %%%
%%%                                                                         %%%
%%% 'bench' actually triggers one specific bench,also tagged 'VSD', which   %%%
%%% stands for Voter, Steady load and Direct calls.  For more on that see   %%%
%%% doc/BENCHMARKS.html                                                     %%%
%%%                                                                         %%%
%%% 'benches' runs four voter benchmarks,  in a row:  VSD, VSM, VBM, VBD.   %%%
%%% For details also see doc/BENCHMARKS.html                                %%%
%%%                                                                         %%%
%%% There are a dozen of parameters that you can use to test your set up,   %%%
%%% starting bench.erl directly. Those options are explained further down.  %%%
%%%                                                                         %%%
%%% From 'make clean profile bench' you will see this kind of response:     %%%
%%%                                                                         %%%
%%%   Erlvolt Bench 0.9 (client 'VSD')                                      %%%
%%%   ------------------------------------------------------------------..  %%%
%%%   Client 'VSD', voter, 100,000 calls, steady, 200 workers, verbose  ..  %%%
%%%   Hosts: localhost:21212 localhost:21212                                %%%
%%%   connect ...                                                           %%%
%%%   preparation ...                                                       %%%
%%%   calls ...                                                             %%%
%%%                                                                         %%%
%%%   0.232sec:  7,434 T/sec, ø  7,443 T/sec, pend:  66, avglat: 8.243ms..  %%%
%%%   0.433sec:  9,159 T/sec, ø  8,235 T/sec, pend: 151, avglat: 6.666ms..  %%%
%%%   0.637sec: 10,206 T/sec, ø  8,877 T/sec, pend: 153, avglat: 5.825ms..  %%%
%%%   0.831sec: 13,006 T/sec, ø  9,839 T/sec, pend:  66, avglat: 4.790ms..  %%%
%%%   1.031sec: 17,139 T/sec, ø 11,261 T/sec, pend:  93, avglat: 3.684ms..  %%%
%%%   1.231sec: 18,953 T/sec, ø 12,510 T/sec, pend: 140, avglat: 2.890ms..  %%%
%%%   1.432sec: 17,893 T/sec, ø 13,254 T/sec, pend:  74, avglat: 2.548ms..  %%%
%%%   1.631sec: 18,157 T/sec, ø 13,861 T/sec, pend: 114, avglat: 2.240ms..  %%%
%%%   1.831sec: 15,904 T/sec, ø 14,081 T/sec, pend:  67, avglat: 2.074ms..  %%%
%%%   2.032sec: 18,682 T/sec, ø 14,537 T/sec, pend:  76, avglat: 1.876ms..  %%%
%%%   2.232sec: 19,301 T/sec, ø 14,965 T/sec, pend:  66, avglat: 1.727ms..  %%%
%%%   2.431sec: 19,922 T/sec, ø 15,370 T/sec, pend:  69, avglat: 1.580ms..  %%%
%%%   2.630sec: 20,093 T/sec, ø 15,726 T/sec, pend: 123, avglat: 1.456ms..  %%%
%%%   2.832sec: 20,183 T/sec, ø 16,042 T/sec, pend:  98, avglat: 1.357ms..  %%%
%%%   3.032sec: 20,013 T/sec, ø 16,304 T/sec, pend:  87, avglat: 1.276ms..  %%%
%%%   3.232sec: 19,734 T/sec, ø 16,516 T/sec, pend: 114, avglat: 1.210ms..  %%%
%%%   3.431sec: 19,385 T/sec, ø 16,684 T/sec, pend: 164, avglat: 1.167ms..  %%%
%%%   3.630sec: 20,385 T/sec, ø 16,890 T/sec, pend:  99, avglat: 1.111ms..  %%%
%%%   3.832sec: 18,447 T/sec, ø 16,970 T/sec, pend:  59, avglat: 1.118ms..  %%%
%%%   4.031sec: 20,299 T/sec, ø 17,133 T/sec, pend: 107, avglat: 1.072ms..  %%%
%%%   4.231sec: 19,197 T/sec, ø 17,230 T/sec, pend: 147, avglat: 1.035ms..  %%%
%%%   4.432sec: 20,320 T/sec, ø 17,372 T/sec, pend:  74, avglat: 0.996ms..  %%%
%%%   4.631sec: 19,750 T/sec, ø 17,473 T/sec, pend:  68, avglat: 0.964ms..  %%%
%%%   4.831sec: 19,480 T/sec, ø 17,559 T/sec, pend: 128, avglat: 0.935ms..  %%%
%%%   5.030sec: 19,565 T/sec, ø 17,636 T/sec, pend: 130, avglat: 0.909ms..  %%%
%%%   5.231sec: 19,349 T/sec, ø 17,701 T/sec, pend: 143, avglat: 0.885ms..  %%%
%%%   5.442sec: 10,070 T/sec, ø 17,406 T/sec, pend:  48, avglat: 0.873ms..  %%%
%%%   5.630sec: 18,263 T/sec, ø 17,434 T/sec, pend:  69, avglat: 0.852ms..  %%%
%%%   5.833sec:  8,931 T/sec, ø 17,139 T/sec, pend:  16, avglat: 0.843ms..  %%%
%%%                                                                         %%%
%%%   cool down ...                                                         %%%
%%%   check writes ... ok                                                   %%%
%%%                                                                         %%%
%%%   results ...  votes:     110,000 (6 contestants)                       %%%
%%%   .....Alana Bregman:      18,713                                       %%%
%%%   ....Jessie Alloway:      18,389                                       %%%
%%%   ...Tabatha Gehling:      18,340                                       %%%
%%%   ....Jessie Eichman:      18,305                                       %%%
%%%   .....Edwina Burnam:      18,162                                       %%%
%%%   ......Kelly Clauss:      18,091                                       %%%
%%%                                                                         %%%
%%%   close pool ...                                                        %%%
%%%                                                                         %%%
%%%   Client 'VSD', voter, 100,000 calls, steady, 200 workers, verbose  ..  %%%
%%%   ------------------------------------------------------------------..  %%%
%%%   Client 'VSD' overall: 17,018 T/sec throughput, 0.00% fails, 5.876s..  %%%
%%%   Erlvolt 0.3.0, start 2013-02-02 20:30:58, end 2013-02-02 20:31:04 ..  %%%
%%%   [+++++++++++++++++]                                                   %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% USAGE - MULTI VM SHELL BATCH                                            %%%
%%%                                                                         %%%
%%% 'make bench-mvm' starts multiple Erlang VMs (emulators), e.g.:          %%%
%%%                                                                         %%%
%%%     $ make bench-mvm VMS=2 CORES=4                                      %%%
%%%                                                                         %%%
%%% VMS   (5)      number of virtual machines (Erlang emulators) started    %%%
%%% CORES (1)      number of cores used per VM (erl parameter +S)           %%%
%%% CALLS (100000) number of transactions per VM for the benchmark          %%%
%%% SPAWN (100)    number of parallel ('steady') workers = max server load  %%%
%%%                                                                         %%%
%%% This can be the most effective way to run real benchmarks.  This make   %%%
%%% command uses the etc/bench/benchstart shell script. It can make sense   %%%
%%% to first experiment with  direct parameters  as described below, then   %%%
%%% hardcode the results into  etc/bench/benchstart to prepare for a real   %%%
%%% bench run, e.g. in a cloud setup.                                       %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% USAGE - DIRECT PARAMETERS                                               %%%
%%%                                                                         %%%
%%% As you can see in the etc/bench/Makefile,  there are multiple options   %%%
%%% that you can use to run a fine tuned benchmark calling bench directly.  %%%
%%%                                                                         %%%
%%% Note that this bench module  cannot  perform automatic fine tuning as   %%%
%%% the original  Java  client can,  which you can run in the same folder   %%%
%%% as the server, using './run.sh client'.                                 %%%
%%%                                                                         %%%
%%% However, it doesn't take long to zero in to a best load and the 'VSD'   %%%
%%% configuration would be your starting point (voter, steady, direct).     %%%
%%% It produces a steady load from parallel worker threads  that are each   %%%
%%% blocking individually, until they get a response. This is the fastest   %%%
%%% way on the *client*-side  to  protect the server from  overload  from   %%%
%%% 'fire-hosing' clients.                                                  %%%
%%%                                                                         %%%
%%% The parameters are:                                                     %%%
%%%                                                                         %%%
%%%     $ erl -pa ../../ebin -s bench run <vmid> <bench> <calls> <rhythm>\  %%%
%%%         <burstsize> <delay> <mode> <queuesize> <slots> <limit> \        %%%
%%%         <verbosity> <dumpinterval> -s init stop -noshell +p 1000000  \  %%%
%%%         -smp +s $cores -config bench                                    %%%
%%%                                                                         %%%
%%% E.g. the standard bench is kicked in with:                              %%%
%%%                                                                         %%%
%%%     $ erl -pa ../../ebin -s bench run VSD voter 100000 steady 100 x  \  %%%
%%%         direct x x x nonverbose 500 -s init stop -noshell +P 1000000 \  %%%
%%%         -smp +S $CORES -config bench                                    %%%
%%%                                                                         %%%
%%% <vmid>          Virtual Machine ID, trails screen logs                  %%%
%%% <bench>         type of benchmark calls, 'voter' or 'hello'             %%%
%%% <calltarget>    number of calls before benchmark ends                   %%%
%%% <rhythm>        timed 'bursts' or 'steady' synched calls                %%%
%%% <burstsize>     calls per burst or # of steady workers                  %%%
%%% <delay>         millisec delay between bursts                           %%%
%%% <mode>         'direct' calls or using 'managed' driver queue           %%%
%%% <queuesize>     max processes that can wait in queue                    %%%
%%% <slots>         max pending request before queueing next ones           %%%
%%% <limit>        'managed': queue size, 'direct': # pending               %%%
%%% <verbosity>     quiet, nonverbose, verbose, veryverbose                 %%%
%%% <dumpinterval>  for 'make profile' msec between stats                   %%%
%%%                                                                         %%%
%%% For examples see etc/bench/Makefile.                                    %%%
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
%%%                                                                         %%%
%%% VOTER - DESCRIPTION                                                     %%%
%%%                                                                         %%%
%%% The Voter sample application simulates a phone-based election process.  %%%
%%% Voters  (based  on  phone numbers generated  randomly  by the  client   %%%
%%% application) are allowed a limited number of votes.                     %%%
%%%                                                                         %%%
%%% The main goal of the voter application is to demonstrate the            %%%
%%% performance possibilities of VoltDB:                                    %%%
%%%                                                                         %%%
%%% Stored procedures are invoked  asynchronously.  The client can record   %%%
%%% and report thoughput and latency, when compiled with the driver-built   %%%
%%% in profiler (`make clean profile`).                                     %%%
%%%                                                                         %%%
%%% Usually, you will run this sample on a single node.  However,  it can   %%%
%%% easily be reconfigured to run any combination of clients and servers.   %%%
%%% The servers support multiple clients,  and single-node  or multi-node   %%%
%%% server clusters are supported.                                          %%%
%%%                                                                         %%%
%%% To test the server with a different configuration, simply edit run.sh   %%%
%%% listing one  server node as the  lead node  when building the catalog   %%%
%%% and using a comma-separated list of the server addresses  as argument   %%%
%%% to the client. See the comments in the build script for details.        %%%
%%%                                                                         %%%
%%% The client  starts  'fire-hosing'  the  VoltDB server  using  pre-set   %%%
%%% parameters.  You  can  experiment  with  different  loads  to  better   %%%
%%% understand the  best tuning  to  get  the  most  out of your specific   %%%
%%% VoltDB deployment.                                                      %%%
%%%                                                                         %%%
%%% Rate-limiting your clients (or adding cluster nodes)  is essential to   %%%
%%% preventing  'fire-hosing'  your server (cluster)  and will ensure you   %%%
%%% get  proper  application  responsiveness  (latency)  while maximizing   %%%
%%% througput (TPS) for your hardware configuration.                        %%%
%%%                                                                         %%%
%%% The "Voter" application is specifically  designed for benchmarking to   %%%
%%% give you a good feel  for the type  of performance  VoltDB is capable   %%%
%%% of on your hardware.                                                    %%%
%%%                                                                         %%%
%%% For more on  benchmarking and  tips on application tuning,  visit the   %%%
%%% VoltDB blog: http://voltdb.com/search/node/benchmark and                %%%
%%% http://voltdb.com/search/node/tuning                                    %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(bench).
-export([run/0, run/1,
         prep_hello/1, call_hello/5, done_hello/2,
         prep_voter/1, call_voter/5, done_voter/2,
         voter_return_text/1]).

-include("erlvolt.hrl").

-author("H. Diedrich <hd2010@eonblast.com>").

%% Voter return values
-define(VOTE_SUCCESSFUL, 0).
-define(ERR_INVALID_CONTESTANT, 1).
-define(ERR_VOTER_OVER_VOTE_LIMIT, 2).

%%%----------------------------------------------------------------------------
%%% Benchmark Start Function
%%%----------------------------------------------------------------------------
%% @spec run() -> any().

run() ->

    run(['VSD', 'voter', '100000', 'steady', 200, 'x', 'direct', 'x', 'x', 'x', 'verbose', '200']).

%% These are atoms as they come as atoms from the shell command line.
%% @spec run([VMID, Bench, Mode, ADumpInterval, ABurstSize, ACallTarget, APendingTarget, ASlots, AQueueTarget, AQueueSize]) -> ok.
%%   VMID = non_neg_integer()
%%   Bench = hello | voter
%%   Mode = managed | direct
%%   ADumpInterval = atom()
%%   ABurstSize = atom()
%%   ACallTarget = atom()
%%   APendingTarget = atom()
%%   ASlots = atom()
%%   AQueueTarget = atom()
%%   AQueueSize = atom()

run([VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval]) ->

    %% Cast from command line atoms and verify
    DumpInterval = list_to_integer(atom_to_list(ADumpInterval)),
    BurstSize = list_to_integer(atom_to_list(ABurstSize)),
    CallTarget1 = list_to_integer(atom_to_list(ACallTarget)),
    Bursts = trunc(CallTarget1 / BurstSize),
    CallTarget = Bursts * BurstSize,
    Slots = try list_to_integer(atom_to_list(ASlots)) catch _:_ -> 'n/a' end,
    QueueSize = try list_to_integer(atom_to_list(AQueueSize)) catch _:_ -> 'n/a' end,
    WaitTarget = try list_to_integer(atom_to_list(ATarget)) catch _:_ -> 'n/a' end,
    Prep = case Bench of hello -> prep_hello; voter -> prep_voter end,
    Call = case Bench of hello -> call_hello; voter -> call_voter end,
    Done = case Bench of hello -> done_hello; voter -> done_voter end,
    Verbosity = case AVerbosity of quiet -> 0; nonverbose -> 1; verbose -> 2; veryverbose -> 3; debug -> 4 end,
    Delay = try list_to_integer(atom_to_list(ADelay)) catch _:_ -> 'n/a' end,

    trace(2, Verbosity, "~n~nErlvolt Bench 0.9 (client ~p)",[VMID]),
    crypto:start(),
    application:start(erlvolt),

    %% timed dumps, or, when not compiled with 'profile', print dots.
    Tid = spawn(fun() ->
        receive start -> done end,
        erlvolt_profiler:reset(),
        timer:apply_interval(DumpInterval, erlvolt_profiler, ?ERLVOLT_PROFILER_DUMP_FUNCTION, [VMID]),
        receive stop -> done end
        end),
    register(timerhull, Tid),

    client(VMID, Bench, Prep, Call, Done, Mode, Rhythm, AVerbosity, Verbosity, DumpInterval, CallTarget, BurstSize, Delay, Slots, QueueSize, WaitTarget),

    ok.

start_delay(0, 0, _) ->

    ok;

start_delay(Granul, MinDelay, Verbosity) when is_integer(Granul) ->

    %% Set start time
    T = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    T0 = (T div Granul) * Granul + MinDelay,
    print_time("Start at: ", T0, " ", Verbosity, 1),

    %% Sleep until T0. Make Dots.
    start_delay_loop(T0,0,Verbosity),

    print_time("~nStarting: ", "~n", Verbosity, 0).

start_delay_loop(T0,I,Verbosity) ->
    T = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    if T < T0 ->
        timer:sleep(5),
        Verbosity > 1 andalso I rem 50 == 0 andalso io:format(".",[]),
        start_delay_loop(T0,I+1,Verbosity);
    true -> ok
    end.

%%%----------------------------------------------------------------------------
%%% Bench Client
%%%----------------------------------------------------------------------------
%%% The bench client is called multiple times in parallel, multiple times sequentially.
%%% The parallel calls are summarily called a 'burst' for the purpose of this benchmark.
%%% The bursts test the queue, if it is used ('managed' mode), as any number above the
%%% 'slots' are immediately queued.

client(ClientID, Bench, Prep, Call, Done, Mode, Rhythm, AVerbosity, Verbosity, DumpInterval, CallTarget, BurstSize, Delay, Slots, QueueSize, WaitTarget) ->

    DumpFrequency = dump_frequency(DumpInterval),
    Bursts = trunc(CallTarget / BurstSize),
    BurstSizeUse = case Rhythm of steady -> workers; bursts -> bursts end,

    Pars = io_lib:format("Client ~p, ~p, ~s calls, ~p, ~s ~s, delay ~s, ~p, queue ~s, slots ~s, limit ~s, ~p, ~p stats/sec",
        [ClientID, Bench, ts(CallTarget), Rhythm, ts(BurstSize), BurstSizeUse, ts(Delay,"ms"), Mode, ts(QueueSize), ts(Slots), ts(WaitTarget), AVerbosity, DumpFrequency]),
    trace(2, Verbosity, ["-"||_<-lists:seq(1,length(lists:flatten(Pars)))]),
    trace(1, Verbosity, Pars),
    register(master, self()),

    Hosts = case application:get_env(erlvolt,hosts) of
        {ok,Hosts1} ->
            Hosts1;
        undefined ->
            [{"localhost", 21212}]
    end,

    trace(2, Verbosity, "Hosts: ~s", [[D++":"++integer_to_list(P)++" "||{D,P}<-Hosts]]),
    XSlots   = case Slots of 'n/a' -> 200; _ -> Slots end,
    Opts = [{slots, XSlots}, {queue_size, QueueSize}],

    try
        %%%
        %%% Connect to the database
        %%%

        trace(2, Verbosity, "connect ..."),
        PoolID = ClientID,
        erlvolt:add_pool(PoolID, Hosts, Opts),

        %%%
        %%% Load sample data into the database
        %%%

        trace(2, Verbosity, "preparation ..."),
        apply(?MODULE, Prep, [PoolID]),

        %%%
        %%% 'Before' snapshot
        %%%

        Count1 = case Bench of voter -> query_votes(PoolID); _ -> 0 end,

        %%%
        %%% Wait for other client machines
        %%%

        {ok,StartDelay} = application:get_env(erlvolt,start_delay),
        start_delay(StartDelay, StartDelay, Verbosity),

        %%%
        %%% Measure
        %%%

        trace(2, Verbosity, "calls ... ", [], ?ERLVOLT_PROFILER_CR),
        timerhull ! start,

        T0 = now(),
        case {Mode,Rhythm} of
            %%% Chunked, managed executions
            {managed,bursts} ->
                [   begin
                        %% This is one 'burst' of BurstSize parallel calls,
                        %% after, there is a wait until the QUEUE recedes.
                        [ spawn(?MODULE, Call, [nil, 1, PoolID, managed, Verbosity]) || _ <- lists:seq(1,BurstSize) ],
                        ?ERLVOLT_PROFILER_WAITQUEUED(WaitTarget),
                        timer:sleep(Delay)
                    end
                 || _ <- lists:seq(1,Bursts) ];

            %%% Chunked, direct executions
            {direct,bursts} ->
                ConList = erlvolt:get_connections(PoolID),
                [   begin
                        %% This is one 'burst' of BurstSize parallel calls,
                        %% after, there is a wait until the COUNT OF PENDING requests from the servers recedes.
                        [ spawn(?MODULE, Call, [nil, 1, {ConList, Round}, direct, Verbosity]) || Round <- lists:seq(1,BurstSize) ],
                        ?ERLVOLT_PROFILER_WAITPENDING(WaitTarget),
                        timer:sleep(Delay)
                    end
                || _ <- lists:seq(1,Bursts) ];

            %%% Synchronized, managed executions
            {managed,steady} ->
                %% This is one 'burst' of BurstSize parallel calls,
                [ spawn(?MODULE, Call, [self(), Bursts, PoolID, managed, Verbosity]) || _ <- lists:seq(1,BurstSize) ],
                wait_done(BurstSize);

            %%% Synchronized, direct executions
            {direct,steady} ->
                ConList = erlvolt:get_connections(PoolID),
                %% This is one 'burst' of BurstSize parallel calls,
                [ spawn(?MODULE, Call, [self(), Bursts, {ConList, Round}, direct, Verbosity]) || Round <- lists:seq(1,BurstSize) ],
                wait_done(BurstSize)
        end,

        %%%
        %%% Wait for last responses
        %%%

        trace(2, Verbosity, ?ERLVOLT_PROFILER_NCR ++ "cool down ... ", [], ?ERLVOLT_PROFILER_CR), % profile dots may continue
        {ok, Oks, Fails1} = drain(CallTarget),
        TN = now(),
        TDs = timer:now_diff(TN,T0) / 1000000,
        T0H = human_time(T0),
        TNH = human_time(TN),
        timerhull ! stop,
        receive after 0 -> nil end, % yield

        %%%
        %%% Check fail count
        %%%

        trace(2, Verbosity, ?ERLVOLT_PROFILER_NCR ++ "check writes ... ", [], ""),
        Count2 = case Bench of voter -> query_votes(PoolID); _ -> Oks end,

        Fails = if
            Fails1 == CallTarget ->
                io:format("### benchmark failure: all writes failed. ###~n"),
                Fails1;
            Fails1 > 0.01 * CallTarget ->
                io:format("### benchmark failure: too many failed writes (~s). ###~n", [ erlvolt_profiler:ts(Fails1) ]),
                Fails1;
            true ->


        %%%
        %%% Double check database fill change
        %%%

                Fails2 = if
                    Bench == voter andalso Count2 == Count1 ->
                        io:format("### benchmark failure: no database writes. ###~n"),
                        CallTarget;
                    Bench == voter andalso Count2 < Count1 + 0.99 * CallTarget ->
                        io:format("### benchmark failure: too many missing writes (~s). ###~n", [ erlvolt_profiler:ts(CallTarget - Count2 + Count1) ]),
                        max(0,CallTarget - Count2 + Count1); % sic
                    true ->
                        trace(2, Verbosity, "ok"),
                        0
                end,
                max(Fails1, Fails2)
        end,

        %%%
        %%% Show results, for illustration
        %%%

        trace(2, Verbosity, "results ... ", [], ""),
        apply(?MODULE, Done, [PoolID, Verbosity]),

        %%%
        %%% Shut Down
        %%%

        trace(2, Verbosity, "close pool ..."),
        erlvolt:close_pool(PoolID),

        %%%
        %%% Result Log
        %%%

        F = trunc((CallTarget - Fails) / TDs),
        FPrc = if CallTarget > 0 -> Fails / CallTarget; true -> 0 end * 100,
        {ok,Ver} = application:get_key(erlvolt, vsn),
        Bar = ["+"||_<-lists:seq(1,trunc(F/1000))],
        Extra = case Bench of
            voter -> io_lib:format(", database: +~s new votes", [ts(Count2-Count1)]);
            _ -> ""
        end,

        trace(2, Verbosity, Pars),
        trace(2, Verbosity, ["-"||_<-lists:seq(1,length(lists:flatten(Pars)))]),
        io:format("~nClient ~p overall: ~s T/sec throughput, ~.2f% fails, total transactions: ~s, fails: ~s, total time: ~.3fsec ~nErlvolt ~s, bench started ~s, ended ~s~s~n[~s]~n",
             [ClientID, erlvolt_profiler:ts(F), FPrc, erlvolt_profiler:ts(CallTarget), erlvolt_profiler:ts(Fails), TDs, Ver, T0H, TNH, Extra, Bar]),
        file:write_file("./bench.totals", io_lib:fwrite("~p\n", [F]), [append])

    catch
        throw:{ open_failed, _, _}=Why ->
            io:format("Failed to open server connection.~nIs the VoltDB server running and accessible?"),
            io:format("Error details: ~n ~w ~w ~n ~p", [throw, Why, erlang:get_stacktrace()]),
            exit({throw, Why})
    end.

wait_done(0) ->
    ok;

wait_done(C) ->
    receive
        done -> wait_done(C-1)
    end.

-ifdef(profile).
drain(CallTarget) ->
    receipts(CallTarget,0,0). %%% 
-else.
drain(CallTarget) ->
    receipts(CallTarget,0,0).
-endif.

receipts(0,Oks,Fails) ->
    {ok,Oks,Fails};

receipts(Calls,Oks,Fails) ->
    receive
        ok ->
            receipts(Calls -1,Oks+1,Fails);
        fail ->
            if Fails < 1 orelse (Fails rem 10000) == 0 -> io:format("#"); true -> nil end,
            receipts(Calls -1,Oks,Fails+1);
        Else ->
            io:format("~n~~*** Bad benchmark client receipt: ~p ***~n~n", [Else]),
            receipts(Calls,Oks,Fails+1)
        after 5000 ->
            io:format("~n~n~n*** Benchmark client receipt timeout (~p calls left to receive)~n~n",[Calls]),
            exit(client_receive_timeout)
    end.

-ifdef(profile).
dump_frequency(DumpInterval) ->
    1000 / DumpInterval.
-else.
dump_frequency(_) ->
    "n/a".
-endif.


%%%----------------------------------------------------------------------------
%%% Log to Screen
%%%----------------------------------------------------------------------------

trace(V,V0,S) ->
    trace(V,V0,S,[],"~n").

trace(V,V0,S,L) ->
    trace(V,V0,S,L,"~n").

trace(V,V0,S,L,CR) ->
    V0 >= V andalso
        io:format(S ++ CR, L).

ts(N,Unit) ->
    case N of
        'n/a' -> N;
        _ -> erlvolt_profiler:ts(N) ++ Unit
    end.

ts(N) ->
    case N of
        'n/a' -> N;
        _ -> erlvolt_profiler:ts(N)
    end.

%%%----------------------------------------------------------------------------
%%% Callbacks for the Hello Benchmark
%%%----------------------------------------------------------------------------
%%% This 'hello' bench works with the catalog and server of voltdb/doc/tutorial/helloworld.
%%% Cd there, start the server that with ./run.sh and then 'make hello' in the driver root.

prep_hello(PoolID) ->

    %% Initialize the database
    erlvolt:call_procedure(PoolID, "Insert", ["Hello",  "World", "English"]),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(0),
    erlvolt:call_procedure(PoolID, "Insert", ["Bonjour","Monde", "French"]),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(0),
    ok.

call_hello(Parent,0, _, _, _) ->
    case Parent of
        nil -> ok;
        _ -> Parent ! done
    end;

%% @spec call_hello(Bursts, Link, Mode, Verbosity) -> ok.
%%  Bursts = non_neg_integer()
%%  Link = PoolID::any() |  {_ConnList::list(), _Round:integer()}
%%  Mode = managed | direct
%%  Verbosity = integer()
call_hello(Parent, Calls, Link, Mode, Verbosity) ->

    Opts = case Mode of
        managed -> []; %% = [monitored, synchronous, queue]
        direct -> [force, direct] %% = [direct, force, synchronous]
    end,

    R = (catch erlvolt:call_procedure(Link, "Select", ["French"], Opts)),

    react_to_response(R, Verbosity),

    call_hello(Parent, Calls-1, Link, Mode, Verbosity).

done_hello(_,_) ->

    nil.

%%%----------------------------------------------------------------------------
%%% Callbacks for the Voter Benchmark
%%%----------------------------------------------------------------------------
%%% This 'voter' bench works with the catalog and server of voltdb/examples/voter
%%% Cd there, start the server that with ./run.sh and then 'make voter' in the driver root.

%% @spec prep_voter(PoolID) -> ok
prep_voter(PoolID) ->

    %% Seed pseduo random generator
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),

    %% In case it was filled before, e.g. by 'make voter'
    % erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM contestants"]),
    % erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM area_code_state"]),
    % erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM votes"]),

    %% Initialize the database
    Result = erlvolt:call_procedure(PoolID, "Initialize", [6,voter_candidates()]),
    try
        {result, {voltresponse, {0,<<0,0,_,0,0,0,0,0>>,1,<<>>,128,<<>>,<<>>,_}, [{volttable,[<<>>],[6],[{voltrow,[6]}]}]}} = Result
    catch
        error:{badmatch,_} ->
            case erlvolt:get_statusstring(Result) of
                <<"Procedure Initialize was not found">> ->
                    io:format("### Calling stored procedure 'Initialize' failed. Right database running? ###~n"),
                    exit(cant_initialize);
                Status ->
                    io:format("### Calling stored procedure 'Initialize' failed: ~p~n",[Status])
            end
    end,
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(0),
    ok.

%% Names of candidates.
%% @spec voter_candidates() -> string()
voter_candidates() ->

    "Edwina Burnam,Tabatha Gehling,Kelly Clauss," ++
    "Jessie Alloway,Alana Bregman,Jessie Eichman,Allie Rogalski,Nita Coster," ++
    "Kurt Walser,Ericka Dieter,Loraine NygrenTania Mattioli".

%% Get a pseudo random candidate number
%% @spec voter_get_candidate() -> 1..6
voter_get_candidate() ->

    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(6).

%% List of Area Codes.
%% @spec voter_area_codes() -> list()
voter_area_codes() ->

    [907, 205, 256, 334, 251, 870, 501, 479, 480, 602, 623, 928,
    520, 341, 764, 628, 831, 925, 909, 562, 661, 510, 650, 949, 760, 415, 951, 209,
    669, 408, 559, 626, 442, 530, 916, 627, 714, 707, 310, 323, 213, 424, 747, 818,
    858, 935, 619, 805, 369, 720, 303, 970, 719, 860, 203, 959, 475, 202, 302, 689,
    407, 239, 850, 727, 321, 754, 954, 927, 352, 863, 386, 904, 561, 772, 786, 305,
    941, 813, 478, 770, 470, 404, 762, 706, 678, 912, 229, 808, 515, 319, 563, 641,
    712, 208, 217, 872, 312, 773, 464, 708, 224, 847, 779, 815, 618, 309, 331, 630,
    317, 765, 574, 260, 219, 812, 913, 785, 316, 620, 606, 859, 502, 270, 504, 985,
    225, 318, 337, 774, 508, 339, 781, 857, 617, 978, 351, 413, 443, 410, 301, 240,
    207, 517, 810, 278, 679, 313, 586, 947, 248, 734, 269, 989, 906, 616, 231, 612,
    320, 651, 763, 952, 218, 507, 636, 660, 975, 816, 573, 314, 557, 417, 769, 601,
    662, 228, 406, 336, 252, 984, 919, 980, 910, 828, 704, 701, 402, 308, 603, 908,
    848, 732, 551, 201, 862, 973, 609, 856, 575, 957, 505, 775, 702, 315, 518, 646,
    347, 212, 718, 516, 917, 845, 631, 716, 585, 607, 914, 216, 330, 234, 567, 419,
    440, 380, 740, 614, 283, 513, 937, 918, 580, 405, 503, 541, 971, 814, 717, 570,
    878, 835, 484, 610, 267, 215, 724, 412, 401, 843, 864, 803, 605, 423, 865, 931,
    615, 901, 731, 254, 325, 713, 940, 817, 430, 903, 806, 737, 512, 361, 210, 979,
    936, 409, 972, 469, 214, 682, 832, 281, 830, 956, 432, 915, 435, 801, 385, 434,
    804, 757, 703, 571, 276, 236, 540, 802, 509, 360, 564, 206, 425, 253, 715, 920,
    262, 414, 608, 304, 307].


%% Get pseudo randomly one of the area codes
%% @spec voter_get_phone() -> 2050000000..9079999999.
voter_get_phone() ->

    lists:nth(random:uniform(305), voter_area_codes()) * 10000000 + random:uniform(9999999).

call_voter(Parent,0, _, _, _) ->
    case Parent of
        nil -> ok;
        _ -> Parent ! done
    end;

%% @spec call_voter(Bursts, Link, Mode, Verbosity) -> ok.
%%  Bursts = non_neg_integer()
%%  Link = PoolID::any() |  {_ConnList::list(), _Round:integer()}
%%  Mode = managed | direct
%%  Verbosity = integer()
call_voter(Parent, Calls, Link, Mode, Verbosity) ->

    Candidate = voter_get_candidate(),
    Phone = voter_get_phone(),

    %% io:format("Voting: candidate ~p, phone ~p~n", [Candidate, Phone]),

    Opts = case Mode of
        managed -> []; %% = [monitored, synchronous, queue]
        direct -> [force, direct] %% = [direct, force, synchronous]
    end,

    R = (catch erlvolt:call_procedure(Link, "Vote", [Phone, Candidate, 20000], Opts)),
    % trace(3, Verbosity, "Return: ~p~n", [voter_return_text(erlvolt:get_one(R))]),
    react_to_response(R, Verbosity),

    call_voter(Parent, Calls-1, Link, Mode, Verbosity).

react_to_response(Result, Verbosity) ->

    case Result of

        %% empty result (not expected)
        {result, { voltresponse, _Response, []}} = Result ->
            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
            trace(3, Verbosity, "RECEIVED EMPTY RESPONSE"),
            io:format("Benchmark client received unexpected result: ~p~n", [Result]),
            exit(bench_unexpected_empty_response);

        %% good result
        {result, { voltresponse, _, [ Table | _ ] }} = _Result ->

            trace(3, Verbosity, "RECEIVED RESPONSE"),
            _Row = erlvolt:get_row(Table, 1),
            _Time = erlvolt_wire:roundtrip(_Result),
            ?ERLVOLT_PROFILER_COUNT_SUCCESS(_Time),
            master ! ok;

        %% drop for dull queue
        queue_full ->
            trace(3, Verbosity, "DROPPING FOR FULL QUEUE"),
            master ! fail;

        %% drop for dull queue
        queue_timeout ->
            trace(3, Verbosity, "DROPPING FOR QUEUE WAIT TIMEOUT"),
            master ! fail;

        %% unexpected other result
        Other ->
            ?ERLVOLT_PROFILER_COUNT_FAILURE(),
            trace(3, Verbosity, "RECEIVED UNEXPECTED RESPONSE"),
            io:format("Benchmark client received unexpected result: ~p.", [Other]),
            exit(hello_bad_result)
    end.

voter_return_text(?VOTE_SUCCESSFUL) -> "vote successful";
voter_return_text(?ERR_INVALID_CONTESTANT) -> "invalid contestant";
voter_return_text(?ERR_VOTER_OVER_VOTE_LIMIT) -> "voter over vote limit".

done_voter(PoolID, Verbosity) ->

    C = query_contestants(PoolID),
    V = query_votes(PoolID),
    trace(2, Verbosity, " votes: ~11s (~p contestants)", [erlvolt_profiler:ts(V), C]),

    Result1 = erlvolt:call_procedure(PoolID, "Results", []),
    if Verbosity >= 2 -> dump(Result1); true -> nil end,
    ?ERLVOLT_PROFILER_COUNT_SUCCESS().


query_contestants(PoolID) ->

    Result = erlvolt:call_procedure(PoolID, "@AdHoc", ["select COUNT(*) as C from contestants"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
    Count = erlvolt:get_integer(Row, Table, "C"),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(),
    Count.

query_votes(PoolID) ->

    Result = erlvolt:call_procedure(PoolID, "@AdHoc", ["select COUNT(*) as C from votes"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
    Count = erlvolt:get_integer(Row, Table, "C"),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(),
    Count.

dump( {result, {voltresponse,_,Tables}}) ->

    [ dump(T) || T <- Tables ];

dump({ volttable, _, _, Rows }) ->

    [ io:format("~18...s: ~11s~n",[Name, erlvolt_profiler:ts(Votes)]) || {voltrow, [Name, _, Votes]} <- Rows ].

print_time(Prompt, Append, VerbositySetting, VerbosityThreshold) when VerbositySetting > VerbosityThreshold ->

    Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    print_time(Prompt, Time, Append, VerbositySetting, VerbosityThreshold);

print_time(_,_,_,_) ->

        ok.

print_time(Prompt, Time, Append, VerbositySetting, VerbosityThreshold) when VerbositySetting > VerbosityThreshold ->

    HT = calendar:gregorian_seconds_to_datetime(Time),
    {{Y,M,D},{H,I,S}} = HT,
    io:format(Prompt ++ "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B" ++ Append, [Y,M,D,H,I,S]);

print_time(_,_,_,_,_) ->

	ok.

human_time(Now) ->

    {{Y,M,D},{H,I,S}} = calendar:now_to_datetime(Now),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,M,D,H,I,S]).
