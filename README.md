voltdb-client-erlang Erlvolt 0.3.1/beta
=======================================

**Release: 'Erlvolt 0.3.1/beta'**  
**Author: H. Diedrich**  
**Production: Eonblast Corporation**  
**Copyright: (c) 2013 VoltDB, Inc**  
**Licence: MIT**  
**Date: 4 Feb 2013**  

This is an [Erlang](http://www.erlang.org) [VoltDB](hhtp://www.voltdb.com) driver provided by [Eonblast](http://www.eonblast.com). It is [easy][Samples] to use but provides strong [connection pooling][Adding_a_Pool] and a broad array of [options][options]. It is optimized for a central node architecture and super high velocity OLTP.

While databases generally can be accessed via ODBC in Erlang, you should see better performance when using a *driver* like Erlvolt. For [samples][Samples], [API description][Usage] and fine tuning [options][Options] see below. 

[Erlvolt](http://github.com/Eonblast/Erlvolt) was initiated and [created][History] by [Eonblast](http://www.eonblast.com), in this second incarnation with sponsorship by [VoltDB](http://www.voltdb.com).

**This is the master branch. Should you run into problems, please report them by opening an [issue](git://github.com/Eonblast/Erlvolt/issues) at github and try if they go away by checking out the 'stable' branch. Thank you.**

<hr/>

 **Download:** <https://github.com/Eonblast/Erlvolt/archives/master>  
 **Issues:** <https://github.com/Eonblast/Erlvolt/issues>  
 **Repository:** <https://github.com/Eonblast/Erlvolt>  

<hr/>

## Contents

* [Installation][]
* [Files][]
* [Samples][]
* [Usage][]
* [Options][]
* [History][]
* [Tests][]
* [License][]

<hr/>

## Installation                                        <a name=Installation></a>

## Getting VoltDB

    $ git clone git://github.com/VoltDB/voltdb.git voltdb  

For download via the VoltDB website, see below.

## Getting Erlvolt

    $ git clone git://github.com/VoltDB/voltdb-client-erlang.git erlvolt

For the git repository of the newest version, see below.

## Files                                               <a name=Files></a>

      Makefile                        build rules
      README.html                     this file
      README.md                       or this
      
      doc:
          BENCHMARK-README.html       Benchmark How To
          BENCHMARK1.html             Report of an Amazon EC2 benchmark
          BENCHMARK1.md               markdown source
          BENCHMARK2.html             Report of another Amazon EC2 benchmark
          BENCHMARK2.md               markdown source
          CHANGES.html                Changes between versions of this package
          CHANGES.md                  markdown source
          LICENSE                     LICENSE, ASCII
      
      ebin:
          empty                       git placeholder
      
      etc:
          ct_default.css              styles for TC sample in README.html and md
          grep                        grep batch over all erl, hrl, md sources
          include.mk                  Makefile include
          markdown.lua                Lua markdown script to create html from md
          markedoc.sed                sed script to create edoc from md
          replace                     sed in-place replace over all erl, hrl, md sources
      
          bench:               
                  Makefile            build rules AND SAMPLE
                  README.html         Benchmark How To
                  README.md           markdown source
                  bench.config        host config and start sync
                  bench.erl           benchmark module
                  benchstart          multi-vm bench start
          test:
              basics_SUITE.erl	    basic functionality suite
              environment_SUITE.erl   environment and setup tests
      
      examples:
          Makefile                    build rules
          hello.erl                   barebones hello world
          hello_plus.erl              slightly more robust hello world
          parallel.erl                parallel hello word sample
          voter.erl                   VoltDB staple voter sample
      
      include:
          erlvolt.hrl                 higher level driver include
          erlvolt_wire.hrl            wire protocol level driver include
      
      priv:
          empty                       git placeholder
      
      src:
          Makefile                    
          erlvolt.erl                 main driver module
          erlvolt_conn.erl            socket connection
          erlvolt_profiler.erl        optional statistics
          erlvolt_wire.erl            protocol level bit wrangling
          erlvolt.app.src             template for app file
          erlvolt_app.erl             application behavior
          erlvolt_conn_mgr.erl        connection manager
          erlvolt_sup.erl             supervisors
      

## Samples                                             <a name=Samples></a>

### Hello World

This is a hello world program. Follow the steps below to try it out. 
    
    -module(hello).
    -export([run/0]).
    -import(erlvolt).
    -include("erlvolt.hrl").
    
    run() ->
    
        crypto:start(),
        application:start(erlvolt),
    
        erlvolt:add_pool(hello_pool, [{"localhost", 21212}]),
    
        erlvolt:call_procedure(hello_pool, "Insert", ["Hej", "världen", "Swedish"]),
    
        Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"]),
    
        Table = erlvolt:get_table(Result, 1),
        Row = erlvolt:get_row(Table, 1),
        Hello = erlvolt:get_string(Row, Table, "HELLO"), 
        World = erlvolt:get_string(Row, Table, "WORLD"),
    
        io:format("~n~s ~s!~n~n", [Hello, World]),
    
        erlvolt:close_pool(hello_pool).

We'll be coming back to running this on your machine in a minute. Before we do, let's look at the basic building blocks first:

### Executing an SQL Statement

In VoltDB, in production, you are using stored procedures, written in Java. For more
information about that please check out the [VoltDB docs][]. Such stored procedure is invoked
like this using Erlvolt:

        Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"]).

This is a snychronous, blocking call that returns the result data. The first parameter to the function is the pool ID atom, the second the name of the stored procedure that we want to invoke, as a string. The last a list of parameters we want to send to the SP.

Note that the stored procedure goes by the name 'Select' and that is the reason why 'Select' is the second parameter above. The query is `SELECT HELLO, WORLD FROM HELLOWORLD WHERE DIALECT = ?;` as you can see in `voltdb/doc/tutorials/helloworld/Select.java`.

For the exact spec, see below, [Usage][]. Regarding the 'pool', also see below.

### Executing asynchronously

In Erlang, you would usually be executing *synchronous* calls from many parallel worker
processes, rather than asynchronous ones. Due to the architecture of Erlang this will
amount to asynchronous action. But the driver can be used to make asynchronous calls,
too. In which case the execute function returns as fast as possible, and the result is
coming in via message passing, as soon as it becomes available from the VoltDB server:


        erlvolt:call_procedure(hello_pool, "Select", ["Swedish"], [asynchronous]),
        
        receive Result -> Result end.

### Accessing Response Data

Regardless of how you called, you extract the actual values from the response data structure with getter functions. The response data structure contains the complete response; the getter functions point into it, there is no iteration or stream.

        Table = erlvolt:get_table(Result, 1),
        Row = erlvolt:get_row(Table, 1),
        Hello = erlvolt:get_string(Row, Table, "HELLO"), 

### Adding a Connection to the Connection Pool

Erlvolt uses a sophisticated connection pooling mechanism. You can have multiple connections in each pool, which will usually be one connection to each node in the VoltDB cluster. And you can have multiple pools which allows you to access multiple clusters. The pools have a queue each to cushion access peaks, protecting the server from overload.

        erlvolt:add_pool(hello_pool, [{"localhost", 21212}]),

### Running Hello World

Let's run the hello world sample from above. We'll need a VoltDB server for that. This walkthrough assumes a Unix or Linux OS. 

#### 1. Download VoltDB

You can clone the newest community edition from:

    $ git clone git://github.com/VoltDB/voltdb.git voltdb  

Or download the newest VoltDB from `http://voltdb.com/community/downloads.php` and unpack, e.g.:

    $ tar -zxvf voltdb-3.0.tar.gz
    $ mv voltdb-3.0 voltdb

#### 2. Build and run a VoltDB sample database server

The Hello, World! tutorial example comes with every VoltDB distribution. It builds and runs out of the box, on localhost. (Note that it is NOT in the `examples/` directory, but in `doc/tutorials/`):

    $ cd voltdb/doc/tutorials/helloworld
    $ ./run.sh

#### 3. Download and Build Erlvolt

Get the official Erlvolt release from the VoltDB repository:

    $ git clone git://github.com/VoltDB/voltdb-client-erlang.git erlvolt

Or get the newest version from from `https://github.com/Eonblast/Erlvolt`, e.g.:

    $ git clone https://github.com/Eonblast/Erlvolt.git erlvolt
    $ cd erlvolt
    $ make

#### 4. Run the Hello Sample

    $ make hello

You will see a simple 

    Hej, världen!
    
which is `Hello world!` in Swedish, where Erlang was invented by Joe Armstrong, Robert Virding and Mike Williams.

There are more sample programs:


More Samples
------------

Erlang sample programs are in the driver root under `./examples`. 

* **[hello](http://github.com/Eonblast/Erlvolt/blob/master/examples/hello.erl)** - a barebones Hello World   
* **[hello_plus](http://github.com/Eonblast/Erlvolt/blob/master/examples/hello_plus.erl)** - a more robust Hello World    
* **[parallel](http://github.com/Eonblast/Erlvolt/blob/master/examples/parallel.erl)** - an asynchronous Hello World     
* **[voter](http://github.com/Eonblast/Erlvolt/blob/master/examples/voter.erl)** - a VoltDB staple TV show voting example     

To run the samples, do:

    $ make hello-barebones
    $ make hello-plus # same as 'make hello'
    $ make parallel
    $ make voter
    
or (after building Erlvolt.app and the database, as explained above), start hello etc. manually along these lines:

    $ make
    $ cd examples
    $ erlc -I ../include hello.erl
    $ erl -pa ../ebin -s hello run -s init stop -noshell

**Be sure to run the right Volt-database for the respective samples.**  

The Volt sample databases are in `voltdb/doc/tutorials/helloworld` for all **hello, world!** examples and in `voltdb/examples/voter` for the **voter** example. Simply change into the respective directory of your VoltDB installation and build and start the database with `./run.sh`.

## Usage                                                    <a name="Usage"></a>

#### Starting an Application

The Erlvolt driver is an Erlang application. It also uses crypto. This is how you start the driver. If crypto is already working, which is likely in a more complex Erlang program, the first lines is obsolete.

    crypto:start(),
    application:start(Erlvolt).

#### Adding a Pool                                  <a name="Adding_a_Pool"></a>

A pool consists of a number of connections, one to each server in the VoltDB cluster. In the future you will be able to have multiple pools to multiple clusters.

        erlvolt:add_pool(hello_pool, [{"localhost", 21212}]),
    
#### Executing SQL Statements

SQL statements are most of the times stored procedure calls in VoltDB. But you can also freely form 'ad hoc' queries. Calls can be synchronous or asynchronous.

        Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"]).

        Result = erlvolt:call_procedure(PoolID, "@AdHoc", ["select COUNT(*) as cnt from contestants"]).
        
        Result = erlvolt:call_procedure(hello_pool, "Select", ["Swedish"], [asynchronous]),        
        receive 
            Result -> Result
         end.
 
#### Accessing the Result

The Result of a query comes back in one piece. You extract tables, rows and fields out of it like this:

        Table = erlvolt:get_table(Result, 1),
        Row = erlvolt:get_row(Table, 1),
        Hello = erlvolt:get_string(Row, Table, "HELLO"), 

#### The Stored Procedures

The results arrive from the Java stored procedure that you would have complete freedom to program as simple or complex as you want. Knowledge of what you are returning from the SP is indispensable to handle the response. The meta structure is always a list of 'tables'. For example, the above Result comes from this Java procedure, which you find in  `voltdb/doc/tutorials/helloworld/Select.java`:

      public class Select extends VoltProcedure {
      
        public final SQLStmt sql = new SQLStmt(
            "SELECT HELLO, WORLD FROM HELLOWORLD " +
            " WHERE DIALECT = ?;"
        );
      
        public VoltTable[] run( String language)
            throws VoltAbortException {
                voltQueueSQL( sql, language );
                return voltExecuteSQL();
            }
      }

Learn more about VoltDB in this [hands-on tutorial](http://voltdb.com/downloads/documentation/GettingStarted.pdf).

## Tests                                             <a name="Tests"></a>

**Please add a Common Test suite if you are proposing a pull request.**

### Basic Tests

Common Test suites (Unit Tests) can be found in the `etc/test` folder. They help to test the basic functionality of the driver. They might also help you find trip ups in your Erlang and VoltDB system set up (`environment` and `basics` suites).

If you are new to Erlang: *Common Tests* are the Erlang pendant to Unit Tests. They work in the same spirit but employ the Erlang way to assert, using the bind operator  '`=`' (sic) and have a native focus on parallel execution. They are organized in suites and give you a summary of passed and failed tests in the end. But the test is centered around the idea of crashing or not, more than evaluating something to true. To verify results, you write things like `ok = somefunc()` where `somefunc()` is expected to return the atom `ok`. This will crash if `somefunc()` returns something other than `ok` and result in one fail count for the test suite, but not the abortion of the test suite. 

For the tests you need to have the VoltDB **Voter** sample database running, which you find in the VoltDB installation:

    $ cd voltdb/examples/voter
    $ ./run.sh

To run the tests, in a different terminal window, in the driver root folder, type:

    $ make test

These tests currently check access to the database (environment suite) and basic  functionality (basics suite).

The screen will look like the following but the actual results of Common Tests are stored in html.

      make[1]: Nothing to be done for `all'.
      make[1]: Nothing to be done for `all'.
      (cd test; ct_run -suite environment_SUITE basics_SUITE -pa ../ebin /opt/local/var/macports/software/erlang/R14A_0/opt/local/lib/erlang/lib/crypto-2.0/ebin/)
      Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
      
      Converting "../ebin" to "/Users/hd/Erlvolt2/ebin" and re-inserting with add_patha/1
      
      Eshell V5.9.2  (abort with ^G)
      (ct@metal)1> 
      Common Test v1.6.2 starting (cwd is /Users/hd/Erlvolt2/test)
      
      
      Common Test: Running make in test directories...
      Recompile: basics_SUITE
      
      CWD set to: "/Users/hd/Erlvolt2/test/ct_run.ct@metal.2013-02-02_13.17.55"
      
      TEST INFO: 2 test(s), 9 case(s) in 2 suite(s)
      
      Testing hd.Erlvolt2.environment_SUITE: Starting test, 6 test cases
      Testing hd.Erlvolt2.environment_SUITE: TEST COMPLETE, 6 ok, 0 failed of 6 test cases
      
      Testing hd.Erlvolt2.basics_SUITE: Starting test, 3 test cases
      Testing hd.Erlvolt2.basics_SUITE: TEST COMPLETE, 3 ok, 0 failed of 3 test cases
      
      Updating /Users/hd/Erlvolt2/test/index.html... done
      Updating /Users/hd/Erlvolt2/test/all_runs.html... done
      
The last lines give you the starting point for the detailed test results in html.

<div style="font-size:.6em">
<link rel="stylesheet" href="etc/ct_default.css" type="text/css">
<center>
<h1>Test Results</h1>
</center>
<br />
<center>
<div id="button_holder" class="btn">
<a href="#">ALL RUNS</a>
</div><br /><br />
<table id="SortableTable">
<thead>
<tr>
<th>Test Name</th>
<th>Label</th>
<th>Test Run Started</th>
<th>Ok</th>
<th>Failed</th>
<th>Skipped<br>(User/Auto)</th>
<th>Missing<br>Suites</th>
<th>Node</th>
<th>CT Log</th>
<th>Old Runs</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><a href="#">hd.Erlvolt2.basics_SUITE</a></td>
<td align=center><b>-</b></td>
<td>Sat Feb 02 2013 13:17:55</td>
<td align=right>3</td>
<td align=right>0</td>
<td align=right>0 (0/0)</td>
<td align=right>0</td>
<td align=right>ct@metal</td>
<td><a href="#">CT Log</a></td>
<td><a href="#">Old Runs</a></td>
</tr>
<tr class="even">
<td><a href="#">hd.Erlvolt2.environment_SUITE</a></td>
<td align=center><b>-</b></td>
<td>Sat Feb 02 2013 13:17:55</td>
<td align=right>6</td>
<td align=right>0</td>
<td align=right>0 (0/0)</td>
<td align=right>0</td>
<td align=right>ct@metal</td>
<td><a href="#">CT Log</a></td>
<td><a href="#">Old Runs</a></td>
</tr>
</tbody>
<tfoot>
<tr class="odd">
<td><b>Total</b></td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td align=right><b>9<b></td>
<td align=right><b>0<b></td>
<td align=right>0 (0/0)</td>
<td align=right><b>0<b></td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
</tr>
</tfoot>
</table>
</center>
<center>
<br /><br />
<div class="copyright">Copyright &copy; 2013 <a href="#">Open Telecom Platform</a><br />
Updated: <!date>Sat Feb 02 2013 13:17:57<!/date><br />
</div>
</center>
</div>

It is easy to add tests following the template of `etc/test/basic_SUITE.erl`. Note that `io:format()` in test suites does NOT print to the screen. It prints 'to' the lowest level html details result page. Use `ct:log()` instead, in a Common Test suite module, when you want to log to screen, while testing the tests.

It won't hurt to read about the [basics of Erlang Common Tests](http://www.erlang.org/doc/apps/common_test/basics_chapter.html).


## Options                                               <a name="Options"></a>

The sequence of events of the execution of a call depends on the call options. The options go into a propslist in the third parameter of the call_procedure function, e.g.:

    erlvolt:add_pool(PoolID, Hosts, [force, direct]),

The options are addressing four major issues:

### Call Management
    force | queue | drop  
Determine whether the call management's peak buffer, a call `queue` is used or not; the call management can also be used without the queue and set to `drop` calls that can't be executed immediately; or the call management can be circumvented using `force`, in which case a strict round robin is applied for choosing the cluster node to send a call to.

### Monitoring
    monitored | direct  
A call can be executed through a `monitored`, specially spawned workhorse process, which shields the user process from problems in the driver; or it can be executed in `direct` fashion were the internal driver functions are executed by the user process itself.
    
### Synchronous Execution    
    synchronous | asynchronous
With `synchronous` execution, `call_procedure()` blocks until it receives the answer from the server and returns the actual result; with `asynchronous` execution, the call returns immediately and the result is sent to the calling processes' message box.

### Acknowledgement Level
    awaitsendack | fireandforget | blowout  
For fine tuning, the acknowledgement level can be used to make call execution 'one-way'. These options only apply to  `asynchronous` execution. The setting of `awaitsendack` means that call_procedure() returns after it got the ok from the socket that the call was successfully sent. This is meaningful, and can cause a long wait, because of **backpressure**: a VoltDB cluster can stop reading from the socket temporarily in a sign that it is at capacity. A less thorough setting is `fireandforget`, which will return immediately from the send and will have any problems from the socket sent to the user processes' mailbox, just as any error coming from the server, But all non-error results coming from the server are silently dropped. Finally, `blowout` suppresses any feedback, be it from the sending itself or any responses or errors from the server.

Safe Default
------------

The default setting is `[queue, monitored, synchronous]`, it is the safest, most comfortable, most sensible and slowest. Contrary to intuition you are NOT loosing parallelism or block your application using `synchronous` execution because a real Erlang program inevitably consists of thousands of parallel microprocesses. Within the individual microprocess you will probably *want* to wait blocking for results from the server and that is what the `synchronous` setting does for you. Spawning a dedicated `monitored` worker process protects you against possible immaturities in the driver. If something bad happens, the worker will crash and not your own, calling process, so you need not defend against that yourself. There is still some crashing involved, so this doesn't make it less Erlangish, just easier to debug. The `queue` will usually increase performance on the bottom line as the VoltDB cluster is picky about overload, and slows down when it gets too much. With unpredictable load the queue will even out peaks nicely and the server cluster nodes will be served based on their load rather than simple round-robin.

Fast Setting
------------

The 'fastest' setting can be `[force, direct, asynchronous]` and this is used for the benchmarks. It's not actually the 'fastest', but the one with highest throughput, which is what VoltDB is all about. If you don't even care for the answers from the server, you could add `fireandforget`, or even `blowout` for completely blind, one-way offloading of calls, UDP-style. There are settings were this can make sense. The `force` setting circumvents the Connection Managemer and its `queue` and sends calls directly to the server, using Connections in the pool blindly round-robin. This can easily overload the server in a benchmark when you firehose from enough client machines, which can make the results both roughly 10 times slower (in the ~100ms area instead of ~5ms), and the throughput 10 times lower (very roughly, ~1,000 transactions per CPU core instead of ~10,000), than if you had used the `queue`. But if you have a predictably limited, steady flow of data, near the capacity of your system, then `force` can give you 20%-50% higher throughput, with the same latency as `queue`.
    
## Architecture                                               <a name="Architecture"></a>

### Execution

The driver is started once and can serve multiple processes, non-blocking, or blocking, using multiple connections and connection pools.

One connection is established to each node of a VoltDB cluster. Those connections form one 'connection pool'. Erlvolt can serve multiple pools in parallel. However, it has only one Connection Manager to organize load across connections. So the unlikely case of massively many pools would be bottle-necked by this manager and it should be circumvented using the `force` option.

User functions usually have the atomic id of a pool as their first parameter to tell the driver which server cluster is addressed.

Results from the server are delivered as complete structures. There is no notion of streaming large results as this is not the use case for OLTP, but brief results, often simpel write acknowledgements, are the norm.

As is customary for VoltDB, queries are usually executions of stored procedures, baked into the VoltDB cluster and programmed in Java. But normal queries, so-called 'ad-hoc queries' are easily available with this driver, for tests and non-performance sensitive experimentation. E.g.

     R = erlvolt:call_procedure(mypool, "@AdHoc", ["SELECT COUNT(*) AS cnt FROM votes"]),


### Structure

The Erlvolt application* consists of three supervisors, two gen servers, multiple connection management processes and optional per-call workhorse processes.

The supervisors are:  
* the Connection Manager Supervisor  
* the Connection Supervisor and  
* the Profiler Supervisor.  

The gen servers are:  
* the Connection Manager and  
* the Profiler Supervisor.  

Each Connection Management Process is a temporary, supervised child that is not derived from an *OTP behavior* but does follow the requirements for an *OTP supervisor's* child.

The optional workhorse per-call processeses are monitored, within the `erlvolt` module, by the calling user process. Alternatively, the user process can be executing the driver functions itself and thus skip one layer of indirection and protection.

*The term ['application'](http://www.erlang.org/doc/design_principles/applications.html) has special meaning in Erlang. Roughly, a package seen from the point of view of administration.


### Sequence

The internal sequence of a call is as listed below, depending on the options described above. Because of the structure of an Erlang program, `asynchronous` execution of queries may double parallelism. Start thinking about the `synchronous` use case and tune your requirements from that base if needed. The atoms in brackets in the following are the respective call options.

1. user process optionally (`force`): retrieves the pool's list of connections
2. user process calls erlvolt:call\_procedure() to initiate the transaction
3. user process optionally (`queue`, `drop`): applies for a virtual connection slot
4. user process is (`queue`): queued, or (`drop`): rejected when no slot is available
5. user process optionally (`monitored`): creates worker process, or (`direct`): not 
6. user or worker process sends transaction to socket process, which sends it on 
7. call\_procedure() optionally (`asynchronous`: `fireandforget`, `blowout`): returns ok
8. socket process optionally (`synchronous`, `asynchronous`: `awaitsendack`): acks send
9. call\_procedure() optionally (`asynchronous`: `awaitsendack`): returns ok
10. socket process receives answer from the server
11. socket process optionally (`synchronous`, `asynchronous`: `awaitsendack`): forwards it
12. socket process optionally (`asynchronous`: `fireandforget`, `blowout`): drops it
13. socket process forwards any errors except for (`asynchronous`: `blowout`)
14. user or worker process receive any server answers to their process mailbox
15. call\_procedure() optionally (`synchronous`): returns the server response


## History                                               <a name="History"></a>

**For version history, see CHANGES.md**

The Erlvolt driver is the result of the hunt for a better database for heavy duty online-game servers at [Eonblast][]. I had experienced first hand what a pain it could be to try to scale MySQL and found [VoltDB][] uniquely suited for the requirements of more complex game worlds. Better than any other database in fact.

I had also looked for a better language than Java for programming servers, most of all, one that would be less dead-lock prone, and for that I chose Erlang. To be able to use them together, I started creating the Erlang driver for VoltDB.

Work started in 2009 and I donated a first working version of the driver to VoltDB at their request. It was perfectly usable but out of the box only provided for synchronous connections. In 2012 VoltDB decided to sponsor the creation of a bigger and badder version. Now the real deal has arrived.

It is pure Erlang, blazingly fast and fit for VoltDB 3. It incorporates almost all of the previous, robust driver version. To ensure reliable, consistently high throughput, I drew from my experience maintaining the Erlang MySQL driver, Emysql. The connection pooling and call queueing is modeled after the ones used in that reliable workhorse, which was originally created by [Jacob Vorreuter][jv] and [Nick Gerakines][ng] at Electronic Arts. They enable the Erlang driver to absorb access peaks, and to distribute load across VoltDB server nodes. This could be particularly valuable since a VoltDB cluster can slow down quite a bit if you actually make it to use it at capacity.

[ng]: https://github.com/ngerakines     "Nick Gerakines"
[jv]: https://github.com/JacobVorreuter "Jacob Vorreuter"  
[bw]: bill@rupture.com                  "Bill Warnecke"  
[hd]: hd2010@eonblast.com               "Henning Diedrich"  


## License                                                  <a name=License></a>

Copyright (c) 2009-2013
Henning Diedrich <hd2010@eonblast.com>,
Eonblast Corporation <http://www.eonblast.com>.

Permission is  hereby  granted,  free of charge,  to any person
obtaining  a copy of this software and associated documentation
files  (the  "Software"),  to  deal  in  the  Software  without 
restriction,  including  without limitation  the rights to use,
copy, modify,  merge,  publish, distribute,  sublicense, and/or 
sell  copies of the  Software,  and to permit  persons  to whom
the  Software  is furnished to do so,  subject to the following 
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

[Installation]:  #Installation
[Files]:         #Files
[Samples]:       #Samples
[Usage]:         #Usage
[Options]:       #Options
[History]:       #History
[Tests]:         #Tests
[Links]:         #Links
[License]:       #License
[Adding_a_Pool]: #Adding_a_Pool

