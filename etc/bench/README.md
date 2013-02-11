Benchmarks
==========

**voltdb-client-erlang Erlvolt 0.3**  

**Release: 'Erlvolt 0.3.2'**  
**Author: H. Diedrich**  
**Production: Eonblast Corporation**  
**Copyright: (c) 2013 VoltDB, Inc**  
**Licence: MIT**  
**Date: 31 Jan 2013**


Quick Start
-----------

Start the Voter database and run a benchmark client on local host:

    $ cd voltdb/examples/voter && ./run.sh &
    $ cd erlvolt && make clean fast bench 


Quick Read
----------

For a reference benchmark done in the EC2 cloud, see doc/BENCHMARK1.md or html.

Usage
-----

#### Ways to Run

The benchmark module can be parametrized for quite different modi operandi
and it can be started in three different ways:

- **pre-configured make**
- **parametrized multi-VM batch, via make**
- **fully parametrized command line**

Alle three ways, in the end, execute the same module, bench.erl. They expose
different options to change the module's behavior and get benchmark clients
up and running quickly, or easily tune them.

#### Database

The benchmark is mostly based on the 'Voter example' database and server, which
is present in every VoltDB distribution and is a VoltDB benchmark staple. For an
explanation of the 'TV voter' scenario, see comments below.

To run the Voter database, start the Voter server from your VoltDB installation with:   

    $ cd voltdb/examples/voter                                          
    $ ./run.sh                                                          

The benchmark can also be run with the 'Hello world' tutorial database.

To run the Hello World database, start it from your VoltDB installation with:   

    $ cd voltdb/examples/voter                                          
    $ ./run.sh                                                          

You need to start the right database before you can run the benchmark.

#### VoltDB Host addresses

The host addresses, for almost all cases that are described below, is read from 
`etc/bench/bench.config`. You would change that file and write your actual
host information into it, once you are doing a real, live benchmark. The port
would probably always remain 21212, across all nodes.

The default `bench.config` points to localhost, like this:

[ {erlvolt, [
	{hosts, [
		{"localhost", 21212}]},

	{start_delay, 0}

]}].

#### Synch Bench start

The `bench.config` also offers support to get bench clients on multiple 
machines to start at the exact same time:

the `start_delay` parameter is used to synchronize benchmarks across multiple
machines e.g. EC2 instances. Use e.g. 20 to have twenty seconds to start all
benchmarks on all machines. All benchmarks will start after 20 to 40 seconds,
at the same wallclock time.

### Usage (I): Pre-configured Make                                                            

To start a benchmark pre-configured in make, open a new terminal screen,
and from the driver root, do:     

    $ make clean fast bench                                             

You can of course leave out the middle part -- `clean fast` -- for repeat  
runs.

**`fast`** builds  HiPE native compiled beams,  without debug information,  
which makes for  some tens of percents higher throughput  compared to   
the default compilation (`all`) without HiPE & with debug information.  

**`profile`** compiles similar but with the profiler enabled, which   
gives you rolling performance stats every second.                       

**`bench`** actually triggers one specific bench, tagged 'VSD', which   
stands for Voter, Steady load and Direct calls.                         

***`benches`** runs four voter benchmarks,  in a row:  VSD, VSM, VBM, VBD.   

From `make clean fast bench` you will see this kind of response:     

      Erlvolt Bench 0.9 (client 'VSD')
      -------------------------------------------------------------------------------------------------------------------------------------
      Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 'n/a' stats/sec
      Hosts: localhost:21212 localhost:21212 
      connect ...
      preparation ...
      calls ... ........................
      cool down ... 
      check writes ... ok
      results ...  votes:     200,000 (6 contestants)
      .....Alana Bregman:      33,742
      ...Tabatha Gehling:      33,661
      ......Kelly Clauss:      33,264
      ....Jessie Alloway:      33,162
      .....Edwina Burnam:      33,118
      ....Jessie Eichman:      33,053
      close pool ...
      Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 'n/a' stats/sec
      -------------------------------------------------------------------------------------------------------------------------------------
      Client 'VSD' overall: 20,050 T/sec throughput, 0.00% fails, total transactions: 100,000, fails: 0, total time: 4.987sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:21:41, ended 2013-02-03 15:21:46, database: +100,000 new votes
      [++++++++++++++++++++]

From `make clean **profile** bench` you will see this kind of response:  

      Erlvolt Bench 0.9 (client 'VSD')
      -----------------------------------------------------------------------------------------------------------------------------------
      Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 5.0 stats/sec
      Hosts: localhost:21212 localhost:21212 
      connect ...
      preparation ...
      calls ... 
      Client VSD: at  0.217sec: lap   19,339 T/sec,  total   19,368 T/sec, success:   4,203, fails: 0, pending:     111, avglat:  0.286ms, maxlat:    3ms
      Client VSD: at  0.416sec: lap   19,484 T/sec,  total   19,437 T/sec, success:   8,086, fails: 0, pending:     117, avglat:  0.286ms, maxlat:    2ms
      Client VSD: at  0.617sec: lap   19,269 T/sec,  total   19,366 T/sec, success:  11,949, fails: 0, pending:     103, avglat:  0.317ms, maxlat:    2ms
      Client VSD: at  0.817sec: lap   19,798 T/sec,  total   19,481 T/sec, success:  15,916, fails: 0, pending:     109, avglat:  0.302ms, maxlat:    1ms
      Client VSD: at  1.016sec: lap   20,202 T/sec,  total   19,625 T/sec, success:  19,940, fails: 0, pending:     103, avglat:  0.316ms, maxlat:   10ms
      Client VSD: at  1.216sec: lap   19,895 T/sec,  total   19,671 T/sec, success:  23,921, fails: 0, pending:     154, avglat:  0.310ms, maxlat:    1ms
      Client VSD: at  1.417sec: lap   20,071 T/sec,  total   19,727 T/sec, success:  27,954, fails: 0, pending:     129, avglat:  0.301ms, maxlat:    1ms
      Client VSD: at  1.616sec: lap   19,865 T/sec,  total   19,740 T/sec, success:  31,901, fails: 0, pending:      93, avglat:  0.300ms, maxlat:    2ms
      Client VSD: at  1.817sec: lap   18,909 T/sec,  total   19,653 T/sec, success:  35,711, fails: 0, pending:     121, avglat:  0.333ms, maxlat:    5ms
      Client VSD: at  2.018sec: lap   17,793 T/sec,  total   19,463 T/sec, success:  39,277, fails: 0, pending:     114, avglat:  0.412ms, maxlat:   10ms
      Client VSD: at  2.217sec: lap   19,132 T/sec,  total   19,435 T/sec, success:  43,089, fails: 0, pending:     137, avglat:  0.427ms, maxlat:    7ms
      Client VSD: at  2.416sec: lap   20,238 T/sec,  total   19,505 T/sec, success:  47,126, fails: 0, pending:     105, avglat:  0.417ms, maxlat:    2ms
      Client VSD: at  2.616sec: lap   18,462 T/sec,  total   19,419 T/sec, success:  50,802, fails: 0, pending:     144, avglat:  0.462ms, maxlat:   13ms
      Client VSD: at  2.819sec: lap   20,121 T/sec,  total   19,470 T/sec, success:  54,887, fails: 0, pending:      91, avglat:  0.449ms, maxlat:    3ms
      Client VSD: at  3.016sec: lap   19,567 T/sec,  total   19,479 T/sec, success:  58,749, fails: 0, pending:     156, avglat:  0.440ms, maxlat:    2ms
      Client VSD: at  3.217sec: lap   20,251 T/sec,  total   19,525 T/sec, success:  62,813, fails: 0, pending:      99, avglat:  0.429ms, maxlat:    2ms
      Client VSD: at  3.418sec: lap   19,615 T/sec,  total   19,530 T/sec, success:  66,756, fails: 0, pending:     103, avglat:  0.418ms, maxlat:    2ms
      Client VSD: at  3.617sec: lap   19,913 T/sec,  total   19,552 T/sec, success:  70,723, fails: 0, pending:      99, avglat:  0.419ms, maxlat:    7ms
      Client VSD: at  3.818sec: lap   19,580 T/sec,  total   19,554 T/sec, success:  74,658, fails: 0, pending:     141, avglat:  0.410ms, maxlat:    1ms
      Client VSD: at  4.018sec: lap   20,394 T/sec,  total   19,597 T/sec, success:  78,744, fails: 0, pending:      97, avglat:  0.402ms, maxlat:    1ms
      Client VSD: at  4.217sec: lap   20,176 T/sec,  total   19,623 T/sec, success:  82,754, fails: 0, pending:     132, avglat:  0.397ms, maxlat:    2ms
      Client VSD: at  4.416sec: lap   20,239 T/sec,  total   19,653 T/sec, success:  86,791, fails: 0, pending:      86, avglat:  0.392ms, maxlat:    1ms
      Client VSD: at  4.614sec: lap   19,576 T/sec,  total   19,648 T/sec, success:  90,660, fails: 0, pending:     200, avglat:  0.387ms, maxlat:    2ms
      Client VSD: at  4.817sec: lap   19,960 T/sec,  total   19,660 T/sec, success:  94,703, fails: 0, pending:     120, avglat:  0.385ms, maxlat:    9ms
      Client VSD: at  5.017sec: lap   20,069 T/sec,  total   19,677 T/sec, success:  98,720, fails: 0, pending:      89, avglat:  0.384ms, maxlat:    5ms
      Client VSD: at  5.246sec: lap    5,249 T/sec,  total   19,047 T/sec, success:  99,924, fails: 0, pending:      29, avglat:  0.383ms, maxlat:    2ms
      cool down ... 
      check writes ... ok
      results ...  votes:     300,000 (6 contestants)
      ...Tabatha Gehling:      50,320
      .....Alana Bregman:      50,247
      ....Jessie Alloway:      49,990
      ......Kelly Clauss:      49,925
      .....Edwina Burnam:      49,897
      ....Jessie Eichman:      49,621
      close pool ...
      Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 5.0 stats/sec
      -----------------------------------------------------------------------------------------------------------------------------------
      Client 'VSD' overall: 18,622 T/sec throughput, 0.00% fails, total transactions: 100,000, fails: 0, total time: 5.370sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:23:42, ended 2013-02-03 15:23:47, database: +100,000 new votes
      [++++++++++++++++++]

-------------------------------------------------------------------------

### Usage (II) - Multi-VM Shell Batch                                            

'make bench-mvm' starts multiple Erlang VMs (emulators), e.g.:          

    $ make bench-mvm VMS=2 CORES=4                                      

Parameters are:

VMS   (1)      number of virtual machines (Erlang emulators) started    
CORES (1)      number of cores used per VM (erl parameter +S)           
CALLS (10000)  number of transactions per VM for the benchmark          
SPAWN (100)    number of parallel ('steady') workers = max server load   

This can be the most effective way to run real benchmarks.  This make   
command uses the etc/bench/benchstart shell script. It can make sense   
to first experiment with  direct parameters  as described below, then   
hardcode the results into  etc/bench/benchstart to prepare for a real   
bench run, e.g. in a cloud setup.                                       

E.g. for `make clean fast bench-mvm VMS=3 CALLS=1000000` you could get this kind of dump:

      Mulitple VM Erlvolt Bench Log
      This is the followed tail of the file bench.log, which is written to by 3 separate Erlang VMs running the benchmark.
      Client 'VSD2', voter, 1,000,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 'n/a' stats/sec
      Client 'VSD3', voter, 1,000,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 'n/a' stats/sec
      Client 'VSD1', voter, 1,000,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 'n/a' stats/sec
      .................................................................................................... [..]
      Client 'VSD2' overall: 10,617 T/sec throughput, 0.00% fails, total transactions: 1,000,000, fails: 0, total time: 94.183sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:29:51, ended 2013-02-03 15:31:25, database: +2,999,831 new votes
      [++++++++++]
      Client 'VSD3' overall: 10,583 T/sec throughput, 0.00% fails, total transactions: 1,000,000, fails: 0, total time: 94.483sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:29:51, ended 2013-02-03 15:31:26, database: +2,999,995 new votes
      [++++++++++]
      Client 'VSD1' overall: 10,577 T/sec throughput, 0.00% fails, total transactions: 1,000,000, fails: 0, total time: 94.539sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:29:51, ended 2013-02-03 15:31:26, database: +2,999,895 new votes
      [++++++++++]
      Total transactions per second: 31777 T/sec

      
With `make clean profile bench-mvm VMS=3 CALLS=10000` you get rolling stats like this:

      Mulitple VM Erlvolt Bench Log
      This is the followed tail of the file bench.log, which is written to by 3 separate Erlang VMs running the benchmark.
      Client 'VSD3', voter, 20,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 2.0 stats/sec
      Client 'VSD2', voter, 20,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 2.0 stats/sec
      Client 'VSD1', voter, 20,000 calls, steady, 100 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, nonverbose, 2.0 stats/sec
      Client  VSD3: at  0.511sec: lap   10,023 T/sec,  total   10,027 T/sec, success:   5,124, fails: 0, pending:      96, avglat:  2.634ms, maxlat:   25ms
      Client  VSD2: at  0.507sec: lap    9,984 T/sec,  total    9,998 T/sec, success:   5,069, fails: 0, pending:      66, avglat:  2.806ms, maxlat:   17ms
      Client  VSD1: at  0.515sec: lap    9,545 T/sec,  total    9,545 T/sec, success:   4,916, fails: 0, pending:     100, avglat:  2.678ms, maxlat:   18ms
      Client  VSD3: at  1.007sec: lap   10,283 T/sec,  total   10,153 T/sec, success:  10,225, fails: 0, pending:      68, avglat:  2.678ms, maxlat:   14ms
      Client  VSD2: at  1.007sec: lap    9,706 T/sec,  total    9,852 T/sec, success:   9,921, fails: 0, pending:      52, avglat:  2.778ms, maxlat:   11ms
      Client  VSD1: at  1.008sec: lap   10,709 T/sec,  total   10,116 T/sec, success:  10,197, fails: 0, pending:      79, avglat:  2.671ms, maxlat:   14ms
      Client  VSD3: at  1.508sec: lap    9,975 T/sec,  total   10,096 T/sec, success:  15,226, fails: 0, pending:      95, avglat:  2.700ms, maxlat:   14ms
      Client  VSD2: at  1.514sec: lap   10,487 T/sec,  total   10,062 T/sec, success:  15,234, fails: 0, pending:      33, avglat:  2.762ms, maxlat:   12ms
      Client  VSD1: at  1.509sec: lap   10,046 T/sec,  total   10,098 T/sec, success:  15,239, fails: 0, pending:      35, avglat:  2.708ms, maxlat:   14ms
      Client  VSD3: at  2.006sec: lap    9,447 T/sec,  total    9,934 T/sec, success:  19,929, fails: 0, pending:      24, avglat:  2.655ms, maxlat:   10ms
      Client 'VSD3' overall: 9,880 T/sec throughput, 0.00% fails, total transactions: 20,000, fails: 0, total time: 2.024sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:45:49, ended 2013-02-03 15:45:51, database: +59,837 new votes
      [+++++++++]
      Client 'VSD2' overall: 10,065 T/sec throughput, 0.00% fails, total transactions: 20,000, fails: 0, total time: 1.987sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:45:49, ended 2013-02-03 15:45:51, database: +59,427 new votes
      [++++++++++]
      Client 'VSD1' overall: 10,173 T/sec throughput, 0.00% fails, total transactions: 20,000, fails: 0, total time: 1.966sec 
      Erlvolt 0.3.0, bench started 2013-02-03 15:45:49, ended 2013-02-03 15:45:51, database: +58,425 new votes
      [++++++++++]
      Total transactions per second: 30118 T/sec


-------------------------------------------------------------------------

### Usage (III) - Direct Emulator Call                                               

As you can see in the etc/bench/Makefile,  there are multiple options   
that you can use to run a fine tuned benchmark calling bench directly.
   
Note that this bench module  cannot  perform automatic fine tuning as   
the original  Java  client can,  which you can run in the same folder   
as the server, using './run.sh client'.                                 

However, it doesn't take long to zero in to a best load and the 'VSD'   
configuration would be your starting point (voter, steady, direct).     
It produces a steady load from parallel worker threads  that are each   
blocking individually, until they get a response. This is the fastest   
way on the *client*-side  to  protect the server from  overload  from   
'fire-hosing' clients.                                                  

The parameters are:                                                     

    $ erl -pa ../../ebin -s bench run <vmid> <bench> <calls> <rhythm>\  
        <burstsize> <delay> <mode> <queuesize> <slots> <limit> \        
        <verbosity> <dumpinterval> -s init stop -noshell +p 1000000  \  
        -smp +s $cores -config bench                                    

E.g. the standard bench is kicked in with:                              

    $ erl -pa ../../ebin -s bench run VSD voter 100000 steady 100 x  \  
        direct x x x nonverbose 500 -s init stop -noshell +P 1000000 \  
        -smp +S $CORES -config bench                                    

- **vmid**          Virtual Machine ID, trails screen logs                  
- **bench**         type of benchmark calls, 'voter' or 'hello'             
- **calltarget**    number of calls before benchmark ends                   
- **rhythm**        timed 'bursts' or 'steady' synched calls                
- **burstsize**     calls per burst or # of steady workers                  
- **delay**         millisec delay between bursts                           
- **mode**         'direct' calls or using 'managed' driver queue           
- **queuesize**     max processes that can wait in queue                    
- **slots**         max pending request before queueing next ones           
- **limit**        'managed': queue size, 'direct': # pending               
- **verbosity**     quiet, nonverbose, verbose, veryverbose                 
- **dumpinterval**  for 'make profile' msec between stats                   

For more examples see etc/bench/Makefile.                                    

-------------------------------------------------------------------------

Voter Description
-----------------                                                     

The Voter sample application simulates a phone-based election process.  
Voters  (based  on  phone numbers generated  randomly  by the  client   
application) are allowed a limited number of votes.                     

The main goal of the voter application is to demonstrate the            
performance possibilities of VoltDB:                                    

Stored procedures are invoked  asynchronously.  The client can record   
and report thoughput and latency, when compiled with the driver-built   
in profiler (`make clean profile`).                                     

Usually, you will run this sample on a single node.  However,  it can   
easily be reconfigured to run any combination of clients and servers.   
The servers support multiple clients,  and single-node  or multi-node   
server clusters are supported.                                          

To test the server with a different configuration, simply edit run.sh   
listing one  server node as the  lead node  when building the catalog   
and using a comma-separated list of the server addresses  as argument   
to the client. See the comments in the build script for details.        

The client  starts  'fire-hosing'  the  VoltDB server  using  pre-set   
parameters.  You  can  experiment  with  different  loads  to  better   
understand the  best tuning  to  get  the  most  out of your specific   
VoltDB deployment.                                                      

Rate-limiting your clients (or adding cluster nodes)  is essential to   
preventing  'fire-hosing'  your server (cluster)  and will ensure you   
get  proper  application  responsiveness  (latency)  while maximizing   
througput (TPS) for your hardware configuration.                        

The "Voter" application is specifically  designed for benchmarking to   
give you a good feel  for the type  of performance  VoltDB is capable   
of on your hardware.                                                    

For more on  benchmarking and  tips on application tuning,  visit the   
VoltDB blog: [http://voltdb.com/search/node/benchmark][] and                
[http://voltdb.com/search/node/tuning][]                                    

-------------------------------------------------------------------------

For a barebones 'hello world' see examples/hello.erl.             

-------------------------------------------------------------------------

See README.md or .html for instructions,  examples/ for more examples.  
See doc/BENCHMARKS.md or .html for a description of benchmarks done.  

For getting started with VoltDB,see: voltdb/doc/GettingStarted.pdf or   
online:  http://voltdb.com/downloads/documentation/GettingStarted.pdf.  

-------------------------------------------------------------------------

/hd 31 jan 13