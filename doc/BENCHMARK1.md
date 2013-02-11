VoltDB Blog: 877,000 TPS with Erlang and VoltDB 
===============================================

Henning Diedrich - 6 Feb 2013

**Running on a suitable EC2 configuration (see details below), with our new VoltDB Erlang driver we achieved 877,519 transactions per second.**

I am Henning Diedrich [1], CEO of Eonblast Corporation [2] a games company. I would like to introduce the new Erlang VoltDB driver we created, a piece of software that allows two genre-defining technologies to work together: VoltDB [3] and Erlang [4].

The Driver
--------------

I first came to VoltDB on the hunt for a better database for heavy duty online-game servers. I experienced first hand [5] what a pain it was to try to scale MySQL and found **VoltDB** [3] uniquely suitable for the requirements of more complex game worlds. Better than any other database in fact [6].

I had also looked for a better language than Java for programming servers and for that,  **Erlang** [7] had caught my attention. To be able to use them together [8], I started creating the Erlang driver for VoltDB [9].

Work for this started three years ago and I donated a first version of the driver [10] to VoltDB at their request in 2010. It was perfectly usable but out of the box only provided for synchronous connections. In 2012 VoltDB decided to sponsor the creation of a bigger and badder version. Now the real deal has arrived [11].

The benchmark described below was made with the new, asynchronous driver. It is pure Erlang, full of parallel microprocesses, blazingly fast and fit for VoltDB 3 [12]. It builds on and incorporates almost all of the previous, robust driver version. And on my quest to ensure reliable, consistently high throughput, I was able to draw from my experience maintaining an Erlang **MySQL** driver, Emysql [13]. The connection pooling and call queueing is modeled after the ones used in that reliable workhorse, originally designed at Electronic Arts. They enable the Erlang driver to absorb access peaks, and to distribute load across VoltDB server nodes.

To come up with a useful benchmark script I could employ the lessons learned from the Node.js benchmark [14] I did for VoltDB a while ago. This time around I knew which numbers I would be looking for and what double checks I should have in place to put the cloud cluster to good use.

The internal structure of the driver has been implemented as would be expected: your program's microprocesses use the functions the driver exposes to send a message to a dedicated connection process, which handles the socket work. After the request is sent, the initiating process is either blocked in a *synchronous* receive (this, of course, does *not* block all your *other* processes) or goes on to to use its time as it pleases, should you choose the *asynchronous* mode. The answer from the server arrives in your processes' mailbox.  (Note, *synchronous* in this context, when looking at the driver. The call to the VoltDB server is still an asynchronous call. The driver simply has your process wait in a receive block.)

There are many options that you can use. E.g. the *monitored* mode, where a worker process is created that handles the sending of the request, thereby shielding your initiating process from any hiccups in the driver. You can *fire and forget*, for writes where you don't care to hear that they succeeded. Or *blowout* if you don't even care to hear about failure.

The Benchmark Application
-------------------------------------

The benchmark is based on the VoltDB voter example, which comes with every VoltDB distribution. It 'validates and stores phoned-in votes for talent show contestants'. In the original example setup, there is a web page that displays the results for illustration, updated every 400ms. You'll find it in the `examples/voter` directory of your *VoltDB* installation.

The benchmark starts out with a preparational phase, where the database is filled with 6 contestants' names and then one million write transactions are fired towards the server, per CPU core, that each register a 'vote' for one of the contestants, picked at random. In the end, the votes won by each contestants are displayed, using a materialized view and a VoltDB ad-hoc query. (In VoltDB parlance, ad-hoc queries are normal queries that are not pre-formulated in a stored procedure.)

The benchmark source is under etc/bench [15] of the *driver* home directory, where you'll also find a detailed README.md that explains the multiple ways to run the benchmark and make it fit your setup. For a (slow) test run on localhost, it's basically:

    $ git clone git://github.com/VoltDB/voltdb.git voltdb
    $ git clone git://github.com/VoltDB/voltdb-client-erlang.git erlvolt
    $ cd voltdb/examples/voter && ./run.sh &
    $ cd && cd erlvolt && make clean all bench

That should give you a screen like this:

        metal:~ hd$ cd voltdb-3-com/examples/voter && ./run.sh &
        [1] 10817
        metal:~ hd$ Initializing VoltDB...
        
         _    __      ____  ____  ____ 
        | |  / /___  / / /_/ __ \/ __ )
        | | / / __ \/ / __/ / / / __  |
        | |/ / /_/ / / /_/ /_/ / /_/ / 
        |___/\____/_/\__/_____/_____/
        
        --------------------------------
        
        Build: 3.0 voltdb-3.0-95-gfffab2b Community Edition
        Connecting to VoltDB cluster as the leader...
        Initializing initiator ID: 0, SiteID: 0:5
        WARN: Running without redundancy (k=0) is not recommended for production use.
        Server completed initialization.
        
        
        metal:erlvolt hd$ cd && cd erlvolt && make clean all bench
        clean
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_app.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_conn.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_conn_mgr.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_profiler.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_sup.erl
        erlc -W -I ../include  +debug_info -o ../ebin erlvolt_wire.erl
        erlc -W -I ../../include  +debug_info -o ../../ebin bench.erl
        
        
        Erlvolt Bench 0.9 (client 'VSD')
        -------------------------------------------------------------------------------------------------------------------------------------
        Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, "n/a" stats/sec
        Hosts: localhost:21212 
        connect ...
        preparation ...
        Start at: 2013-02-06 18:56:20 .....................
        Starting: 2013-02-06 18:56:20
        calls ... ..................................
        cool down ... 
        check writes ... ok
        results ...  votes:     100,000 (6 contestants)
        .....Edwina Burnam:      16,817
        ....Jessie Alloway:      16,808
        ...Tabatha Gehling:      16,669
        .....Alana Bregman:      16,613
        ....Jessie Eichman:      16,556
        ......Kelly Clauss:      16,537
        close pool ...
        Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, "n/a" stats/sec
        -------------------------------------------------------------------------------------------------------------------------------------
        
        Client 'VSD' overall: 14,357 T/sec throughput, 0.00% fails, total transactions: 100,000, fails: 0, total time: 6.965sec 
        Erlvolt 0.3.3, bench started 2013-02-06 18:56:20, ended 2013-02-06 18:56:26, database: +100,000 new votes
        [++++++++++++++]



For further instructions, e.g. how to best run the benchmark in the cloud, please see etc/bench/README.md [16], or verbatim doc/BENCHMARK-README.html, in your driver home folder.


The Benchmark Results
--------------------------------

When run on a single core (-smb +S 1), with a 12-node VoltDB server cluster listening on the other side, the Erlang driver showed throughput of **26,500 transactions per second (TPS) and more per one core**. When fully utilizing a 16-core cluster instance as client node, it routinely reached throughput of **260,000 transactions per second per machine**. (CPU specs see below.)

Using 8 client nodes connected to a 12-node VoltDB cluster, each client node executed an average of 109,689 transactions per second for a **cluster total of 877,519 TPS**.

Since this benchmark was about the driver, not the server, I made no attempts to tune the server cluster. After a lot of experimenting, I believe that the lower performance per client core for bigger server clusters would reflect the network limitations of the EC2 cloud, even for the bigger cluster instances [17] where the hope would be that a benchmark would not end up network-bound.

Part of the goal for the benchmark was testing how the driver would hold up under load and that turned out very well. The driver does not crash from really heavy overload and it copes well with 'backpressure' [18] when the server does not allow further requests for being at capacity. However, the fastest benchmarks resulted when not overloading the server.


The Environment
-----------------------

I started a 20-node Amazon EC2 cc2.xlarge cluster broken up into 8 Erlang client and 12 VoltDB server nodes. The m3.2xlarge provide the following, as described by the Amazon EC2 Instance Types page [17]:

### Cluster Compute Eight Extra Large Instance (cc2.8xlarge)

 * 60.5 GiB of memory
 * 88 EC2 Compute Units (2 x Intel Xeon E5-2670, eight-core)
 * 3370 GB of instance storage
 * 64-bit platform
 * I/O Performance: Very High (10 Gigabit Ethernet)

These nodes were configured with:

* Ubuntu Server 12.04 LTS for Cluster Instances AMI
* Oracle Java JDK 1.7
* Erlang R15B03
* VoltDB Enterprise Edition 3.0 RC

On advice from VoltDB, each of the five server nodes was set to six partitions, so I had 30 partitions across the database cluster.

This benchmark would perform the same on the free Community Edition of Volt 3.0.


The Transactions
-----------------------

The clients "firehose" the VoltDB cluster by calling Voter's `vote()` stored procedure continuously. This procedure performes not only one write but, depending on how you count, 4 to 6 operations:

* It retrieves the caller's location (a `select`)
* Verifies that the caller has not exceeded his/her vote maximum (a `select`)
* Verifies that the caller was voting for a valid contestant (a `select`)
* And if yes to all of the above, a vote is cast on behalf of that caller (an `insert`)

On top of this, each `insert` also triggers an update to two different materialized views.

Here are the actual queries that define the used stored procedures [19]:

    // Checks if the vote is for a valid contestant
    SELECT contestant_number FROM contestants WHERE contestant_number = ?;

    // Checks if the voter has exceeded their allowed number of votes
    SELECT num_votes FROM v_votes_by_phone_number WHERE phone_number = ?;

    // Checks an area code to retrieve the corresponding state
    SELECT state FROM area_code_state WHERE area_code = ?;

    // Records a vote
    INSERT INTO votes (phone_number, state, contestant_number) VALUES (?, ?, ?);

Consequently, the 877,000 TPS performed **3.5 million SQL operations per second**, i.e. three selects and one insert.


Observations & Notes
-----------------------------

The most important number from this, to my mind, is the **26,500 transactions per second per CPU core** that I saw, which translates into 100,000 operations. This will allow you to make rough estimates on the amount of hardware you may need on the business server side (the VoltDB client side). Your client will usually have more work to do than simply swamp the server, as the benchmark does. So you have an upper limit here and can feel your way down from there. Note that many machines will show a significantly higher performance profile than EC2 instances.

We decided for the Amazon Elastic Cloud for the benchmark in the hopes that this would result into the most transparent setup. A local cluster of eight "bare metal" nodes would certainly perform better than the EC2 instance, and be way more economic if you used them on a daily basis. But our throughput numbers would be hard to reproduce independently.

As it is, you could try the exact same benchmark yourself, easily. VoltDB and the new driver can be downloaded from VoltDB [20]. The README.md [21] of the driver, and of the benchmark [16] have more instructions on how to use the driver and how to make benchmarks. To find experimental new versions of the driver as well as fast bug fixes, try the Eonblast Erlvolt repo at git [22]. The free VoltDB community edition is also on github [23].

[1] Author on Twitter: https://twitter.com/hdiedrich  
[2] Eonblast: http://www.eonblast.com  
[3] VoltDB: http://www.voltdb.com  
[4] Erlang: http://www.erlang.org  
[5] Deepolis: http://www.deepolis.com  
[6] Choosing the Best Database For a Game: http://voltdb.com/dig-deeper/webinars.php  
[7] Why Erlang? http://www.slideshare.net/eonblast/why-erlang-gdc-online-2012  
[8] Erlang and VoltDB: http://www.slideshare.net/eonblast/voltdb-and-erlang-tech-planet-2012   
[9] Erlvolt 0.2: https://github.com/Eonblast/Erlvolt/tree/01b304f8975c2168be105c1b9c972386264c0a4e  
[10] http://blog.voltdb.com/community-contributions-erlang-client-library/  
[11] Erlvolt 0.3: https://github.com/VoltDB/voltdb-client-erlang  
[12] VoltDB 3: http://blog.voltdb.com/introducing-voltdb-3-0/  
[13] Emysql: https://github.com/Eonblast/Emysql  
[14] 695k with Node.js: http://blog.voltdb.com/695k-tps-nodejs-and-voltdb/  
[15] bench.erl: https://github.com/VoltDB/voltdb-client-erlang/blob/master/etc/bench/bench.erl  
[16] Bench README: https://github.com/VoltDB/voltdb-client-erlang/blob/master/etc/bench  
[17] Amazon EC2 Cluster Instances: http://aws.amazon.com/ec2/instance-types/  
[18] Backpressure: http://voltdb.com/docs/UsingVoltDB/DesignAppLogic.php  
[19] Query Source: https://github.com/VoltDB/voltdb/blob/master/examples/voter/src/voter/procedures/Vote.java  
[20] VoltDB Downloads: VoltDB http://voltdb.com/community/downloads.php  
[21] Driver README: https://github.com/VoltDB/voltdb-client-erlang  
[22] Erlvolt Development: https://github.com/Eonblast/Erlvolt  
[23] VoltDB Community Edition: https://github.com/VoltDB/voltdb  

/hd 6 feb 13
