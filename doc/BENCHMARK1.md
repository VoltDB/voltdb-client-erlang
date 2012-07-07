VoltDB Blog: 212 TPS/core with Erlang and VoltDB 
======================================

Henning Diedrich - 31 Jan 2013

**Running on a suitable EC2 configuration (see details below), with our new VoltDB Erlang driver we achieved 212,000 transactions per second.**

I am Henning Diedrich, CEO of [Eonblast Corporation](http://www.eonblast.com) a games company. I would like to introduce the new Erlang VoltDB driver we created, a piece of software that allows two genre-defining technologies to work together: VoltDB and Erlang.

The Driver
--------------

I first came to VoltDB on the hunt for a better database for heavy duty online-game servers. I experienced first hand what a pain it was to try to scale MySQL and found VoltDB uniquely suitable for the requirements of more complex game worlds. Better than any other database in fact.

I had also looked for a better language than Java for programming servers and for that, I had found Erlang. To be able to use them together, I started creating the Erlang driver for VoltDB.

Work for this started three years ago and I donated a first version of the driver to VoltDB at their request in 2009. It was perfectly usable but out of the box only provided for synchronous connections. In 2012 VoltDB decided to sponsor the creation of a bigger and badder version. Now the real deal has arrived.

The benchmark described below was made with the new, asynchronous driver. It is pure Erlang, full of parallel microprocesses, blazingly fast and fit for VoltDB 3. It incorporates almost all of the previous, robust driver version. And on my quest to ensure reliable, consistently high throughput, I was able to draw from my experience maintaining an Erlang MySQL driver, Emysql. The connection pooling and call queueing is modeled after the ones used in this reliable workhorse, which was originally designed at Electronic Arts. They enable the Erlang driver to absorb access peaks, and to distribute load across VoltDB server nodes.

To come up with a useful benchmark script I could employ the lessons learned from the Node.js benchmark I did for VoltDB a while ago, see http://blog.voltdb.com/695k-tps-nodejs-and-voltdb/. This time around I knew which numbers I would be looking for and what double checks I should have in place to put the cloud cluster to good use.

The internal structure of the driver has been implemented as would be expected: your application's microprocesses are made to send a message to a dedicated connection process that handles the socket work. After the request is sent, the initiating process is either blocked in a synchronous receive (this, of course, does not block all your other processes) or goes on to to use its time as it pleases, if you choose the asynchronous mode. The answer from the server arrives in your processes' mailbox. 

There are many options that you can use. E.g. the 'monitored' mode, where a worker process is created that handles the sending of the request, shielding your initiating process from any hiccups in the driver. You can 'fire and forget', for writes where you don't care to hear that they succeeded. Or 'blowout' if you don't even care to hear about failure.

The Benchmark Application
-------------------------------------

The benchmark is based on the VoltDB voter example, which comes with every VoltDB distribution. It 'validates and stores phoned-in votes for talent show contestants'. In the original example setup there is a web page that displays the results for illustration, updated every 400ms. You'll find it in the <VoltDB_Home>/examples/voter directory.

The benchmark starts out with a preparational phase, where the database is filled with 6 contestants' names and then one million write transactions are fired towards the server, per CPU core, that each register a 'vote' for one of the contestant, picked at random. In the end, the votes won by each contestants are displayed, using a materialized view and a VoltDB ad-hoc query.

The Erlang benchmark application that implements a Voter client is packaged with the driver, which can be downloaded here https://github.com/VoltDB/voltdb-client-erlang. The benchmark source is under etc/bench, where you'll also find a detailed README.md that explains how to build and deploy the application. For a (slow) test run on localhost, it's basically:

    $ git clone git://github.com/VoltDB/voltdb.git voltdb
    $ git clone git://github.com/VoltDB/voltdb-client-erlang.git erlvolt
    $ cd voltdb/examples/voter && ./run.sh &
    $ cd && cd erlvolt && make bench

That should give you a screen like this:

		metal:~ hd$ cd voltdb/examples/voter && ./run.sh &
		[1] 62084
		metal:~ hd$ Initializing VoltDB...
		
		_    __      ____  ____  ____ 
		| |  / /___  / / /_/ __ \/ __ )
		| | / / __ \/ / __/ / / / __  |
		| |/ / /_/ / / /_/ /_/ / /_/ / 
		|___/\____/_/\__/_____/_____/
		
		--------------------------------
		
		WARN: Running 3.0 preview (iv2 mode). NOT SUPPORTED IN PRODUCTION.
		Build: 3.0 voltdb-3.0-beta2-110-g178a1e6 Community Edition
		Connecting to VoltDB cluster as the leader...
		Appointing HSId 0:0 as leader for partition 0
		Appointing HSId 0:1 as leader for partition 1
		MP 0:6 for partition 16383 finished leader promotion. Took 104 ms.
		WARN: Mailbox is not registered for site id -4
		Initializing initiator ID: 0, SiteID: 0:13
		WARN: Running without redundancy (k=0) is not recommended for production use.
		Server completed initialization.

		cd && cd ErlVolt2 && make bench
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_app.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_conn.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_conn_mgr.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_profiler.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_sup.erl
		erlc -W -I ../include  +native -smp  -o ../ebin erlvolt_wire.erl
		erlc -W -I ../../include  +native -smp  -o ../../ebin bench.erl
		
		Erlvolt Bench 0.9 (client 'VSD')
		-------------------------------------------------------------------------------------------------------------------------------------
		Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 'n/a' stats/sec
		Hosts: localhost:21212 localhost:21212 
		connect ...
		preparation ...
		calls ... ........................................................................................................................................
		cool down ... 
		check writes ... ok
		results ...  votes:     100,000 (6 contestants)
		....Jessie Alloway:      16,811
		...Tabatha Gehling:      16,661
		....Jessie Eichman:      16,643
		.....Alana Bregman:      16,634
		.....Edwina Burnam:      16,632
		......Kelly Clauss:      16,619
		close pool ...
		Client 'VSD', voter, 100,000 calls, steady, 200 workers, delay n/a, direct, queue n/a, slots n/a, limit n/a, verbose, 'n/a' stats/sec
		-------------------------------------------------------------------------------------------------------------------------------------
		Client 'VSD' overall: 3,657 T/sec throughput, 0.00% fails, total transactions: 100,000, fails: 0, total time: 27.338sec 
		Erlvolt 0.3.0, bench started 2013-01-31 22:09:27, ended 2013-01-31 22:09:54, database: +100,000 new votes
		[+++]
		metal:ErlVolt2 hd$ 


Instructions on how to use it in the cloud can be found in etc/bench/README.me.


The Benchmark Results
--------------------------------

Using 3 nodes connected to a 5 node VoltDB cluster, each client node executed an average of 70,600 TPS for a total of 212,000 TPS. Clients and server worked at only 40% load, so more tuning would probably have yielded higher results, on both client and server side. 

Since this benchmark was about the driver, not the server, I made no attempts to tune the server cluster.


The Environment
-----------------------

I created an 8-node Amazon EC2 m3.2xlarge cluster broken up into Erlang client and VoltDB server nodes. The m3.2xlarge provide the following, as described by the Amazon EC2 Instance Types page:

### M3 Double Extra Large Instance

* 30 GiB memory
* 26 EC2 Compute Units (8 virtual cores with 3.25 EC2 Compute Units each)
* 64-bit platform
* I/O Performance: High
* API name: m3.2xlarge

These nodes were configured with:

* Ubuntu Server 12.04 LTS for Cluster Instances AMI
* Oracle JDK 1.7
* Erlang 15
* VoltDB Enterprise Edition 3.0 RC

Each of the five server nodes was set to six partitions, so I had 30 partitions across the cluster.


The Transactions
-----------------------

The clients "firehosed" the VoltDB cluster by calling Voters vote() stored procedure continuously. This procedure performed not merely one write but, depending on how you count, 4 to 6 operations:

* Retrieve the callers location (select)
* Verify that the caller had not exceeded his/her vote maximum (select)
* Verify that the caller was voting for a valid contestant (select)
* If yes to all of the above, a vote was cast on behalf of that caller (insert)

On top of this, each insert also triggered an update to two different materialized views.

Consequently, the 212,000 TPS performed 848,000 SQL operations per second, i.e. three selects and one insert. 

Observations & Notes
-----------------------------

The most important number from this, to my mind, is the pretty constant 9,000 transactions per second per CPU core that I saw. This will allow you to make rough estimates on the amount of hardware you may need on the business server side (the VoltDB 'client' side).

We decided for the Amazon Elastic Cloud for the benchmark in the hopes that this would result into the most transparent setup. A local cluster of eight "bare metal" nodes would certainly perform better than the EC2 instance, and be way more economic if you used them on a daily basis. But our throughput numbers would be hard to reproduce independently.

As it is, you could try the exact same benchmark yourself, easily. VoltDB and the new driver can be downloaded from http://voltdb.com/community/downloads.php. The README.md of the driver has more instructions on how to use it. To find experimental new versions of the driver as well as fast bug fixes, try https://github.com/VoltDB/voltdb-client-erlang, and VoltDB is also on github, at https://github.com/VoltDB/voltdb.

/hd 31 jan 13