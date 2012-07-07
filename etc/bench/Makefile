INCLUDE_DIRS := ../../include
EBIN_DIR := ../../ebin
include ../../etc/include.mk

all: $(EBIN_DIR)/bench.beam

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

bench-vsm: $(EBIN_DIR)/bench.beam
	@### Voter steady managed (reference 13,000 T/sec/core)
	@# VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval
	@erl -pa ../../ebin -s bench run VSM voter 100000 steady 200 x managed 100000 200 100 verbose 100 -s init stop -noshell +P 1000000 -smp +S 1 -config bench

bench-vsd: $(EBIN_DIR)/bench.beam
	@### Voter steady direct  (reference 21,000 T/sec/core)
	@# VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval
	@erl -pa ../../ebin -s bench run VSD voter 100000 steady 200 x direct x x x verbose 200 -s init stop -noshell +P 1000000 -smp +S 1 -config bench

bench-vbm: $(EBIN_DIR)/bench.beam
	@### Voter bursts managed (reference 10,000 T/sec/core)
	@# VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval
	@erl -pa ../../ebin -s bench run VBM voter 100000 bursts 100 1 managed 10000 100 100 verbose 200 -s init stop -noshell +P 1000000 -smp +S 1 -config bench

bench-vbd: $(EBIN_DIR)/bench.beam
	@### Voter bursts direct (reference 16,000 T/sec/core)
	@# VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval
	@erl -pa ../../ebin -s bench run VBD voter 100000 bursts 100 5 direct x x 100 verbose 200 -s init stop -noshell +P 1000000 -smp +S 1 -config bench

bench-hsd: $(EBIN_DIR)/bench.beam
	@### Hello steady direct  (reference 21,000 T/sec/core)
	@# VMID, Bench, ACallTarget, Rhythm, ABurstSize, ADelay, Mode, AQueueSize, ASlots, ATarget, AVerbosity, ADumpInterval
	@erl -pa ../../ebin -s bench run HSD hello 100000 steady 200 x direct x x x verbose 200 -s init stop -noshell +P 1000000 -smp +S 1 -config bench

### Mulit-VM benchmark VSD using etc/bench/benchstart shell script
VMS   ?=     1 # of virtual machines (erl Erlang emulators) started 
CORES ?=     1 # number of cores used per VM (erl parameter +S)	
CALLS ?= 10000 # of transactions per VM for the benchmark
WORKERS ?=   100 # of parallel ('steady') workers = max server load
RHYTHM ?= steady
MODE ?= direct
bench-mvm: $(EBIN_DIR)/bench.beam
	sudo ./benchstart $(VMS) $(CORES) $(CALLS) $(WORKERS) $(RHYTHM) $(MODE) 
	
bench-help:
	# You need to run the VoltDB 'Voter' example database,
	# e.g. by 'cd voltdb/examples/voter && ./run.sh'
	#
	# Benchmarks can be configured with multiple options,
	# inspect etc/bench/Makefile for samples. The easiest
	# way to start out is 'make bench' from Erlvolt root.
	# Six setups are pre-configured, available via make:
	#
	# bench-vsd: parallel workers, directly on the database
	#            the standard bench, same as 'make bench'
	# bench-vsm: parallel workers, managed with a queue
	#            the queue is the driver's overload protection
	# bench-vbm: timed bursts, managed with a queue
	#            testing the queue to buffer away peaks
	# bench-vbd: timed bursts, directly on the database
	#            easy to crash, showing the queue's value
	# bench-mvm: Multiple virtual machines running bench-vsd
	#			 parameters: VMS, CORES, CALLS, WORKERS
	#			 e.g. 'make bench-mvm VMS=10'
	#
	# Parameters to the bench module are, in this order:
	#
	# VMID: Virtual Machine ID, trails screen logs
	# Bench: type of benchmark calls, 'voter' or 'hello'
	# CallTarget: number of calls before benchmark ends
	# Rhythm: timed 'bursts' or 'steady' synched calls 
	# BurstSize: calls per burst or # of steady workers
	# Delay: millisec delay between bursts 
	# Mode: 'direct' calls or using 'managed' driver queue
	# QueueSize: max processes that can wait in queue
	# Slots: max pending request before queueing next ones
	# Target: 'managed': queue size, 'direct': # pending
	# Verbosity: quiet, nonverbose, verbose, veryverbose
	# DumpInterval: for 'make profile' msec between stats
	@# /hd Jan'13
	
clean:
	@rm -f *.log
	@rm -f *.totals
	@rm -f *.dump
	@rm -f *.beam
	@rm -f EBIN_DIR/bench.beam
	
	
