Changes
=======

**Release: 'Erlvolt 0.3.3'**
**Author: H. Diedrich**
**Production: Eonblast Corporation**
**Copyright: (c) 2013 VoltDB, Inc**
**Licence: MIT**
**Date: Feb 11 2013**

0.3.3   11 Feb 2013  hd

        * Compatibility
        * Ease of Build

        + add: R16 compatibility
        + add: VoltDB 3 samples compatibility
        + fix: make dialyzer 
        + add: make make-test
        + fix: empty table catch
        + fix: status value
        + fix: parallel.erl sample
        + alt: cleanup

0.3.02  03 Feb 2013  hd

        * Asynchronous Release
        * 877k Benchmark version
        * Major extensions

        + add: asynchronous procedure calls
        + add: call queue manager
        + add: monitored mode
        + add: benchmarks
        + add: new samples
        + add: new unit tests
        + add: dialyzer checked
        + add: optional profiler
        + add: minute call options
        + add: more documentation

0.3.01  08 Jul 2012  hd

        + add: auth response codes and output in login()
        + fix: tiny int array element count encoding
        + add: service name as parameter instead of hard coded string
        + add: updated comment for roundtrip time value in invocation response
        + alt: reworked Makefile for tests

0.3.00  07 Jul 2012  hd

        + add: Makefiles
        + alt: dir structure, moved tests into test/, erlunit stripped to etc/
        + add: fixed error/1,2 ambiguity in erlunit

0.2.01  15 Jun 2010  hd

        + add: extensive tests for callback id list (test3.erl)
        + alt: changes in heads and body of callProcedure, now w/callback
        + ext: connection record replaces socket-only parameter
        + add: close(Connection) added, and added to all samples.

0.1.03  11 Jun 2010  hd

        + ext: default encoding of array, (binary) string and integer parameters
        + alt: default parameter encoding attempt of any type as string dropped
        + add: more parameter conversion tests

0.1.02  11 Jun 2010  hd

        + fix: timestamps now interpreted as microseconds (thx Ning)
        + License tags now termed precisely as they should

0.1.01  11 Jun 2010  hd

        + Minor changes

0.1     11 Jun 2010  hd

        * First public release