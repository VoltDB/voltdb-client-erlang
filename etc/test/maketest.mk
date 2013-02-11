# maketest.mk
# Tests sequences of compile, example and bench makes.
#
#   maketest-hello
#   maketest-hello-pre3
#   maketest-voter

maketest-hello:
	$(MAKE) clean
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones
	$(MAKE) parallel
	$(MAKE) bench-hsd
	:
	$(MAKE) clean all
	$(MAKE) hello
	$(MAKE) clean hello
	$(MAKE) clean all
	$(MAKE) hello-plus
	$(MAKE) clean hello-plus
	$(MAKE) clean all
	$(MAKE) hello-barebones
	$(MAKE) clean hello-barebones
	$(MAKE) clean all
	$(MAKE) parallel
	$(MAKE) clean parallel
	$(MAKE) bench-hsd
	:
	$(MAKE) clean fast
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones
	$(MAKE) parallel
	$(MAKE) bench-hsd
	:
	$(MAKE) clean profile
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones
	$(MAKE) parallel
	$(MAKE) bench-hsd
	:
	$(MAKE) clean profile
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones
	$(MAKE) parallel
	$(MAKE) bench-hsd

maketest-hello-pre3:
	$(MAKE) clean
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones-pre3
	$(MAKE) parallel-pre3
	:
	$(MAKE) clean all
	$(MAKE) hello
	$(MAKE) clean hello
	$(MAKE) clean all
	$(MAKE) hello-plus
	$(MAKE) clean hello-plus
	$(MAKE) clean all
	$(MAKE) hello-barebones-pre3
	$(MAKE) clean hello-barebones-pre3
	$(MAKE) clean all
	$(MAKE) parallel-pre3
	$(MAKE) clean parallel-pre3
	:
	$(MAKE) clean fast
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones-pre3
	$(MAKE) parallel-pre3
	:
	$(MAKE) clean profile
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones-pre3
	$(MAKE) parallel-pre3
	:
	$(MAKE) clean profile
	$(MAKE) hello
	$(MAKE) hello-plus
	$(MAKE) hello-barebones-pre3
	$(MAKE) parallel-pre3

maketest-voter:
	$(MAKE) clean all
	$(MAKE) voter
	$(MAKE) bench
	$(MAKE) benches
	$(MAKE) bench-mvm
	$(MAKE) bench-vsd
	$(MAKE) bench-vsm
	$(MAKE) bench-vbd
	$(MAKE) bench-vbm
	:
	$(MAKE) clean fast
	$(MAKE) voter
	$(MAKE) bench
	$(MAKE) benches
	$(MAKE) bench-mvm
	$(MAKE) bench-vsd
	$(MAKE) bench-vsm
	$(MAKE) bench-vbd
	$(MAKE) bench-vbm
	:
	$(MAKE) clean profile
	$(MAKE) voter
	$(MAKE) bench
	$(MAKE) benches
	$(MAKE) bench-mvm
	$(MAKE) bench-vsd
	$(MAKE) bench-vsm
	$(MAKE) bench-vbd
	$(MAKE) bench-vbm
	:
	$(MAKE) clean profile-debug
	$(MAKE) voter
	$(MAKE) bench
	$(MAKE) benches
	$(MAKE) bench-mvm
	$(MAKE) bench-vsd
	$(MAKE) bench-vsm
	$(MAKE) bench-vbd
	$(MAKE) bench-vbm
