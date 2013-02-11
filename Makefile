###-------------------------------------------------------------------------###
### File        : Makefile                                                  ###
### Version     : 0.3/beta                                                  ###
### Description : Erlang VoltDB driver main build and run rules             ###
### Copyright   : VoltDB, LLC - http://www.voltdb.com                       ###
### Production  : Eonblast Corporation - http://www.eonblast.com            ###
### Author      : H. Diedrich <hd2012@eonblast.com>                         ###
### License     : MIT                                                       ###
### Created     : 17 Apr 2010                                               ###
### Changed     : 06 Feb 2013                                               ###
###-------------------------------------------------------------------------###
###                                                                         ###
###   This driver is being contributed to VoltDB by Eonblast Corporation.   ###
###                                                                         ###
###-------------------------------------------------------------------------###
###                                                                         ###
###    Erlvolt 0.3/beta    - Erlang VoltDB client API.                      ###
###                                                                         ###
###    This file is part of VoltDB.                                         ###
###    Copyright (C) 2008-2013 VoltDB, LLC http://www.voltdb.com            ###
###    Author H. Diedrich <hd2012@eonblast.com> http://www.eonblast.com     ###
###                                                                         ###
### Permission is hereby granted, free of charge,  to any person obtaining  ###
### a copy  of this  software  and  associated  documentation  files  (the  ###
### "Software"),  to deal in the  Software without restriction,  including  ###
### without limitation the rights to use,  copy,  modify,  merge, publish,  ###
### distribute,  sublicense,  and/or sell copies of the  Software,  and to  ###
### permit persons to whom the Software is furnished to do so,  subject to  ###
### the following conditions:                                               ###
###                                                                         ###
### The  above  copyright  notice  and  this  permission  notice  shall be  ###
### included in all copies or substantial portions of the Software.         ###
###                                                                         ###
### THE  SOFTWARE  IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY  OF ANY KIND,  ###
### EXPRESS OR IMPLIED,  INCLUDING  BUT  NOT LIMITED  TO THE WARRANTIES OF  ###
### MERCHANTABILITY, FITNESS  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ###
### IN NO EVENT SHALL  THE AUTHORS  BE LIABLE  FOR  ANY CLAIM,  DAMAGES OR  ###
### OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT  OR OTHERWISE,  ###
### ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR  ###
### OTHER DEALINGS IN THE SOFTWARE.                                         ###
###                                                                         ###
###-------------------------------------------------------------------------###
###                                                                         ###
### USAGE                                                                   ###
###                                                                         ###
### You can run a sample using the 'Hello' tutorial-server discussed        ###
### in the VoltDB manual and present in every VoltDB distribution.          ###
###                                                                         ###
### Start that server from your voltdb installation with:                   ###
###                                                                         ###
###     $ cd voltdb/doc/tutorial/helloworld                                 ###
###     $ ./run.sh                                                          ###
###                                                                         ###
### Then run the hello world example, using make from the driver root:      ###
###                                                                         ###
###     $ make hello                                                        ###
### or                                                                      ###
###     $ make                                                              ###
###     $ cd examples                                                       ###
###     $ erlc -I ../include -o ../ebin +debug_info hello_plus.erl          ###
###     $ erl -pa ../ebin -s hello_plus run -s init stop -noshell           ###
###                                                                         ###
### You will see this response, 'Hello, world!' in Swedish:                 ###
###                                                                         ###
###     Hej världen!                                                        ###
###                                                                         ###
### The hello world source is found in examples/hello_plus.erl              ###
###                                                                         ###
###-------------------------------------------------------------------------###
###                                                                         ###
### See README.md or .html for instructions,  examples/ for more examples.  ###
### See doc/BENCHMARKS.md or .html for a description of driver benchmarks.  ###
###                                                                         ###
### For getting started with VoltDB,see: voltdb/doc/GettingStarted.pdf or   ###
### online:  http://voltdb.com/downloads/documentation/GettingStarted.pdf.  ###
###                                                                         ###
###-------------------------------------------------------------------------###

LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.3.3
PKGNAME=erlvolt
APP_NAME=erlvolt

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
MAKETIME=$(shell date)
LESSVERB=--no-print-directory

#
# Main Build, Hello, Bench
#

# Build the driver, with debug info compiled in
all: app
	@(cd src; $(MAKE) $(LESSVERB) DEBUG=true)
	@(cd etc/bench; $(MAKE) $(LESSVERB) DEBUG=true)

# Hello 'redirects' to a robuster hello world
hello: hello-plus
	# This executed examples/hello_plus.erl.
	# Simplest source see examples/hello.erl,
	# 'make hello-barebones' executes that.
	

#
# Driver compilation variants
#

fast: app
	@(cd src;$(MAKE) $(LESSVERB) NATIVE=true)
	@(cd etc/bench; $(MAKE) $(LESSVERB) NATIVE=true)

profile: app
	@(cd src;$(MAKE) $(LESSVERB) PROFILE=true NATIVE=true)
	@(cd etc/bench; $(MAKE) $(LESSVERB) PROFILE=true NATIVE=true)

profile-debug: app
	@(cd src;$(MAKE) $(LESSVERB) PROFILE=true DEBUG=true)
	@(cd etc/bench;$(MAKE) $(LESSVERB) PROFILE=true DEBUG=true)

app: ebin/$(PKGNAME).app

ebin/$(PKGNAME).app: src/$(PKGNAME).app.src
	@mkdir -p ebin
	@sed -e 's/modules, \[\]/{modules, [$(MODULES)]}/;s/%MAKETIME%/$(MAKETIME)/' < $< > $@

#
# Hello variants
#

# A slightly more robust hello world: hello_plus.erl
hello-plus: all
	@(cd examples; $(MAKE) $(LESSVERB) hello-plus DEBUG=true)
	erl -pa ./ebin -s hello_plus run -s init stop -noshell 

# A simpler hello.erl
hello-barebones: all
	@(cd examples; $(MAKE) $(LESSVERB) hello DEBUG=true)
	erl -pa ./ebin -s hello run -s init stop -noshell 

# A simpler hello.erl
hello-barebones-pre3: all
	@(cd examples; $(MAKE) $(LESSVERB) hello-barebones-pre3 DEBUG=true)
	erl -pa ./ebin -s hello_pre3 run -s init stop -noshell 

# A more Erlang hello world with many processes.
parallel: all
	@(cd examples; $(MAKE) $(LESSVERB) parallel DEBUG=true)
	erl -pa ./ebin -s parallel run -s init stop -noshell 

# A more Erlang hello world with many processes, for VoltDB pre 3.0.
# The only difference is a switch in the column order in the hello sample.
parallel-pre3: all
	@(cd examples; $(MAKE) $(LESSVERB) parallel-pre3 DEBUG=true)
	erl -pa ./ebin -s parallel_pre3 run -s init stop -noshell 


#
# Voter sample
#

voter: all
	@(cd examples; $(MAKE) $(LESSVERB) voter DEBUG=true)
	erl -pa ./ebin -s voter run -s init stop -noshell 

#
# Benchmark variants
#

bench: bench-vsd
	# 'make clean fast bench' for faster, HiPE-compiled beams. 
	# 'make clean profile bench' for rolling stats during bench. 

benches: bench-vsm bench-vsd bench-vbm bench-vbd

### Voter steady managed (reference 13,000 T/sec/core)
bench-vsm:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-vsm NATIVE=true)

### Voter steady direct (reference 21,000 T/sec/core)
bench-vsd:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-vsd NATIVE=true)

### Voter bursts managed (reference 10,000 T/sec/core)
bench-vbm:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-vbm NATIVE=true)

### Voter bursts direct (reference 16,000 T/sec/core)
bench-vbd:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-vbd NATIVE=true)

### Hello steady direct (reference 21,000 T/sec/core)
bench-hsd:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-hsd NATIVE=true)

### Multi-VM benchmark (VSD)
# optional parameters (default):
# VMS (5) # of virtual machines (erl Erlang emulators) started 
# CORES (1) # number of cores used per VM (erl parameter +S)
# CALLS (100000) # of transactions per VM for the benchmark
# SPAWN (100) # of parallel ('steady') workers = max server load
# use e.g. make bench-mvm CALLS=1000000
bench-mvm:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-mvm)
	
bench-help:
	@(cd etc/bench; $(MAKE) $(LESSVERB) bench-help)
	
#
# Doc Creation
#

# Create doc HTML from source comments
# IF THIS FAILS, IT'S THE -E
docs: clean-docs
	@echo make docs
	sed -E -f etc/markedoc.sed README.md > doc/readme.edoc
	sed -E -f etc/markedoc.sed CHANGES.md > doc/changes.edoc
	erl -noshell -run edoc_run application "'erlvolt'" '"."' '[{def,{vsn,""}},{stylesheet, "erlvolt-style.css"}]'
	LANG=C sed -E -i "" -e "s/<table width=\"100%\" border=\"1\"/<table width=\"100%\" class=index border=\"0\"/" doc/*.html

# Pushes created docs into dir ../Erlvolt-github-pages to push to github pages.
# Make sure to do 'make docs' first.
# will fail if you haven't checked out github pages into ../Erlvolt-github-pages
pages:
	(cd ../Erlvolt-github-pages; git pull origin gh-pages)
	cp -r doc/* ../Erlvolt-github-pages
	(cd ../Erlvolt-github-pages; git add .; git commit -m 'make pages'; git push origin gh-pages)

# Create HTML from Markdown to test README.md appearance
html:
	@echo make html
	lua etc/markdown.lua README.md
	lua etc/markdown.lua doc/CHANGES.md
	lua etc/markdown.lua doc/BENCHMARK1.md
	lua etc/markdown.lua etc/bench/README.md && cp etc/bench/README.html doc/BENCHMARK-README.html
#
# Building and Deployment
#

# clean and doc creation for release, any branch 
release: clean html

clean:
	@echo clean
	@# these can all appear
	@(cd src; $(MAKE) $(LESSVERB) clean)
	@(cd examples; $(MAKE) $(LESSVERB) clean)
	@(cd etc/bench; $(MAKE) $(LESSVERB) clean)	
	@rm -f ct_run*
	@rm -f .DS_Store
	@rm -rf */.DS_Store
	@rm -rf */*.DS_Store
	@rm -rf ebin/*.app
	@rm -rf ebin/*.beam
	@rm -rf ebin/*.dump
	@rm -rf ebin/cover
	@rm -f *.beam
	@rm -f *.dump
	@rm -f *.log
	@rm -f *.totals
	@rm -f variables-ct*
	@rm -rf etc/test/ct_run*
	@rm -f etc/test/variables-ct*
	@rm -f etc/test/*.beam
	@rm -f etc/test/*.html
	@rm -f etc/test/ct_default.css 
	@rm -f etc/test/jquery*.js 
	@rm -f doc/.!*.html 
	@rm -rf $(PKGNAME)-$(VERSION)
	@rm -f $(PKGNAME)-$(VERSION).tgz

clean-docs:
	@echo clean docs
	@rm -f doc/readme.edoc
	@rm -f doc/changes.edoc
	@rm -f doc/overview.edoc
	@rm -f doc/*.html
	@rm -f doc/.!*.html 
	@rm -f doc/README.html
	@rm -f doc/CHANGES.html
	@rm -f doc/BENCHMARKS.html
	@rm -f doc/BENCHMARK1.html
	@rm -f doc/BENCHMARK-README.html

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile README.md README.html src doc etc examples $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

#
# Unit Tests
#

test: all
	(cd etc/test; ct_run -suite environment_SUITE basics_SUITE -pa ../../ebin)

#
# Dialyzer Tests
#

include etc/test/dialyzer.mk


#
# Make Help
#

help:
	# building
	# --------
	# all: Build all, with debug info, w/o profiler
	# fast: Build native HiPE beams, 20% faster
	# profile: Build native and with profiler, which is slower
	# profile-debug: Build with profiler and debug info
	# clean: Clean all built files
	#
	# examples
	# --------
	# hello: Build hello world example, needs running hello world database
	# hello-plus: A slightly more robust hello world: hello_plus.erl
	# hello-barebones: A simpler hello.erl
	# hello-barebones-pre3: A simpler hello.erl
	# parallel: A more Erlang hello world with many processes.
	# parallel-pre3: A more Erlang hello world with many processes, for VoltDB pre 3.0.
	# voter: Build voter example, needs running voter database
	#
	# benchmarks
	# ----------
	# bench-help: More details on the following bench rules
	# bench: Alias for bench-vsd
	# benches: Bench-vsm bench-vsd bench-vbm bench-vbd
	# bench-vsm: Voter steady managed (reference 13,000 T/sec/core)
	# bench-vsd: Voter steady direct (reference 21,000 T/sec/core)
	# bench-vbm: Voter bursts managed (reference 10,000 T/sec/core)
	# bench-vbd: Voter bursts direct (reference 16,000 T/sec/core)
	# bench-hsd: Hello steady direct (reference 21,000 T/sec/core)
	# bench-mvm: Multi-VM benchmark (VSD)
	#   	Optional parameters (default):
	#   	VMS (5) # of virtual machines (erl Erlang emulators) started 
	#   	CORES (1) # number of cores used per VM (erl parameter +S)
	#   	CALLS (100000) # of transactions per VM for the benchmark
	#   	SPAWN (100) # of parallel ('steady') workers = max server load
	#   	Use e.g. make bench-mvm CALLS=1000000
	#
	# documentation building
	# ----------------------
	# docs: Create doc HTML from source comments
	# html: Create HTML from Markdown to test README.md appearance
	# clean-docs: Clean the files built by make docs and make html
	#
	# release building
	# ----------------
	# release: Clean and doc creation for release, any branch 
	# package: make zipped tarball
	#
	# source tests
	# ------------
	# test: run Common Tests (unit tests)
	# dialyzer: run dialyzer source tests
	@echo
