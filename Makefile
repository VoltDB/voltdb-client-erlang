###-------------------------------------------------------------------------###
### File        : Makefile                                                  ###
### Version     : 0.3.0/beta                                                ###
### Description : Erlang VoltDB driver main build and run rules             ###
### Copyright   : VoltDB, LLC - http://www.voltdb.com                       ###
### Production  : Eonblast Corporation - http://www.eonblast.com            ###
### Author      : H. Diedrich <hd2012@eonblast.com>                         ###
### License     : MIT                                                       ###
### Created     : 17 Apr 2010                                               ###
### Changed     : 02 Feb 2013                                               ###
###-------------------------------------------------------------------------###
###                                                                         ###
###   This driver is being contributed to VoltDB by Eonblast Corporation.   ###
###                                                                         ###
###-------------------------------------------------------------------------###
###                                                                         ###
###    Erlvolt 0.3.0/alpha - Erlang VoltDB client API.                      ###
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
VERSION=0.3.1
PKGNAME=erlvolt
APP_NAME=erlvolt

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
MAKETIME=$(shell date)

#
# Main Build, Hello, Bench
#

# Build the driver, with debug info compiled in
all: app
	@(cd src; $(MAKE) DEBUG=true)
	@(cd etc/bench; $(MAKE) DEBUG=true)

# Hello 'redirects' to a robuster hello world
hello: hello-plus
	# This executed examples/hello_plus.erl.
	# Simplest source see examples/hello.erl,
	# 'make hello-barebones' executes it.
	

#
# Driver compilation variants
#

fast: app
	@(cd src;$(MAKE) NATIVE=true)
	@(cd etc/bench; $(MAKE) NATIVE=true)

profile: app
	@(cd src;$(MAKE) PROFILE=true NATIVE=true)
	@(cd etc/bench; $(MAKE) PROFILE=true NATIVE=true)

profile-debug: app
	@(cd src;$(MAKE) PROFILE=true DEBUG=true)
	@(cd etc/bench;$(MAKE) PROFILE=true DEBUG=true)

app: ebin/$(PKGNAME).app

ebin/$(PKGNAME).app: src/$(PKGNAME).app.src
	@mkdir -p ebin
	@sed -e 's/modules, \[\]/{modules, [$(MODULES)]}/;s/%MAKETIME%/$(MAKETIME)/' < $< > $@

#
# Hello variants
#

# A slightly more robust hello world: hello_plus.erl
hello-plus: all
	@(cd examples; $(MAKE) hello-plus DEBUG=true)
	erl -pa ./ebin -s hello_plus run -s init stop -noshell 

# A simpler hello.erl
hello-barebones: all
	@(cd examples; $(MAKE) hello DEBUG=true)
	erl -pa ./ebin -s hello run -s init stop -noshell 

# A more Erlang hello world with many processes.
parallel: all
	@(cd examples; $(MAKE) parallel DEBUG=true)
	erl -pa ./ebin -s parallel run -s init stop -noshell 

#
# Voter sample
#

voter: all
	@(cd examples; $(MAKE) voter DEBUG=true)
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
	@(cd etc/bench; $(MAKE) bench-vsm NATIVE=true)

### Voter steady direct (reference 21,000 T/sec/core)
bench-vsd:
	@(cd etc/bench; $(MAKE) bench-vsd NATIVE=true)

### Voter bursts managed (reference 10,000 T/sec/core)
bench-vbm:
	@(cd etc/bench; $(MAKE) bench-vbm NATIVE=true)

### Voter bursts direct (reference 16,000 T/sec/core)
bench-vbd:
	@(cd etc/bench; $(MAKE) bench-vbd NATIVE=true)

### Hello steady direct (reference 21,000 T/sec/core)
bench-hsd:
	@(cd etc/bench; $(MAKE) bench-hsd NATIVE=true)

### Multi-VM benchmark (VSD)
# optional parameters (default):
# VMS (5) # of virtual machines (erl Erlang emulators) started 
# CORES (1) # number of cores used per VM (erl parameter +S)
# CALLS (100000) # of transactions per VM for the benchmark
# SPAWN (100) # of parallel ('steady') workers = max server load
# use e.g. make bench-mvm CALLS=1000000
bench-mvm:
	@(cd etc/bench; $(MAKE) bench-mvm)
	
bench-help:
	@(cd etc/bench; $(MAKE) bench-help)
	
#
# Doc Creation
#

# Create HTML from Markdown to test README.md appearance
html:
	lua etc/markdown.lua README.md
	lua etc/markdown.lua doc/CHANGES.md
	lua etc/markdown.lua doc/BENCHMARK1.md
	lua etc/markdown.lua doc/BENCHMARK2.md
	lua etc/markdown.lua etc/bench/README.md && cp etc/bench/README.html doc/BENCHMARK-README.html
#
# Building and Deployment
#

# clean and doc creation for release, any branch 
release: clean html

# clean for master branch
master: release
	etc/replace %-%.* ""
	etc/replace TODO.*$ ""
	rm -f doc/edoc-info
	rm -f doc/erlang.png
	rm -f doc/erlvolt-footer.png
	rm -f doc/erlvolt-style.css
	rm -f doc/stylesheet.css

clean:
	@# these can all appear
	@(cd src; $(MAKE) clean)
	@(cd examples; $(MAKE) clean)
	@(cd etc/bench; $(MAKE) clean)	
	@rm -rf ebin/*.app
	@rm -rf ebin/*.beam
	@rm -rf ebin/*.dump
	@rm -rf ebin/cover
	@rm -f *.beam
	@rm -f *.dump
	@rm -f *.html
	@rm -f *.log
	@rm -f *.totals
	@rm -rf ct_run*
	@rm -f variables-ct*
	@rm -rf etc/test/ct_run*
	@rm -f etc/test/variables-ct*
	@rm -f etc/test/*.beam
	@rm -f etc/test/*.html
	@rm -f etc/test/ct_default.css 
	@rm -f etc/test/jquery*.js 
	@rm -rf $(PKGNAME)-$(VERSION)
	@rm -f $(PKGNAME)-$(VERSION).tgz
	# clean

clean-docs:
	@rm -f doc/readme.edoc
	@rm -f doc/changes.edoc
	@rm -f doc/overview.edoc
	@rm -f doc/*.html
	@rm -f doc/README.html
	@rm -f doc/CHANGES.html
	@rm -f doc/BENCHMARKS.html
	@rm -f doc/BENCHMARK1.html
	@rm -f doc/BENCHMARK2.html
	@rm -f doc/BENCHMARK-README.html

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile README.md src etc examples $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/


#
# Tests
#

test: all
	(cd etc/test; ct_run -suite environment_SUITE basics_SUITE -pa ../../ebin)


