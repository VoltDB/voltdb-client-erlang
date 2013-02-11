# dialyzer.mk
# Dialyzer runs, with or without profiler compiled in.
#
#   dialyzer

dialyzer: 
	$(MAKE) $(LESSVERB) dialyzer-noprofile
	$(MAKE) $(LESSVERB) dialyzer-profile

dialyzer-noprofile: clean all
	@(cd examples; $(MAKE) $(LESSVERB) hello-plus DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) hello-barebones DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) parallel DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) voter DEBUG=true)
	@echo The first run with dializer can take a minute for preparations
	@echo You should NOT have commented out the +debug_info flag in include.mk
	@# the following has to be that separeted out as dialyzer
	@# won't catch rebuilt modules using a different set of defines.
	@# also otherwise removed functions can persist in the plt.
	dialyzer --check_plt --apps erts kernel stdlib crypto
	dialyzer --remove_from_plt -r ebin/ 
	dialyzer --add_to_plt -r ebin/
	dialyzer -r ebin/

dialyzer-profile: clean profile-debug
	@(cd examples; $(MAKE) $(LESSVERB) hello-plus PROFILE=true DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) hello-barebones PROFILE=true DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) parallel PROFILE=true DEBUG=true)
	@(cd examples; $(MAKE) $(LESSVERB) voter PROFILE=true DEBUG=true)
	@echo The first run with dializer can take a minute for preparations
	@echo You should NOT have commented out the +debug_info flag in include.mk
	@# the following has to be that separeted out as dialyzer
	@# won't catch rebuilt modules using a different set of defines
	@# also otherwise removed functions can persist in the plt.
	dialyzer --check_plt --apps erts kernel stdlib crypto
	dialyzer --remove_from_plt -r ebin/ 
	dialyzer --add_to_plt -r ebin/
	dialyzer -r ebin/
