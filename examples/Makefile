include ../etc/include.mk

all: $(EXAMPLE_EBIN_FILES)

%.beam: %.erl
	erlc -I ../include -o ../ebin +debug_info $<

hello: $(EBIN_DIR)/hello.beam

hello-plus: $(EBIN_DIR)/hello_plus.beam

parallel: $(EBIN_DIR)/parallel.beam

voter: $(EBIN_DIR)/voter.beam

clean:
	@rm -rf $(EBIN_FILES)
	@rm -rf *.beam
	@rm -rf *.log
	@rm -rf *.dump


