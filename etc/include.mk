ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS ?= ../include $(wildcard ../deps/*/include)
EBIN_DIRS ?= $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifdef PROFILE
  ERLC_FLAGS += -Dprofile
endif

ifdef DEBUG
  ERLC_FLAGS += +debug_info
endif

ifdef NATIVE
  ERLC_FLAGS += +native -smp 
endif

EBIN_DIR ?= ../ebin
DOC_DIR  ?= ../doc
EMULATOR := beam

ERL_TEMPLATE := $(wildcard *.et)
ERL_SOURCES  := $(wildcard *.erl)
ERL_HEADERS  := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS  := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)
ERL_TEMPLATES := $(ERL_TEMPLATE:%.et=$(EBIN_DIR)/%.beam)
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app) $(ERL_TEMPLATES)

$(EBIN_DIR)/%.$(EMULATOR): %.erl $(ERL_HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<

$(DOC_DIR)/%.html: %.erl
	$(ERL) -noshell -run edoc file $< -run init stop
	mv *.html $(DOC_DIR)

