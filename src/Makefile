ERLC = erlc
EBIN = ../ebin
ERLINC = ../include 
ERL_FLAGS = -I$(ERLINC) -I$(ENGINE_PATH)/include +warn_unused_vars +debug_info

MODULES = \
	main \
	config \
	font \
	box \
	graphics \
	gr

INCLUDES =

TARGETS = $(MODULES:%=$(EBIN)/%.beam)

all:	$(TARGETS)

clean:
	rm -f  $(TARGETS) erl_crash.dump

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERL_FLAGS) -o$(EBIN) $<
