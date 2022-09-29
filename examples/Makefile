MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64
DBG_FLAGS=-debug true -debug-runtime true -keep g
TRACE_FLAGS=-trace true -trace-runtime true
NODETECT_FLAGS=-detect-entanglement false

PROGRAMS= \
	fib \
	random \
	primes \
	msort \
	dmm \
	ray \
	tokens \
	nn \
	dedup \
	nqueens \
	reverb \
	seam-carve \
	coins

TRACE_PROGRAMS := $(addsuffix .trace,$(PROGRAMS))
DBG_PROGRAMS := $(addsuffix .dbg,$(PROGRAMS))
NODETECT_PROGRAMS := $(addsuffix .nodetect,$(PROGRAMS))
SYSMPL_PROGRAMS := $(addsuffix .sysmpl,$(PROGRAMS))

all: $(PROGRAMS)

all-dbg: $(DBG_PROGRAMS)

all-nodetect: $(DETECT_PROGRAMS)

all-sysmpl: $(SYSMPL_PROGRAMS)

$(PROGRAMS): %: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* src/$*/sources.mlb
	@echo "successfully built bin/$*"

$(DBG_PROGRAMS): %.dbg: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) $(DBG_FLAGS) -output bin/$*.dbg src/$*/sources.mlb
	@echo "successfully built bin/$*.dbg"

$(TRACE_PROGRAMS): %.trace: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) $(TRACE_FLAGS) -output bin/$*.trace src/$*/sources.mlb
	@echo "successfully built bin/$*.trace"

$(NODETECT_PROGRAMS): %.nodetect: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) $(NODETECT_FLAGS) -output bin/$*.nodetect src/$*/sources.mlb
	@echo "successfully built bin/$*.nodetect"

$(SYSMPL_PROGRAMS): %.sysmpl: phony
	@mkdir -p bin
	mpl $(FLAGS) -output bin/$*.sysmpl src/$*/sources.mlb
	@echo "successfully built bin/$*.sysmpl"

.PHONY: clean phony

phony:

clean:
	rm -rf bin/
