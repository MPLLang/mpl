MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64
DBG_FLAGS=-debug true -debug-runtime true -keep g

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

DBG_PROGRAMS := $(addsuffix .dbg,$(PROGRAMS))
SYSMPL_PROGRAMS := $(addsuffix .sysmpl,$(PROGRAMS))

all: $(PROGRAMS)

all-dbg: $(DBG_PROGRAMS)

all-sysmpl: $(SYSMPL_PROGRAMS)

$(PROGRAMS): %: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* src/$*/sources.mlb
	@echo "successfully built bin/$*"

$(DBG_PROGRAMS): %.dbg: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) $(DBG_FLAGS) -output bin/$*.dbg src/$*/sources.mlb
	@echo "successfully built bin/$*.dbg"

$(SYSMPL_PROGRAMS): %.sysmpl: phony
	@mkdir -p bin
	mpl $(FLAGS) -output bin/$*.sysmpl src/$*/sources.mlb
	@echo "successfully built bin/$*.sysmpl"

.PHONY: clean phony

phony:

clean:
	rm -rf bin/
