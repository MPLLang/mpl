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
	seam-carve

DBG_PROGRAMS := $(addsuffix .dbg,$(PROGRAMS))

all: $(PROGRAMS)

$(PROGRAMS): %: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* src/$*/sources.mlb
	@echo "successfully built bin/$*"

$(DBG_PROGRAMS): %.dbg: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) $(DBG_FLAGS) -output bin/$*.dbg src/$*/sources.mlb
	@echo "successfully built bin/$*.dbg"


.PHONY: clean phony

phony:

clean:
	rm -rf bin/
