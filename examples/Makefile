MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64

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

all: $(PROGRAMS)

$(PROGRAMS): %: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* src/$*/sources.mlb
	@echo "successfully built bin/$*"

$(PROGRAMS:=.dbg): %.dbg: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -debug true -debug-runtime true -keep g -output bin/$*.dbg src/$*/sources.mlb
	@echo "successfully built $@"

.PHONY: clean phony

phony:

clean:
	rm -rf bin/
