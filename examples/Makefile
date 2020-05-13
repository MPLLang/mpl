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
	nqueens

all: $(PROGRAMS)

$(PROGRAMS:=.dbg): %.dbg: src/* src/lib/*
	$(MPL) $(FLAGS) -debug true -debug-runtime true -keep g -output $@ src/$*.mlb
	@echo "successfully built $@"

$(PROGRAMS): %: src/* src/lib/*
	$(MPL) $(FLAGS) -output $* src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
