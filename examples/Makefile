MPL=../build/bin/mpl-cc
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

$(PROGRAMS): %: src/* src/lib/*
	$(MPL) $(FLAGS) -output $* src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
