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
	nqueens \
	reverb \
	seam-carve

all: $(PROGRAMS)

$(PROGRAMS): %: phony
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* src/$*/sources.mlb
	@echo "successfully built bin/$*"

.PHONY: clean phony

phony:

clean:
	rm -rf bin/
