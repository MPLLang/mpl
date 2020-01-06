MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64

PROGRAMS= \
	fib \
	tabulate \
	primes

all: $(PROGRAMS)

$(PROGRAMS): %: src/* src/lib/*
	$(MPL) $(FLAGS) -output $* src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
