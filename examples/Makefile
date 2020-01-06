MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64

PROGRAMS= \
	fib \
	tabulate

all: $(PROGRAMS)

$(PROGRAMS): %: src/*
	$(MPL) $(FLAGS) -output $* src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
