MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64

PROGRAMS= \
  fib \
	tab-tree

all: $(addprefix bin/, $(PROGRAMS))

bin/%: shared/* %/*.mlb %/*.sml
	@mkdir -p bin
	$(MPL) $(FLAGS) -output bin/$* $*/sources.mlb
	@echo "successfully built bin/$*"
