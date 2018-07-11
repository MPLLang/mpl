MLTON=../build/bin/mlton-parmem
MLTRACE=../build/bin/mltrace
DBG=-keep g -debug true -debug-runtime true
FLAGS=

.PHONY: tab tab.dbg fib fib.dbg clean

all: fib tab

fib: fib.sml fib.mlb
	$(MLTON) $(FLAGS) -output fib fib.mlb

fib.dbg: fib.sml fib.mlb
	$(MLTON) $(FLAGS) $(DBG) -output fib.dbg fib.mlb

tab: tab.sml tab.mlb
	$(MLTON) $(FLAGS) -output tab tab.mlb

tab.dbg: tab.sml tab.mlb
	$(MLTON) $(FLAGS) $(DBG) -output tab.dbg tab.mlb

clean:
	rm -f tab tab.dbg fib fib.dbg tab.*.c fib.*.c
