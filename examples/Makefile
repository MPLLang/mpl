MLTON=MLTON_JOBS=32 ../build/bin/mlton-parmem
MLTRACE=../build/bin/mltrace
DBG=-keep g -debug true -debug-runtime true
FLAGS=

.PHONY: tab tab.dbg fib fib.dbg fib.trc trace clean

tab: tab.sml tab.mlb
	$(MLTON) $(FLAGS) -output tab tab.mlb

tab.dbg: tab.sml tab.mlb
	$(MLTON) $(FLAGS) $(DBG) -output tab.dbg tab.mlb

badmap: badmap.sml badmap.mlb
	$(MLTON) $(FLAGS) -output badmap badmap.mlb

badmap.dbg: badmap.sml badmap.mlb
	$(MLTON) $(FLAGS) $(DBG) -output badmap.dbg badmap.mlb

prom: prom.sml prom.mlb
	$(MLTON) $(FLAGS) -output prom prom.mlb

prom.dbg: prom.sml prom.mlb
	$(MLTON) $(FLAGS) $(DBG) -output prom.dbg prom.mlb

fib: fib.sml fib.mlb
	$(MLTON) $(FLAGS) -output fib fib.mlb

fib.dbg: fib.sml fib.mlb
	$(MLTON) $(FLAGS) $(DBG) -output fib.dbg fib.mlb

fib.trc: fib.sml fib.mlb
	$(MLTON) $(FLAGS) -trace true -trace-runtime true -output fib.trc fib.mlb

trace: fib.trc
	$(MLTRACE) record ./fib.trc @MLton number-processors 4 fixed-heap 2G --
	@echo "To visualize: $(MLTRACE) export <file>.trace.gz; $(MLTRACE) visu <file>.sqlite"

clean:
	rm -f tab tab.dbg fib fib.dbg fib.trc tab.*.c fib.*.c