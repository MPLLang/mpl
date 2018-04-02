MLTON=../build/bin/mlton-parmem
MLTRACE=../build/bin/mltrace

tab: tab.sml tab.mlb
	$(MLTON) -output tab tab.mlb

tab.dbg: tab.sml tab.mlb
	$(MLTON) -debug true -debug-runtime true -output tab.dbg tab.mlb

fib: fib.sml sources.mlb
	$(MLTON) -output fib sources.mlb

fib.dbg: fib.sml sources.mlb
	$(MLTON) -debug true -debug-runtime true -output fib.dbg sources.mlb

fib.trc: fib.sml sources.mlb
	$(MLTON) -trace true -trace-runtime true -output fib.trc sources.mlb

.PHONY: trace

trace: fib.trc
	$(MLTRACE) record ./fib.trc @MLton number-processors 4 fixed-heap 2G --
	@echo "To visualize: $(MLTRACE) export <file>.trace.gz; $(MLTRACE) visu <file>.sqlite"
