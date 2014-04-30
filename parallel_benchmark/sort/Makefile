
BIN = sort
PLOTS = scheduling-overhead msort-speedup msorti-speedup qsort-speedup msort-space msortl-speedup msortl-space qsortl-space qsortt-space qsortt-speedup msortf-steals msortf-suspends qsortfl qsortfl-space msortft msortft-space msortft2 msortft2-space

include ../Makefile.include

.PHONY: sort-hax
sort-hax:
	for i in 0 1 2 3; do gcc -g2 -O0 -m64 -o sort.$${i}.o -c sort.$${i}.c -I ../../build/lib/self/include/ -I ../../build/lib/include/; done
	gcc -m64 -o sort-hax sort.*.o -lmlton -lgdtoa -lm -lgmp -lpthread -lplpa_included -lrt -L ../../build/lib/self
