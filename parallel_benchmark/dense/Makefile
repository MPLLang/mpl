
BIN = dense
PLOTS = mmm-speedup mmm-space scheduling-overhead mmm-headtohead mmm-blocking mmm-restrict mmm-space-noblk mmm-space-noblk-zoom

include ../Makefile.include

.PHONY: dense-hax
dense-hax:
	for i in 0 1 2 3; do gcc -g2 -O0 -o dense.${i}.o -c dense.${i}.c -I ../../build/lib/self/include/ -I ../../build/lib/include/; done
	gcc -o dense dense.*.o -lmlton -lgdtoa -lm -lgmp -lpthread -lplpa_included -L ../../build/lib/self
