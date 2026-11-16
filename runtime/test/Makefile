## Copyright (C) 2026 Kostiantyn Dementiev.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

# Stress tests; build the runtime libraries first, then: make -C runtime/test check

CFLAGS := -std=gnu11 -O2 -g -pthread -Wall -Wextra -pedantic -I..
LIBS := -lgmp -lm

TESTS := hh-union-find-stress hh-union-find-stress-dbgdetect

.PHONY: all check clean
all: $(TESTS)

check: $(TESTS)
	@for t in $(TESTS); do echo "./$$t"; ./$$t || exit 1; done

hh-union-find-stress: hh-union-find-stress.c ../libmlton.a ../libgdtoa.a
	$(CC) $(CFLAGS) -o $@ $< ../libmlton.a ../libgdtoa.a $(LIBS)

hh-union-find-stress-dbgdetect: hh-union-find-stress.c ../libmlton-dbgdetect.a ../libgdtoa-dbgdetect.a
	$(CC) $(CFLAGS) -O0 -DASSERT=1 -DDETECT_ENTANGLEMENT -o $@ $< \
		../libmlton-dbgdetect.a ../libgdtoa-dbgdetect.a $(LIBS)

clean:
	rm -f $(TESTS)
