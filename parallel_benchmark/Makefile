
.PHONY: scheduler-test
# -debug true -debug-runtime true -keep g
BINS = scheduler-test future-test
${BINS} : % : %.sml
	../build/bin/multimlton -default-ann 'allowFFI true' -mlb-path-map ../build/lib/sml/basis/parallel/ws6-map $@.mlb
