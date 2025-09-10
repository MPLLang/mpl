DEFAULT_FLAGS?=-default-type int64 -default-type word64
SMLC?=mpl

COMPILER_NAME:=$(notdir $(SMLC))

SUBDIRS := $(sort $(dir $(wildcard */*.mlb)))
TARGETS := $(patsubst %/, %, $(SUBDIRS))

SUBDIRS := $(sort $(dir $(wildcard */*.mlb)))
TARGETS := $(patsubst %/, %.bin, $(SUBDIRS))

.PHONY: all clean

all: $(TARGETS)

%.bin: %/*.mlb
	$(SMLC) -output $@ -mlb-path-var 'COMPAT $(COMPILER_NAME)' $(DEFAULT_FLAGS) $<

clean:
	rm -f $(TARGETS)
