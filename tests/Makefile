PBBS_DIR        ?= pbbsbench
INPUT_DIR       := inputs
BIN_DIR         := bin


#=================inputs=================
RAND_PTS        := $(PBBS_DIR)/testData/geometryData/randPoints
RMAT_GRAPH      := $(PBBS_DIR)/testData/graphData/rMatGraph

UC_INPUTS       := $(INPUT_DIR)/uniform-circle-1M $(INPUT_DIR)/uniform-circle-20M
RMAT_INPUTS     := $(INPUT_DIR)/rmat-1M-symm $(INPUT_DIR)/rmat-10M-symm
RMAT_BIN_INPUTS := $(patsubst %,%-bin,$(RMAT_INPUTS))
TEXT_INPUTS     := $(INPUT_DIR)/words-8 $(INPUT_DIR)/words-32
ALL_INPUTS      := $(UC_INPUTS) $(RMAT_INPUTS) $(RMAT_BIN_INPUTS) $(TEXT_INPUTS)

.PHONY: all clean deepclean

all: input tests

input: $(ALL_INPUTS)

$(PBBS_DIR):
	git clone https://github.com/cmuparlay/pbbsbench $@
	git -C $@ submodule update --init --recursive

$(RAND_PTS) $(RMAT_GRAPH): $(PBBS_DIR)
	$(MAKE) -C $(@D) $(@F)

$(INPUT_DIR)/uniform-circle-%: $(RAND_PTS)
	$< -s $(subst M,000000,$(*)) $@

$(INPUT_DIR)/rmat-%-symm: $(RMAT_GRAPH)
	$< -s 15210 -o -j $(subst M,000000,$(*)) $@

$(INPUT_DIR)/rmat-%-symm-bin: $(BIN_DIR)/graphio.mlton $(INPUT_DIR)/rmat-%-symm $(INPUT_DIR)
	$< $(word 2, $^) -outfile $@

$(INPUT_DIR)/words:
	curl -L -o $@ https://raw.githubusercontent.com/dwyl/english-words/refs/heads/master/words_alpha.txt

$(INPUT_DIR)/words-%: $(INPUT_DIR)/words
	# Evil shell magic:
	# 1. count the lines of the original words file
	# 2. multiply that by n in words-n
	# 3. shuffle with repetition that many times
	#
	# You can technically create words-n for any n, not just 8 or 32
	shuf -n $$(( $$(wc -l < $<) * $(*) )) -o $@ --repeat $<

$(ALL_INPUTS): | $(INPUT_DIR)/

#=================binaries=================
DEFAULT_FLAGS ?= -default-type int64 -default-type word64
SMLC          ?= mlton

COMPILER_NAME := $(notdir $(SMLC))
TESTS_NAME    := $(sort $(basename $(notdir $(wildcard bench/*/*.mlb))))
BINARIES      := $(addprefix $(BIN_DIR)/,$(TESTS_NAME))

$(BIN_DIR)/%.$(COMPILER_NAME): bench/%/*.mlb $(BIN_DIR)/
	$(SMLC) -output $@ -mlb-path-var 'COMPAT $(COMPILER_NAME)' $(DEFAULT_FLAGS) $<

#=================default parameters for tests=================
primes_N            := 100000000
dense-matmul_N      := 1024
msort_N             := 20000000
suffix-array_N      := 1000000
palindrome_N        := 1000000
nqueens_N           := 13
linefit-opt_N       := 500000000
linearrec_N         := 200000000
bignum-add-opt_N    := 500000000
integrate-opt_N     := 500000000
sparse-mxv-opt_N    := 200000000
mcss-opt_N          := 500000000
ocaml-lu-decomp_N   := 1024
ocaml-binarytrees5_N := 19

dedup_W          := $(INPUT_DIR)/words-32
grep_W           := $(INPUT_DIR)/words-32
tokens_W         := $(INPUT_DIR)/words-32
msort-strings_W  := $(INPUT_DIR)/words-8

delaunay_C       := 1M
nearest-nbrs_C   := 1M
quickhull_C      := 20M

dedup_PRE        := --verbose --no-output
grep_PRE         := EE
tokens_PRE       := --verbose --no-output
bfs_PRE          := --no-dir-opt

NUMERICAL_TESTS  := primes dense-matmul msort suffix-array palindrome \
					nqueens linefit-opt linearrec bignum-add-opt integrate-opt \
					sparse-mxv-opt mcss-opt ocaml-lu-decomp ocaml-binarytrees5
WORDS_TESTS      := dedup grep tokens msort-strings
CIRC_TESTS       := delaunay nearest-nbrs quickhull
RMAT_TESTS       := bfs centrality low-d-decomp max-indep-set triangle-count wc-opt
MISC_TESTS       := tinykaboom reverb seam-carve range-tree raytracer ocaml-nbody-imm

ALL_TESTS := $(NUMERICAL_TESTS) $(WORDS_TESTS) $(CIRC_TESTS) $(RMAT_TESTS) $(MISC_TESTS)
$(ALL_TESTS): %: $(BIN_DIR)/%.$(COMPILER_NAME)

tests: $(ALL_TESTS)

$(NUMERICAL_TESTS):
ifdef N
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n $(N)
else
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n $($@_N)
endif

ifdef WORDS
$(WORDS_TESTS):
	$(BIN_DIR)/$@.$(COMPILER_NAME) $($@_PRE) $(WORDS)
else
$(WORDS_TESTS): $(%_W)
	$(BIN_DIR)/$@.$(COMPILER_NAME) $($@_PRE) $($@_W)
endif

$(CIRC_TESTS): $(INPUT_DIR)/uniform-circle-$(%_C)
	$(BIN_DIR)/$@.$(COMPILER_NAME) -input $<

$(RMAT_TESTS): $(INPUT_DIR)/rmat-10M-symm-bin
	$(BIN_DIR)/$@.$(COMPILER_NAME) $($@_PRE) $<

tinykaboom:
	$(BIN_DIR)/$@.$(COMPILER_NAME) -width 100 -height 100 -frames 10 -fps 1

reverb: 
	$(BIN_DIR)/$@.$(COMPILER_NAME) $(INPUT_DIR)/mangore-waltz.wav

seam-carve:
	$(BIN_DIR)/$@.$(COMPILER_NAME) $(INPUT_DIR)/pano.ppm -num-seams 100

range-tree:
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n 1000000 -q 1000000

raytracer:
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n 1000 -m 1000

game-of-life:
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n_times 100 -board_size 1024 

ocaml-nbody-imm:
	$(BIN_DIR)/$@.$(COMPILER_NAME) -n 500 -num_bodies 1024 

clean:
	rm -rf $(INPUT_DIR)
	rm -rf $(BIN_DIR)

deepclean: clean
	-$(MAKE) -C $(PBBS_DIR) clean 2>/dev/null || true
	rm -rf $(PBBS_DIR)

$(INPUT_DIR)/ $(BIN_DIR)/:
	mkdir -p $@
