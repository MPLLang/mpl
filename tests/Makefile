PBBS_DIR?=pbbsbench
INPUT_DIR:=inputs

RAND_PTS        :=$(PBBS_DIR)/testData/geometryData/randPoints
RMAT_GRAPH      :=$(PBBS_DIR)/testData/graphData/rMatGraph

UC_INPUTS       := $(INPUT_DIR)/uniform-circle-1M $(INPUT_DIR)/uniform-circle-20M
RMAT_INPUTS     := $(INPUT_DIR)/rmat-1M-symm $(INPUT_DIR)/rmat-10M-symm
RMAT_BIN_INPUTS := $(patsubst %,%-bin,$(RMAT_INPUTS))
TEXT_INPUTS     := $(INPUT_DIR)/words-8 $(INPUT_DIR)/words-32
ALL_INPUTS      := $(UC_INPUTS) $(RMAT_INPUTS) $(RMAT_BIN_INPUTS) $(TEXT_INPUTS)

.PHONY: all clean deepclean

all: inputs

inputs: $(ALL_INPUTS)

$(PBBS_DIR):
	git clone https://github.com/cmuparlay/pbbsbench $@
	git -C $@ submodule update --init --recursive

$(RAND_PTS) $(RMAT_GRAPH): $(PBBS_DIR)
	$(MAKE) -C $(@D) $(@F)

bench/graphio.bin:
	$(MAKE) -C $(@D) SMLC=mlton $(@F)

$(INPUT_DIR)/uniform-circle-%: $(RAND_PTS)
	$< -s $(subst M,000000,$(*)) $@

$(INPUT_DIR)/rmat-%-symm: $(RMAT_GRAPH)
	$< -s 15210 -o -j $(subst M,000000,$(*)) $@

$(INPUT_DIR)/rmat-%-symm-bin: bench/graphio.bin $(INPUT_DIR)/rmat-%-symm 
	$< $(word 2, $^) -outfile $@

$(INPUT_DIR)/words:
	curl -L -o $@ https://raw.githubusercontent.com/dwyl/english-words/refs/heads/master/words_alpha.txt

$(INPUT_DIR)/words-%: $(INPUT_DIR)/words
	shuf -n $$(( $$(wc -l < $<) * $(*) )) -o $@ --repeat $<

$(INPUT_DIR):
	mkdir -p $@

clean:
	rm -rf $(INPUT_DIR)
	rm -f bench/graphio.bin

deepclean: clean
	-$(MAKE) -C $(PBBS_DIR) clean 2>/dev/null || true
	rm -rf $(PBBS_DIR)
