#ifndef ES_SET_H
#define ES_SET_H

#if (defined(MLTON_GC_INTERNAL_TYPES))

#define SUSPECT_MASK ((GC_header)0x40000000)
#define SUSPECT_SHIFT 30

void ES_add(GC_state s, HM_chunkList es, objptr op);

bool ES_contains(HM_chunkList es, objptr op);

HM_chunkList ES_append (GC_state s, HM_chunkList es1, HM_chunkList es2);

void ES_clear(GC_state s, HM_chunkList es);

#endif
#endif