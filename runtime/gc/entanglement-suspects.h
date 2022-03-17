#ifndef ES_SET_H
#define ES_SET_H

#if (defined(MLTON_GC_INTERNAL_FUNCS))

#define SUSPECT_MASK ((GC_header)0x40000000)
#define SUSPECT_SHIFT 30

typedef struct ES_clearArgs {
  HM_chunkList newList;
  uint32_t heapDepth;
  GC_thread thread;
} * ES_clearArgs;

bool ES_mark(__attribute__((unused)) GC_state s, objptr op);
void ES_unmark(GC_state s, objptr op);

void ES_add(GC_state s, HM_chunkList es, objptr op);

bool ES_contains(HM_chunkList es, objptr op);

HM_chunkList ES_append(GC_state s, HM_chunkList es1, HM_chunkList es2);

void ES_clear(GC_state s, HM_HierarchicalHeap hh);

void ES_move(HM_chunkList list1, HM_chunkList list2);

int ES_foreachSuspect(GC_state s, HM_chunkList storage, struct GC_foreachObjptrClosure * fObjptrClosure);

#endif
#endif