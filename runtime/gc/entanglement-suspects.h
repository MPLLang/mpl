#ifndef ES_SET_H
#define ES_SET_H

#if (defined(MLTON_GC_INTERNAL_FUNCS))

#define SUSPECT_MASK ((GC_header)0x40000000)
#define SUSPECT_SHIFT 30

typedef struct ES_clearArgs {
  HM_chunkList newList;
  uint32_t heapDepth;
  GC_thread thread;
  size_t numMoved;
  size_t numFailed;
  size_t numCleared;
} * ES_clearArgs;


typedef struct ES_clearSet {
  HM_chunk *chunkArray; // array of chunks that need to be processed
  size_t lenChunkArray; // len(chunkArray)
  uint32_t depth;
  size_t numSuspects;
  struct timespec startTime;
} * ES_clearSet;

typedef struct ES_finishedClearSetGrain {
  struct HM_chunkList *output; // output[d]: unsuccessful clears that were moved to depth d
  size_t lenOutput; // len(output array)
} * ES_finishedClearSetGrain;


bool ES_mark(__attribute__((unused)) GC_state s, objptr op);
void ES_unmark(GC_state s, objptr op, ES_clearArgs args);

void ES_add(GC_state s, HM_chunkList es, objptr op);

bool ES_contains(HM_chunkList es, objptr op);

HM_chunkList ES_append(GC_state s, HM_chunkList es1, HM_chunkList es2);

void ES_clear(GC_state s, HM_HierarchicalHeap hh);

// These functions allow us to clear a suspect set in parallel,
// by integrating with the scheduler. The idea is...
size_t ES_numSuspects(GC_state s, HM_HierarchicalHeap hh);
ES_clearSet ES_takeClearSet(GC_state s, HM_HierarchicalHeap hh);
size_t ES_numChunksInClearSet(GC_state s, ES_clearSet es);
ES_finishedClearSetGrain ES_processClearSetGrain(GC_state s, ES_clearSet es, size_t start, size_t stop);
void ES_commitFinishedClearSetGrain(GC_state s, GC_thread thread, ES_finishedClearSetGrain es);
void ES_deleteClearSet(GC_state s, ES_clearSet es);

void ES_move(HM_chunkList list1, HM_chunkList list2);

int ES_foreachSuspect(GC_state s, HM_chunkList storage, struct GC_foreachObjptrClosure * fObjptrClosure);

#endif
#endif
