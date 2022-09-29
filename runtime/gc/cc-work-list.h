#ifndef CC_WORK_LIST_H
#define CC_WORK_LIST_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_workList {
  struct HM_chunkList storage;
  HM_chunk currentChunk;
} * CC_workList;

typedef struct CC_workList_elem {
  objptr op;    // object front
  union data {
    struct normal {
      uint16_t objptrIdx;
    } normal;
    struct sequence {
      size_t cellIdx;
      uint16_t objptrIdx;
    } sequence;
    struct stack {
      pointer topCursor;
      unsigned int frameOffsetsIdx;
    } stack;
  } data;
} * CC_workList_elem;

#else

struct CC_workList;
typedef struct CC_workList * CC_workList;

struct CC_workList_elem;
typedef struct CC_workList_elem * CC_workList_elem;

struct CC_workList_range;
typedef struct CC_workList_range * CC_workList_range;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

bool CC_workList_isEmpty(GC_state s, CC_workList w);
void CC_workList_init(GC_state s, CC_workList w);
void CC_workList_free(GC_state s, CC_workList w);
void CC_workList_push(GC_state s, CC_workList w, objptr op);

/** Returns a single field of an object that still needs to be traced.
  * So, note that a single push can result in many pops.
  *
  * Returns NULL if work list is empty */
objptr* CC_workList_pop(GC_state s, CC_workList w);

void CC_workList_free(GC_state s, CC_workList w);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_WORK_LIST_H */
