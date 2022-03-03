#ifndef CC_WORK_LIST_H
#define CC_WORK_LIST_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_workList {
  struct HM_chunkList storage;
  HM_chunk currentChunk;
} * CC_workList;

#else

struct CC_workList;
typedef struct CC_workList * CC_workList;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void CC_workList_init(GC_state s, CC_workList w);
void CC_workList_push(GC_state s, CC_workList w, objptr op);

// returns BOGUS_OBJPTR if empty
objptr CC_workList_pop(GC_state s, CC_workList w);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_WORK_LIST_H */
