/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */


#ifndef CC_LIST_H
#define CC_LIST_H

struct CC_concList;
typedef struct CC_concList * CC_concList;

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct CC_concList {
  HM_chunk firstChunk;
  HM_chunk lastChunk;
  pthread_mutex_t mutex;
};

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void CC_initConcList(CC_concList concList);
pointer CC_storeInConcListWithPurpose(CC_concList concList, void* p, size_t objSize, enum BlockPurpose purpose);

// void CC_foreachObjInList(CC_concList concList, size_t objSize, HM_foreachObjClosure f);
// void CC_foreachRemInConc(GC_state s, CC_concList concList, struct HM_foreachDownptrClosure* f);
void CC_popAsChunkList(CC_concList concList, HM_chunkList chunkList);

HM_chunk CC_getLastChunk (CC_concList concList);
void CC_freeChunksInConcListWithInfo(GC_state s, CC_concList concList, void *info, enum BlockPurpose purpose);
void CC_appendConcList(CC_concList concList1, CC_concList concList2);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_LIST_H */
