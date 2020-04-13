/**
 * @file hierarchical-heap-collection.c
 *
 * @author Jatin Arora
 *
 * @brief
 * Implementation of the concurrent collection interface
 */


#include "concurrent-collection.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))


void CC_addToStack (ConcurrentPackage cp, pointer p) {
  assert(cp->rootList!=NULL);
  concurrent_stack_push(cp->rootList, (void*)p);
}

bool CC_isPointerMarked (pointer p) {
  return MARK_MASK & getHeader (p);
}

bool isInScope(HM_chunk chunk, HM_chunkList list) {
  // Check that this chunk belongs to this list.
  // I think we should path compress here because we will access the
  // levelHead fairly regularly during collection.
  if(HM_HH_getChunkList(HM_getLevelHeadPathCompress(chunk)) == list) {
    return true;
  }
  return false;
}

bool isChunkSaved(HM_chunk chunk, ConcurrentCollectArgs* args) {
  // Alternative implementation that would require a bool in the chunks
  // check if it's already added in the toSpace/saved
  // return (HM_isChunkMarked(chunk));
  return isInScope (chunk, args->repList);
}

// Mark the object uniquely identified by p
void markObj(pointer p) {
  GC_header* headerp = getHeaderp(p);
  GC_header header = *headerp;
  header ^= MARK_MASK;
  *headerp = header;
}


// void unmarkObjects (HM_chunkList repList) {
//  HM_chunk chunk = repList->firstChunk;

//    while (NULL != chunk) {

//      HM_unmarkChunk(chunk);
//      chunk = chunk->nextChunk;

// }

void linearUnmark(GC_state s, HM_chunkList repList) {
  HM_chunk chunk = repList->firstChunk;

  while(chunk!=NULL) {
    pointer q = HM_getChunkStart(chunk);
    while (q != HM_getChunkFrontier(chunk)) {
      if (CC_isPointerMarked(q))
        markObj(q); // mark/unmark is just xor
      q += sizeofObject(s, q);

    }
    chunk = chunk->nextChunk;
  }
}


void saveChunk(HM_chunk chunk, void* rawArgs) {

  ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;
  HM_unlinkChunk(args->origList, chunk);
  HM_appendChunk(args->repList, chunk);
  // HM_markChunk(chunk) = true;
}

// Takes in a pointer and adds the chunk that it points to
// in the toSpace, if not added already.
void forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs) {
  objptr op = *opp;
  pointer p = objptrToPointer (op, NULL);
  ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

  HM_chunk cand_chunk = HM_getChunkOf(p);

  bool chunkOrig  = isInScope(cand_chunk, args->origList);
  bool chunkSaved = isChunkSaved(cand_chunk, args);

  // save this chunk if not saved already
  if(chunkOrig && !chunkSaved) {
    saveChunk(cand_chunk, args->repList);
  }

  // forward the object if in scope and not already forwarded.
  if(chunkOrig || chunkSaved) {
    if(!CC_isPointerMarked(p)) {
      markObj(p);
      foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
              forwardPtrChunk, &args);
    }
  }
}

// Dual unmarking function -- can merge codes by adding another param to args
void unmarkObjects(GC_state s, objptr *opp, void * rawArgs) {
  objptr op = *opp;
  pointer p = objptrToPointer (op, NULL);
  ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

  HM_chunk cand_chunk = HM_getChunkOf(p);

  if(isInScope(cand_chunk, args->repList)) {
    if(CC_isPointerMarked(p)) {
      // marking is xor. taking xor twice is unmarking
      markObj(p);
      foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
              unmarkObjects, &args);
    }
  }
}

// TODO: This function will develop as the policy is added.
// It is to document all integrity concerns and assert them.
void ensureCallSanity(GC_state s, HM_HierarchicalHeap targetHH,
              ConcurrentPackage args) {
  assert(targetHH!=NULL);
  assert(args!=NULL);

  assert(args->snapLeft!=BOGUS_OBJPTR);
  assert(args->snapRight!=BOGUS_OBJPTR);

}

void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth) {
  // 1) ensure the depth is indeed a public depth
  // 2) it is not being collected somewhere already -- not done, race condition on isCollecting
  // 3) construct arguments to the function call
  // 4) ensure the children can't join back until the collector is done -- not done
  // 5) Races with fork -- The heap could be in process of forking.

  HM_HierarchicalHeap currentHeap = thread->hierarchicalHeap;
  // LOG(LM_HH_COLLECTION, LL_Log, "called func");

  // Copied the ifs from HM_HHC_collectLocal.
  if (NONE == s->controls->collectionType) {
    return;
  }

  if (s->wsQueueTop == BOGUS_OBJPTR || s->wsQueueBot == BOGUS_OBJPTR) {
    return;
  }

  if (thread->currentDepth == 0) {
    return;
  }

  if(HM_HH_getDepth(currentHeap) < thread->currentDepth) {
    return;
  }

  // uint32_t originalLocalScope = pollCurrentLocalScope(s);

  // if(originalLocalScope > thread->currentDepth) {
  //   return;
  // }

  // assert(originalLocalScope == thread->currentDepth);
  // uint32_t maxDepth = (originalLocalScope>0)?originalLocalScope - 1 : 0;
  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t shallowestPrivateLevel = UNPACK_IDX(topval);
  uint32_t maxDepth = (shallowestPrivateLevel>0)?(shallowestPrivateLevel-1):0;

  if(depth > maxDepth) {
    LOG(LM_HH_COLLECTION, LL_Log, "Level is not public, skipping");
    return;
  }

  while(HM_HH_getDepth(currentHeap) > depth) {
    currentHeap = currentHeap->nextAncestor;
  }

  ConcurrentPackage args = currentHeap->concurrentPack;
  assert(args!=NULL); // it has to be the case that the currentHeap is well-formed from (HM_HH_new)
  if(args->isCollecting
    || args->snapLeft == BOGUS_OBJPTR
    || args->snapRight == BOGUS_OBJPTR) {
    return;
  }

  assert(HM_HH_getDepth(currentHeap) == depth);

  CC_collectWithRoots(s, currentHeap, currentHeap->concurrentPack);

}

void CC_collectWithRoots(GC_state s, HM_HierarchicalHeap targetHH,
              ConcurrentPackage args) {

  ensureCallSanity(s, targetHH, args);


  struct HM_chunkList _repList;
  HM_chunkList repList = &(_repList);
  assert(args->repList == NULL);
  args->repList = &(_repList);

  HM_chunkList origList = HM_HH_getChunkList(targetHH);

  ConcurrentCollectArgs lists = {
    .origList = origList,
    .repList  = repList
  };

  concurrent_stack* workStack = args->rootList;

  // clearing extraneous additions from previous collection
  concurrent_stack_clear(workStack);


  args->isCollecting = true;

  pointer pl = objptrToPointer (args->snapLeft, NULL);
  markObj(pl);
  foreachObjptrInObject(s, pl, false, trueObjptrPredicate, NULL,
              forwardPtrChunk, &args);
  pointer pr = objptrToPointer (args->snapRight, NULL);
  markObj(pr);
  foreachObjptrInObject(s, pr, false, trueObjptrPredicate, NULL,
              forwardPtrChunk, &args);


  while(concurrent_stack_size(workStack) != 0) {
    objptr * q = concurrent_stack_pop(workStack);
    // assert(isObjPtr(*q));

    forwardPtrChunk(s, q, &lists);
    // callIfIsObjptr(s, forwardPtrChunk, ((objptr*)(q)), &args);
  }

// JATIN_NOTE:
//  Turn off collection as soon as tracing is complete. This might race with the write barrier
//  the WB may end up adding stuff to the stack, because it sees that the collection is infact
//  on. Might need to remove this race.
  args->isCollecting = false;

  // Free the chunks in the original list
  HM_appendChunkList(getFreeListSmall(s), origList);

  // for(auto q: INIT_ROOT_SET) {
  //  callIfIsObjptr(s, unmarkObjects, ((objptr*)(q)), &lists);
  // }
  linearUnmark(s, repList);

  // Update the original list
  origList->firstChunk = repList->firstChunk;
  origList->lastChunk = repList->lastChunk;
  origList->size = repList->size;

  args->repList = NULL;
}
#endif