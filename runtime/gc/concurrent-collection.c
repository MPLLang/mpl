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
#define casCC(F, O, N) ((__sync_val_compare_and_swap(F, O, N)))
bool forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs);
void saveChunk(HM_chunk chunk, ConcurrentCollectArgs* args);
// ASSERT2 is for saving code in case I need it later. It is not for debugging purposes since
// the code is not read only and may change state.
#define ASSERT2 0

void CC_addToStack (ConcurrentPackage cp, pointer p) {
  if(cp->rootList==NULL) {
    // LOG(LM_HH_COLLECTION, LL_FORCE, "Concurrent Stack is not initialised\n");
    // assert(0);
    cp->rootList = (struct CC_stack*) malloc(sizeof(struct CC_stack));
    CC_stack_init(cp->rootList, 2);
  }
  // printf("%s\n", "trying to add to stack");
  CC_stack_push(cp->rootList, (void*)p);
}

void CC_clearMutationStack(ConcurrentPackage cp) {
  assert(!cp->isCollecting);
  if(cp->rootList!=NULL) {
    CC_stack_clear(cp->rootList);
  }
}

bool CC_isPointerMarked (pointer p) {
  return ((MARK_MASK & getHeader (p)) == MARK_MASK);
}

bool isInScope(HM_chunk chunk, ConcurrentCollectArgs* args) {
  // Check that this chunk belongs to this list.
  // I think we should path compress here because we will access the
  // levelHead fairly regularly during collection.
  return chunk->tmpHeap == args->fromHead;
/*  if(HM_HH_getChunkList(HM_getLevelHeadPathCompress(chunk)) == list) {
    return true;
  }
  return false;
*/
}

bool isChunkSaved(HM_chunk chunk, ConcurrentCollectArgs* args) {
  // Alternative implementation that would require a bool in the chunks
  // check if it's already added in the toSpace/saved
  // return (HM_isChunkMarked(chunk));
  // return chunk->levelHead == args->toHead;
  return (chunk->tmpHeap==args->toHead);
}


// JATIN_NOTE: this function should be called only for in scope objects.
// for out of scope objects the assertion and sanctity of *p is uncertain
// This is because of local collection. In general, CC assumes that the access to chunk level info
// about p is safe since it does not depend on (*p). Once it is established that p is in scope,
// we can call getTransitivePtr on it. Only after the transitive p is reached can we begin to inspect
// (*p). Otherwise, all bets are off because of races
pointer getTransitivePtr(GC_state s, pointer p, ConcurrentCollectArgs* args) {
  objptr op;

  assert(isInScope(HM_getChunkOf(p), args) ||
          isChunkSaved(HM_getChunkOf(p), args));
  while (hasFwdPtr(p)) {
    HM_chunk chunk = HM_getChunkOf(p);
    if(chunk->tmpHeap == args->fromHead){
      // they need to be saved because of LC issues. See issue 6 in README of
      // e9d8e56e4ad69631c17c11f1bf27370b1db0cfdc for more detail.
      saveChunk(chunk, args);
    }
    op = getFwdPtr(p);
    p = objptrToPointer(op, NULL);
    // printf("%p %d", p, HM_getLevelHead(chunk)->depth);
  }
  assert(sizeofObject(s, p) >=0);
  return p;
}


// Mark the object uniquely identified by p
void markObj(pointer p) {
  GC_header* headerp = getHeaderp(p);
  GC_header header = *headerp;
  header ^= MARK_MASK;
  *headerp = header;
  /* GC_header header_old = *headerp;
  GC_header header_new = header_old ^ MARK_MASK;
  GC_header q = casCC(headerp, header_old, header_new);
  assert(q==header_old);*/ // commented out cas implementation
}

#if ASSERT2
void linearTraverseChunkList (GC_state s, HM_chunkList list, HM_chunk skipChunk) {
  HM_assertChunkListInvariants(list);
  HM_chunk chunk = HM_getChunkListFirstChunk (list);

  while(chunk!=NULL) {
    if(chunk==skipChunk) {
      chunk= chunk->nextChunk;
      continue;
    }

    pointer p = HM_getChunkStart(chunk);
    chunk->tmpHeap = NULL;

    assert(chunk->frontier <= chunk->limit);
    while(p != chunk->frontier){
      assert(p < chunk->frontier);
      p = advanceToObjectData(s, p);
      p += sizeofObjectNoMetaData(s, p);
    }

    assert(chunk->frontier <= chunk->limit);
    chunk = chunk->nextChunk;
  }
  HM_assertChunkListInvariants(list);
}

bool isInExtraChunk(objptr q, HM_HierarchicalHeap hh) {
  return (HM_getChunkOf(q) == (hh->chunkList).firstChunk);
}
#else
void linearTraverseChunkList (GC_state s, HM_chunkList list, HM_chunk skipChunk) {}
bool isInExtraChunk(objptr q, HM_HierarchicalHeap hh) {return true;}
#endif

void linearUnmarkChunkList(GC_state s, ConcurrentCollectArgs* args,
                            HM_HierarchicalHeap hh) {
  #if ASSERT2
  HM_assertChunkListInvariants(args->repList);
  HM_chunk chunk = HM_getChunkListFirstChunk (args->repList);
  while(chunk!=NULL) {
    pointer p = HM_getChunkStart(chunk);
    chunk->tmpHeap = NULL;
    // chunk->levelHead = hh;
/*

    assert(chunk->frontier <= chunk->limit);
    while(p != chunk->frontier){
      assert(p < chunk->frontier);
      p = advanceToObjectData(s, p);

      if (CC_isPointerMarked(p)) {
        markObj(p); // mark/unmark is just xor
        assert(0);
      }
      p += sizeofObjectNoMetaData(s, p);
    }
    assert(chunk->frontier <= chunk->limit);
*/
    chunk = chunk->nextChunk;
  }
  HM_assertChunkListInvariants(args->repList);
  #endif
}

// #if ASSERT2
// bool isChunkInList(HM_chunkList chunkList, HM_chunk T) {
//   HM_chunk chunk = chunkList->firstChunk;
//   while (chunk != NULL) {
//     if(chunk == T) {
//       return true;
//     }
//     chunk = chunk->nextChunk;
//   }
//   return false;
// }
// #else
// void isChunkInList(HM_chunkList chunkList, HM_chunk chunk) {
//   return true;
// }
// #endif /* ASSERT2 */

// This function is exactly the same as in chunk.c.
// The only difference is, it doesn't NULL the levelHead of the unlinking chunk.
void CC_HM_unlinkChunk(HM_chunkList list, HM_chunk chunk) {
  #if ASSERT2
    assert(isChunkInList(list, chunk));
  #endif

  if (NULL == chunk->prevChunk) {
    assert(list->firstChunk == chunk);
    list->firstChunk = chunk->nextChunk;
  } else {
    assert(list->firstChunk != chunk);
    chunk->prevChunk->nextChunk = chunk->nextChunk;
  }

  if (NULL == chunk->nextChunk) {
    assert(list->lastChunk == chunk);
    list->lastChunk = chunk->prevChunk;
  } else {
    assert(list->lastChunk != chunk);
    chunk->nextChunk->prevChunk = chunk->prevChunk;
  }

  list->size -= HM_getChunkSize(chunk);

  chunk->prevChunk = NULL;
  chunk->nextChunk = NULL;

  #if ASSERT2
  HM_assertChunkListInvariants(list);
  // assert(!isChunkInList(list, chunk));
  #endif

}

void saveChunk(HM_chunk chunk, ConcurrentCollectArgs* args) {
  CC_HM_unlinkChunk(args->origList, chunk);
  HM_appendChunk(args->repList, chunk);

  assert(chunk->tmpHeap == args->fromHead);
  chunk->tmpHeap = args->toHead;

  HM_assertChunkListInvariants(args->origList);
  HM_assertChunkListInvariants(args->repList);
}

bool saveNoForward(GC_state s, pointer p, void* rawArgs) {
  ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

  HM_chunk cand_chunk = HM_getChunkOf(p);
  bool chunkSaved = isChunkSaved(cand_chunk, args);
  bool chunkOrig  = (chunkSaved)?true:isInScope(cand_chunk, args);

  if(chunkOrig && !chunkSaved) {
    assert(getTransitivePtr(s, p, rawArgs) == p);
    #if ASSERT2
      assert(cand_chunk->tmpHeap == args->fromHead);
      assert(isChunkInList(args->origList, cand_chunk));
    #endif
    saveChunk(cand_chunk, args);
  }
  return (chunkSaved || chunkOrig);
}

void markAndScan(GC_state s, pointer p, void* rawArgs) {
  if(!CC_isPointerMarked(p)) {
    markObj(p);
    assert(CC_isPointerMarked(p));
    foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
            forwardPtrChunk, rawArgs);
  }
}


void printObjPtrFunction(GC_state s, objptr* opp, void* rawArgs) {
  printf("\t %p", *opp);
}

void printDownPtrs (GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs) {
  printf("(%p, %p, %p) ", dst, field, src);
}

bool isChunkInList(HM_chunk chunk, HM_chunkList list) {
  for(HM_chunk c = list->firstChunk; c!=NULL; c=c->nextChunk) {
    if(c == chunk)
      return true;
  }
  return false;
}

bool forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs) {
  objptr op = *opp;
  assert(isObjptr(op));
  pointer p = objptrToPointer (op, NULL);
  if (isInScope(HM_getChunkOf(p), rawArgs)
     || isChunkSaved(HM_getChunkOf(p), rawArgs)) {
    p = getTransitivePtr(s, p, rawArgs);
  }
  // JATIN_NOTE: Optimisation, to be enabled later.
  // if(args->rootList->firstChunk == NULL) {
  //   assert(args->rootList->lastChunk == NULL);
  //   // The collection saved everything. No need to scan further.
  //   return true;
  // }

  bool saved = saveNoForward(s, p, rawArgs);

  // forward the object, if in repList and not forwarded already.
  if(saved) {
    #if ASSERT2
      // ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;
      // if(!isChunkInList (args->repList, HM_getChunkOf(p))) {
      //   printf("%s\n", "this is failing\n");
      //   assert(0);
      // }
    #endif
    // p = getTransitivePtr(s, p, rawArgs);
    markAndScan(s, p, rawArgs);
  }
  return saved;
}

void forwardDownPtrChunk(GC_state s, __attribute__((unused)) objptr dst,
                          __attribute__((unused)) objptr* field,
                          objptr src, void* rawArgs) {
  #if ASSERT
  ConcurrentCollectArgs* args =  (ConcurrentCollectArgs*) rawArgs;
  HM_chunk T = HM_getChunkOf((pointer)dst);
  bool inScope = false;
  // if (isInScope(T, args) || isChunkSaved(T, args)) {
  //   printDownPtrs(s, dst, field, src, rawArgs);
  //   inScope = true;
  // }
  // pointer p = objptrToPointer(src, NULL);
  #endif


  forwardPtrChunk(s, &src, rawArgs);
  // the runtime needs dst to be saved in case it is in the scope of collection.
  // can potentially remove the downPointer, but there are some race issues with the write Barrier
  // forwardPtrChunk(s, &dst, rawArgs);
  #if ASSERT
  if(inScope) {
    // assert(isChunkSaved(HM_getChunkOf, args));
  }
  #endif
}

void unmarkPtrChunk(GC_state s, objptr* opp, void* rawArgs) {
  objptr op = *opp;
  assert(isObjptr(op));

  pointer p = objptrToPointer (op, NULL);
  HM_chunk chunk = HM_getChunkOf(p);

  // check that getTransitivePtr can be called.
  if (!isChunkSaved(chunk, rawArgs)) {
    return;
  }
  p = getTransitivePtr(s, p, rawArgs);

  if(CC_isPointerMarked (p)) {
    assert(chunk->tmpHeap == ((ConcurrentCollectArgs*)rawArgs)->toHead);
    #if ASSERT2
    GC_header  header = getHeader (p);
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    GC_objectTypeTag tag;
    // printf("%p %p\n", header, p);
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
    #endif
    markObj(p);

    assert(!CC_isPointerMarked(p));
    foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
                  unmarkPtrChunk, rawArgs);
  }
}

void unmarkDownPtrChunk (GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs) {
  unmarkPtrChunk(s, &src, rawArgs);
  unmarkPtrChunk(s, &dst, rawArgs);

}

// this function does more than forwardPtrChunk and is safer. It forwards even if not in scope.
// Recursively however it only calls forwardPtrChunk and not itself
void forceForward(GC_state s, objptr *opp, void* rawArgs) {
  pointer p = objptrToPointer(*opp, NULL);

  // forceForward is called manually. So those things should not have forwarding pointers installed
  bool saved = saveNoForward(s, p, rawArgs);

  // Inlined markAndScan here since we don't want to mark if not in scope
  if(saved && !CC_isPointerMarked(p)) {
    assert(getTransitivePtr(s, p, rawArgs) == p);
    markObj(p);
  }
  foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
          forwardPtrChunk, rawArgs);
}

void forceUnmark (GC_state s, objptr* opp, void* rawArgs) {
  pointer p = objptrToPointer(*opp, NULL);
  if(CC_isPointerMarked(p)){
    assert(getTransitivePtr(s, p, rawArgs) == p);
    markObj(p);
  }
  foreachObjptrInObject(s, p, false, trueObjptrPredicate, NULL,
                unmarkPtrChunk, rawArgs);
}

// TODO: This function will develop as the policy is added.
// It is to document all integrity concerns and assert them.
void ensureCallSanity(__attribute__((unused)) GC_state s,
                      HM_HierarchicalHeap targetHH,
                      ConcurrentPackage args) {
  assert(targetHH!=NULL);
  assert(args!=NULL);
  assert(args->snapLeft!=BOGUS_OBJPTR);
  assert(args->snapRight!=BOGUS_OBJPTR);

}


bool checkLocalScheduler (GC_state s) {
  return !( NONE == s->controls->collectionType ||
           s->wsQueueTop == BOGUS_OBJPTR ||
           s->wsQueueBot == BOGUS_OBJPTR
         );
}

HM_HierarchicalHeap claimHeap (GC_thread thread, int depth) {
  HM_HierarchicalHeap currentHeap = thread->hierarchicalHeap;
  while(HM_HH_getDepth(currentHeap) > depth) {
    currentHeap = currentHeap->nextAncestor;
  }

  if(HM_HH_getDepth(currentHeap) < depth) {
    // LOG(LM_HH_COLLECTION, LL_FORCE, "no heap at this depth for the thread");
    return NULL;
  }

  ConcurrentPackage cp = currentHeap->concurrentPack;

  if(cp == NULL
    || cp->isCollecting
    || cp->snapLeft == BOGUS_OBJPTR
    || cp->snapRight == BOGUS_OBJPTR
    || (cp->stack == BOGUS_OBJPTR && depth ==1 )
    || cp->shouldCollect == false) {
    return NULL;
  }
  else if(casCC(&(cp->isCollecting), false, true)) {
    printf("\t %s\n", "returning because someone else claimed collection");
    assert(0);
    return NULL;
  }
  else {
    // LOG(LM_HH_COLLECTION, LL_FORCE, "\t collection turned on isCollect = & depth = ");
    // printf("%d %d\n", cp->isCollecting, depth);
    // #if ASSERT2
    //   while(oldStart!=currentHeap) {
    //     printf(" %p => ", (void*) oldStart);
    //     oldStart = oldStart->nextAncestor;
    //   }
    //   printf("FIN: %p\n", currentHeap);
    // #endif
    assert(currentHeap->concurrentPack->isCollecting);
    assert(HM_HH_getDepth(currentHeap) == depth);
    return currentHeap;
  }

  // This point is reachable only after the fork is completed.
  assert(HM_HH_getDepth(currentHeap) == depth);

  return currentHeap;
}

void CC_collectAtRoot(GC_thread thread) {
  // return;
  GC_state s = pthread_getspecific (gcstate_key);
  HM_HierarchicalHeap currentHeap = thread->hierarchicalHeap;
  int depth = 1;

  if (!checkLocalScheduler(s)) {
    return;
  }

  assert(thread->currentDepth!=0);

  HM_HierarchicalHeap heap = claimHeap(thread, depth);
  if (heap == NULL) {
    return;
  }

  assert(!s->amInCC);
  s->amInCC = true;

  // pid_t childPid = fork();
  // if(childPid >= 0) {
  //   if(childPid == 0) {
  //     printf("child created \n");
  //   }
  //   else {
  //     printf("continuing parent\n");
  //     s->amInCC = false;
  //     return;
  //   }
  // }


  CC_collectWithRoots(s, heap, thread);
  heap->concurrentPack->isCollecting = false;
  heap->concurrentPack->shouldCollect = false;
  s->amInCC = false;
  // printf("child completed collection\n");
  // exit(1);
}

void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth) {
  // 1) ensure the depth is indeed a public depth
  // 2) it is not being collected somewhere already
  // 3) construct arguments to the function call
  // 4) ensure the children can't join back until the collector is done -- done by construction
  // 5) Races with fork -- does not matter if the force left heap has been done.
  // bool gdbArg = false;
  // if (gdbArg) {
  //   depth = (depth>0? depth -1: 0);
  // }
  // return;
  // depth = (depth==1)?(depth+1):depth;

  // LOG(LM_HH_COLLECTION, LL_Log, "called func");

  checkLocalScheduler(s);

  if (thread->currentDepth == 0
    || depth <= 0
    || thread->currentDepth < depth) {
    return;
  }

  // uint32_t originalLocalScope = pollCurrentLocalScope(s);
  // if(originalLocalScope > thread->currentDepth) {
  //   return;
  // }
  // assert(originalLocalScope == thread->currentDepth);
  // uint32_t maxDepth = (originalLocalScope>0)?originalLocalScope - 1 : 0;

  // ensure collection is at a public level (1)
  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t shallowestPrivateLevel = UNPACK_IDX(topval);
  uint32_t maxDepth = (shallowestPrivateLevel>0)?(shallowestPrivateLevel-1):0;
  if(depth > maxDepth) {
    return;
    // if(depth == thread->currentDepth)
      // return;
  }
  else if (depth >= thread->currentDepth) {
    return;
  }

  #if ASSERT2
    HM_HierarchicalHeap oldStart = thread->hierarchicalHeap;
  #endif

  // checks (2, 3)
  HM_HierarchicalHeap heap = claimHeap(thread, depth);
  if(heap == NULL){
    return;
  }
  else if (HM_getChunkListSize(&(heap->chunkList)) <= 2 * HM_BLOCK_SIZE) {
    heap->concurrentPack->isCollecting = false;
    heap->concurrentPack->shouldCollect = false;
    return;
  }

  assert(s->currentThread == thread);
  // printf("collecting seq : %p\n", heap);
  assert(heap->concurrentPack->shouldCollect);
  CC_collectWithRoots(s, heap, thread);
  // printf("turning off for %p \n", heap);
  heap->concurrentPack->shouldCollect = false;
  // printf("turned off for %p \n", heap);
  heap->concurrentPack->isCollecting = false;
}



void CC_filterDownPointers(GC_state s, HM_chunkList x, HM_HierarchicalHeap hh){
  // Since the root collection is truly concurrent the depth 1 remSet is separated at the time of fork.
  HM_chunkList y;
  if (hh->depth == 1) {
    y = &(hh->concurrentPack->remSet);
  }
  else {
    y = HM_HH_getRemSet(hh);
  }

  HM_foreachRemembered(s, y, bucketIfValid, (void*)x);
  HM_appendChunkList(getFreeListSmall(s), y);
  *y = *x;
}


void CC_collectWithRoots(GC_state s, HM_HierarchicalHeap targetHH,
                         GC_thread thread) {
  struct timespec startTime;
  struct timespec stopTime;

  timespec_now(&startTime);

  ConcurrentPackage cp = targetHH->concurrentPack;
  ensureCallSanity(s, targetHH, cp);
  // At the end of collection, repList will contain all the chunks that have
  // some object that is reachable from the roots. origList will contain the
  // chunks in which all objects are garbage. Before exiting, chunks in
  // origList are added to the free list and origList is made equal to repList

// depth 1 heap is collected completely concurrently and does not wait for a join
  bool isConcurrent = (HM_HH_getDepth(targetHH) == 1);

  struct HM_chunkList _repList;
  HM_chunkList repList = &(_repList);
  HM_initChunkList(repList);
  HM_chunkList origList = (isConcurrent)?HM_HH_getFromList(targetHH):HM_HH_getChunkList(targetHH);
  HM_assertChunkListInvariants(origList);

  #if ASSERT2
  int lenOrig = 0;
  HM_chunk T = origList->firstChunk;
  while(T!=NULL) {
    lenOrig++;
    T = T->nextChunk;
  }
  #endif
  HM_chunk stackChunk =  HM_getChunkOf(thread->stack);

  // linearTraverseChunkList(s, origList, stackChunk);

  ConcurrentCollectArgs lists = {
    .origList = origList,
    .repList  = repList,
    .toHead = (void*)repList,
    .fromHead = (void*) &(origList)
  };

  for(HM_chunk T = HM_getChunkListFirstChunk(origList); T!=NULL; T = T->nextChunk) {
    #if ASSERT2
      if (T!=stackChunk && HM_getLevelHead(T) != targetHH) {
        printf("%s\n", "this is failing");
        assert(0);
      }
    #endif
    T->tmpHeap = lists.fromHead;
    T->levelHead = targetHH;
  }



  HM_chunk baseChunk = HM_getChunkOf(targetHH);
  if(isInScope(baseChunk, &lists)) {
    saveChunk(baseChunk, &lists);
  }
  else if (baseChunk == (targetHH->chunkList).firstChunk && isConcurrent) {}
  else {
    assert(0);
  }

  // forward down pointers
  struct HM_chunkList downPtrs;
  HM_initChunkList(&downPtrs);
  CC_filterDownPointers(s, &downPtrs, targetHH);
  // if(HM_HH_getDepth(targetHH) == 3) {
  //   printf("downPtrs: ");
  //   HM_foreachRemembered(s, &downPtrs, printDownPtrs, &lists);
  //   printf("\n");
  // }
  HM_foreachRemembered(s, &downPtrs, forwardDownPtrChunk, &lists);
  // forward closures and stack
  forceForward(s, &(cp->snapLeft), &lists);
  forceForward(s, &(cp->snapRight), &lists);

// if at depth 1, use the copied stack. Otherwise, use the current thread's stack.
  objptr stackp = (cp->stack);
  // objptr stackp = isConcurrent?(cp->stack):(getStackCurrentObjptr(s));
  forceForward(s, &(stackp), &lists);

  // JATIN_NOTE: This is important because the stack object of the thread we are collecting
  // often changes the level it is at. So it might in fact be at depth = 1. It is important that we
  // only mark the stack and not scan it. Forwarding the stack races with the thread using it, causing arbitrary behaviour
  // Not forwarding the stack is okay, since we've preserved the stack from before.
  saveNoForward(s, (pointer)(thread->stack), &lists);

  // forward reachability altering writes
  forEachObjptrinStack(s, cp->rootList, forwardPtrChunk, &lists);


  // unmark in the exact same order
  HM_foreachRemembered(s, &downPtrs, unmarkDownPtrChunk, &lists);
  forceUnmark(s, &(cp->snapLeft), &lists);
  forceUnmark(s, &(cp->snapRight), &lists);
  forceUnmark(s, &(stackp), &lists);
  forEachObjptrinStack(s, cp->rootList, unmarkPtrChunk, &lists);

  /*
  #if ASSERT2
   {
     HM_chunk soleChunk = HM_getChunkListFirstChunk(HM_HH_getChunkList(targetHH));
     assert(HM_getChunkListLastChunk(HM_HH_getChunkList(targetHH)) == soleChunk);
     assert(soleChunk->tmpHeap == NULL);
     pointer p =  HM_getChunkStart(soleChunk);
     assert(soleChunk->frontier <= soleChunk->limit);
     while(p != soleChunk->frontier){
       assert(0);
       assert(p < soleChunk->frontier);
       p = advanceToObjectData(s, p);
       objptr ob = pointerToObjptr(p, NULL);
       forceForward(s, &(ob), &lists);
       p += sizeofObjectNoMetaData(s, p);
     }
   }
   #endif

  forwardPtrChunk(s, &(s->currentThread), &lists);
  if(workStack!=NULL){
    size_t size = CC_stack_size(workStack);
    while(size!=0){
      void* q = CC_stack_pop(workStack);
      forwardPtrChunk(s, q, &lists);
      // callIfIsObjptr(s, forwardPtrChunk, ((objptr*)(q)), &args);
      q = CC_stack_pop(workStack);
      if(HM_getChunkListFirstChunk(origList)==NULL)
        break;
    }
  }
  */

  #if ASSERT2
  HM_assertChunkListInvariants(origList);
  HM_assertChunkListInvariants(repList);

  int lenFree = 0;
  int lenRep = 0;
  HM_chunk Q = origList->firstChunk;
  // assert(Q==NULL);
  while(Q!=NULL) {
    lenFree++;
    Q = Q->nextChunk;
  }

  Q = repList->firstChunk;
  while(Q!=NULL){
    lenRep++;
    assert(Q->tmpHeap == lists.toHead);
    assert(HM_getLevelHeadPathCompress(Q) == targetHH);
    Q = Q->nextChunk;
  }
  assert(lenRep+lenFree == lenOrig);
  // printf("%s %d \n", "Chunks Collected = ", lenFree);

  // printf("%s", "collected chunks: ");
  for(HM_chunk chunk = origList->firstChunk; chunk!=NULL; chunk = chunk->nextChunk){
    // printf("%p", chunk);
    assert(chunk->tmpHeap == lists.fromHead);
    assert(HM_getLevelHeadPathCompress(chunk) == targetHH);
  }
  // printf("\n");
  // printf("Chunk list collected = %p \n", origList);

  // JATIN_NOTE: This loop is not needed if tmpHeap is made NULL in HM_getFreeChunk.
  // Adding it here so that this runs in debug and the difference is evident if reasoning is flawed
  int countStopGapChunks = 0;
  int stopGapMem = 0;
  for(HM_chunk chunk = origList->firstChunk; chunk!=NULL; chunk = chunk->nextChunk){
    assert(chunk->tmpHeap == lists.fromHead);
    if(chunk->startGap != 0) {
      saveChunk(chunk, &lists);
      countStopGapChunks++;
      stopGapMem+=(HM_getChunkSize(chunk));
    }
    else {
      chunk->levelHead = NULL;
      chunk->tmpHeap  = NULL;
    }
  }
  #endif

  // HM_chunk Q = origList->firstChunk;
  // while (Q!=NULL) {
  //   pointer q = Q;
  //   Q = Q->nextChunk;
  //   saveChunk(q, &lists);
  // }

  float ratio;

  if(cp->bytesSurvivedLastCollection > 0){
    ratio = ((float)(cp->bytesAllocatedSinceLastCollection))
              /(cp->bytesSurvivedLastCollection);
  }
  else{
    ratio= 0;
  }
  uint64_t bytesSaved =  HM_getChunkListSize(repList);
  uint64_t bytesScanned =  HM_getChunkListSize(repList) + HM_getChunkListSize(origList);

  // printf("collected %s = %d %d and at %f\n", (isConcurrent? " ":"seq"), (bytesScanned-bytesSaved), bytesScanned, ratio);
  cp->bytesSurvivedLastCollection = HM_getChunkListSize(repList);
  cp->bytesAllocatedSinceLastCollection = 0;
  struct HM_chunkList _deleteList;
  HM_chunkList deleteList = &(_deleteList);
  HM_initChunkList(deleteList);

  // printf("sizes released: ");
  HM_chunk chunk = HM_getChunkListFirstChunk(origList);
  uint64_t bytesCollected = HM_getChunkListSize(origList);
  bytesCollected = bytesCollected>0?bytesCollected:0;
  bytesCollected/=2;
  uint64_t bytesFreed = 0;

  while (chunk!=NULL) {
    // if(HM_getChunkSize(chunk) > 2 * (HM_BLOCK_SIZE) || (bytesFreed < bytesCollected)) {
    if(HM_getChunkSize(chunk) > 2 * (HM_BLOCK_SIZE)) {
      pointer q = chunk;
      chunk = chunk->nextChunk;
      HM_unlinkChunk(origList, q);
      HM_appendChunk(deleteList, q);
      bytesFreed+= HM_getChunkSize(q);
      // printf("%d ", HM_getChunkSize(q));
      // GC_release (q, HM_getChunkSize(q));
    }
    else {
      chunk = chunk->nextChunk;
    }
  }
  // printf("\n");

  // HM_appendChunkList(getFreeListSmall(s), origList);

  // if not at root, no need to add to the shared list. It's like a local collection
  if (isConcurrent) {
    HM_appendChunkList(getFreeListSmall(s), origList);
    // HM_appendToSharedList(s, origList);
  }
  else {
    HM_appendChunkList(getFreeListSmall(s), origList);
  }
  HM_deleteChunks(s, deleteList);
  // linearUnmarkChunkList(s, &lists, targetHH);
  *(origList) = *(repList);
  // origList->firstChunk = HM_getChunkListFirstChunk(repList);
  // origList->lastChunk =  HM_getChunkListLastChunk(repList);
  // origList->size = HM_getChunkListSize(repList);
  HM_assertChunkListInvariants(origList);

  linearTraverseChunkList(s, origList, NULL);

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);
  size_t msTotal =
    (size_t)stopTime.tv_sec * 1000 + (size_t)stopTime.tv_nsec / 1000000;
  // printf("collection time: %zu ms\n", msTotal);

  if (isConcurrent) {
    timespec_add(&(s->cumulativeStatistics->timeRootCC), &stopTime);
    s->cumulativeStatistics->numRootCCs++;
    s->cumulativeStatistics->bytesReclaimedByRootCC += bytesScanned-bytesSaved;
  } else {
    timespec_add(&(s->cumulativeStatistics->timeInternalCC), &stopTime);
    s->cumulativeStatistics->numInternalCCs++;
    s->cumulativeStatistics->bytesReclaimedByInternalCC += bytesScanned-bytesSaved;
  }

}
#endif
