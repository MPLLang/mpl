/**
 * @file concurrent-collection.c
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
#define ASSERT2 0

void CC_initStack(ConcurrentPackage cp) {
  CC_stack* temp  = (struct CC_stack*) malloc(sizeof(struct CC_stack));
  CC_stack_init(temp, 2);
  cp->rootList = temp;
}

void CC_addToStack (ConcurrentPackage cp, pointer p) {
  if(cp->rootList==NULL) {
    LOG(LM_HH_COLLECTION, LL_FORCE, "Concurrent Stack is not initialised\n");
    CC_initStack(cp);
  }
  CC_stack_push(cp->rootList, (void*)p);
}

void CC_clearMutationStack(ConcurrentPackage cp) {
  // assert(!cp->isCollecting);
  if(cp->rootList!=NULL) {
    CC_stack_clear(cp->rootList);
  }
}

bool CC_isPointerMarked (pointer p) {
  return ((MARK_MASK & getHeader (p)) == MARK_MASK);
}

bool isInScope(HM_chunk chunk, ConcurrentCollectArgs* args) {
  return chunk->tmpHeap == args->fromHead;
}

bool isChunkSaved(HM_chunk chunk, ConcurrentCollectArgs* args) {
  return chunk->tmpHeap==args->toHead;
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
      saveChunk(chunk, args);
    }
    op = getFwdPtr(p);
    p = objptrToPointer(op, NULL);
  }
  assert(sizeofObject(s, p) >=0);
  return p;
}

void markObj(pointer p) {
  GC_header* headerp = getHeaderp(p);
  GC_header header = *headerp;
  header ^= MARK_MASK;
  *headerp = header;
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

// This function is exactly the same as in chunk.c.
// The only difference is, it doesn't NULL the levelHead of the unlinking chunk.
void CC_HM_unlinkChunk(HM_chunkList list, HM_chunk chunk) {

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


void printObjPtrInScopeFunction(GC_state s, objptr* opp, void* rawArgs) {
  objptr op = *opp;
  assert(isObjptr(op));
  pointer p = objptrToPointer (op, NULL);
  if (isInScope(HM_getChunkOf(p), rawArgs)
     && !isChunkSaved(HM_getChunkOf(p), rawArgs)) {
    printf("%p \n", *opp);
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
  bool saved = saveNoForward(s, p, rawArgs);

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
  forwardPtrChunk(s, &src, rawArgs);
  // the runtime needs dst to be saved in case it is in the scope of collection.
  // can potentially remove the downPointer, but there are some race issues with the write Barrier
  // forwardPtrChunk(s, &dst, rawArgs);
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

  bool saved = saveNoForward(s, p, rawArgs);

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
  while(currentHeap!=NULL && HM_HH_getDepth (currentHeap) != depth) {
    currentHeap = currentHeap->nextAncestor;
  }

  if(currentHeap==NULL)
    return NULL;
  assert(HM_HH_getDepth(currentHeap) == depth);
  // if(HM_HH_getDepth(currentHeap) < depth) {
    // LOG(LM_HH_COLLECTION, LL_FORCE, "no heap at this depth for the thread");
    // return NULL;
  // }

  ConcurrentPackage cp = currentHeap->concurrentPack;
  if(cp == NULL
    // || cp->isCollecting
    || cp->ccstate != CC_REG
    || cp->snapLeft == BOGUS_OBJPTR
    || cp->snapRight == BOGUS_OBJPTR
    || (cp->stack == BOGUS_OBJPTR && depth ==1 )
    // || cp->shouldCollect == false
    ) {
    return NULL;
  }
  else if (casCC (&(cp->ccstate), CC_REG, CC_COLLECTING) != CC_REG) {
    printf("\t %s\n", "returning because someone else claimed collection");
    return NULL;
  }
  // else if(casCC(&(cp->isCollecting), false, true)) {
  //   printf("\t %s\n", "returning because someone else claimed collection");
  //   assert(0);
  //   return NULL;
  // }
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
    // assert(currentHeap->concurrentPack->isCollecting);
    assert(currentHeap->concurrentPack->ccstate == CC_COLLECTING);
    assert(HM_HH_getDepth(currentHeap) == depth);
    return currentHeap;
  }
  // This point is reachable only after the fork is completed.
  assert(HM_HH_getDepth(currentHeap) == depth);

  return currentHeap;
}

void CC_collectAtRoot(pointer threadp, pointer hhp) {
  GC_state s = pthread_getspecific (gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  HM_HierarchicalHeap hh = (HM_HierarchicalHeap)hhp;


  HM_HierarchicalHeap currentHeap = thread->hierarchicalHeap;

  if (!checkLocalScheduler(s) || thread->currentDepth<=0) {
    return;
  }

  int depth = 1;
  HM_HierarchicalHeap heap = claimHeap(thread, depth);
  if (heap == NULL) {
    return;
  }

  if(heap!= hh) {
    DIE("heap != hh, the bug is caused by race with LC");
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

  // assert(heap->concurrentPack->shouldCollect);
  CC_collectWithRoots(s, heap, thread);
  // heap->concurrentPack->shouldCollect = false;
  // heap->concurrentPack->isCollecting = false;
  heap->concurrentPack->ccstate = CC_UNREG;
  s->amInCC = false;
  // printf("child completed collection\n");
  // exit(1);
}

// Function called for internal CC
void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth) {
  // 1) ensure the depth is indeed a public depth
  // 2) it is not being collected somewhere already
  // 3) construct arguments to CC_collectWithRoots
  checkLocalScheduler(s);

  if (thread->currentDepth == 0
    || depth <= 0
    || thread->currentDepth < depth) {
    return;
  }

  // (1)
  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t shallowestPrivateLevel = UNPACK_IDX(topval);
  uint32_t maxDepth = (shallowestPrivateLevel>0)?(shallowestPrivateLevel-1):0;
  if(depth > maxDepth) {
    return;
  }
  else if (depth >= thread->currentDepth) {
    return;
  }

  // (2, 3)
  HM_HierarchicalHeap heap = claimHeap(thread, depth);
  if(heap == NULL){
    return;
  }

  if (HM_getChunkListSize(&(heap->chunkList)) >= 2 * HM_BLOCK_SIZE) {
    // just a quick check to see if it is worth it to collect.
    assert(getThreadCurrent(s) == thread);
    CC_collectWithRoots(s, heap, thread);
  }

  // Mark that collection is complete
  heap->concurrentPack->ccstate = CC_UNREG;
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

  HM_foreachRemembered(s, y, bucketIfValidAtList, (void*)x);
  HM_appendChunkList(getFreeListSmall(s), y);
  *y = *x;
}

void printChunkListSize(HM_chunkList list) {
  printf("sizes: ");
  for(HM_chunk chunk = list->firstChunk; chunk!=NULL; chunk = chunk->nextChunk) {
    printf("%d ", HM_getChunkSize(chunk));
  }
  printf("\n");
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
  // origList are added to the free list.

  bool isConcurrent = (HM_HH_getDepth(targetHH) == 1);

  struct HM_chunkList _repList;
  HM_chunkList repList = &(_repList);
  HM_initChunkList(repList);
  HM_chunkList origList = (isConcurrent)?HM_HH_getFromList(targetHH):HM_HH_getChunkList(targetHH);

  HM_assertChunkListInvariants(origList);

  ConcurrentCollectArgs lists = {
    .origList = origList,
    .repList  = repList,
    .toHead = (void*)repList,
    .fromHead = (void*) &(origList)
  };

  // JATIN_NOTE: Some HM_hierarchical objects in origList
  // might not be reachable from the mutator roots.
  // So, I assign the levelHead of all chunks in the list to targetHH.
  // Then I explicitly preserve the chunk that contains targetHH.
  for(HM_chunk T = HM_getChunkListFirstChunk(origList); T!=NULL; T = T->nextChunk) {
    #if ASSERT2
    // there were some issues with stack chunks
    // preserving for future debugging
      HM_chunk stackChunk =  HM_getChunkOf(thread->stack);
      if (T!=stackChunk && HM_getLevelHead(T) != targetHH) {
        printf("%s\n", "this is failing");
        assert(0);
      }
    #endif
    assert(T->tmpHeap == NULL);
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
  HM_foreachRemembered(s, &downPtrs, forwardDownPtrChunk, &lists);
  // forward closures, stack and deque?
  forceForward(s, &(cp->snapLeft), &lists);
  forceForward(s, &(cp->snapRight), &lists);
  forceForward(s, &(cp->snapTemp), &lists);
  forceForward(s, &(s->wsQueue), &lists);
  forceForward(s, &(cp->stack), &lists);

  // JATIN_NOTE: This is important because the stack object of the thread we are collecting
  // often changes the level it is at. So it might in fact be at depth = 1.
  // It is important that we only mark the stack and not scan it.
  // Scanning the stack races with the thread using it.
  saveNoForward(s, thread->stack, &lists);
  saveNoForward(s, thread, &lists);
  forEachObjptrinStack(s, cp->rootList, forwardPtrChunk, &lists);

  #if ASSERT
  if (HM_HH_getDepth(targetHH) != 1){
    foreachObjptrInObject(s, thread->stack, false, trueObjptrPredicate, NULL,
          printObjPtrInScopeFunction, &lists);
  }
  #endif

  HM_foreachRemembered(s, &downPtrs, unmarkDownPtrChunk, &lists);
  forceUnmark(s, &(cp->snapLeft), &lists);
  forceUnmark(s, &(cp->snapRight), &lists);
  forceUnmark(s, &(cp->snapTemp), &lists);
  forceUnmark(s, &(s->wsQueue), &lists);
  forceUnmark(s, &(cp->stack), &lists);
  forEachObjptrinStack(s, cp->rootList, unmarkPtrChunk, &lists);

  #if ASSERT2 // just contains code that is sometimes useful for debugging.
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
  printf("%s %d \n", "Chunks Collected = ", lenFree);

  printf("%s", "collected chunks: ");
  for(HM_chunk chunk = origList->firstChunk; chunk!=NULL; chunk = chunk->nextChunk){
    printf("%p", chunk);
    assert(chunk->tmpHeap == lists.fromHead);
    assert(HM_getLevelHeadPathCompress(chunk) == targetHH);
  }
  printf("\n");
  // printf("Chunk list collected = %p \n", origList);
  #endif

  #if ASSERT2
    int countStopGapChunks = 0;
    int stopGapMem = 0;
    HM_chunk chunk = origList->firstChunk;
    while (chunk!=NULL)  {
      HM_chunk tChunk = chunk->nextChunk;
      assert(chunk->tmpHeap == lists.fromHead);
      if(chunk->startGap != 0) {
        saveChunk(chunk, &lists);
        countStopGapChunks++;
        stopGapMem+=(HM_getChunkSize(chunk));
      }
      chunk=tChunk;
    }
  #endif

  uint64_t bytesSaved =  HM_getChunkListSize(repList);
  uint64_t bytesScanned =  HM_getChunkListSize(repList)
                          + HM_getChunkListSize(origList);

  cp->bytesSurvivedLastCollection = bytesSaved;
  cp->bytesAllocatedSinceLastCollection = 0;

  struct HM_chunkList _deleteList;
  HM_chunkList deleteList = &(_deleteList);
  HM_initChunkList(deleteList);

  HM_chunk chunk = HM_getChunkListFirstChunk(origList);
  while (chunk!=NULL) {
    HM_chunk tChunk = chunk->nextChunk;
    chunk->levelHead = NULL;
    chunk->tmpHeap  = NULL;
    if(HM_getChunkSize(chunk) > 2 * (HM_BLOCK_SIZE)) {
      HM_unlinkChunk(origList, chunk);
      HM_appendChunk(deleteList, chunk);
    }

    chunk = tChunk;
  }

  HM_appendChunkList(getFreeListSmall(s), origList);
  HM_deleteChunks(s, deleteList);

  for(HM_chunk chunk = repList->firstChunk;
    chunk!=NULL; chunk = chunk->nextChunk) {
    chunk->tmpHeap = NULL;
  }

  *(origList) = *(repList);

  HM_assertChunkListInvariants(origList);

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);
  size_t msTotal =
    (size_t)stopTime.tv_sec * 1000 + (size_t)stopTime.tv_nsec / 1000000;

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