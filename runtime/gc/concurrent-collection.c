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

void forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs);
void saveChunk(HM_chunk chunk, ConcurrentCollectArgs* args);
#define ASSERT2 0

void CC_initStack(ConcurrentPackage cp) {
  // don't re-initialize
  if(cp->rootList!=NULL) {
    return;
  }

  CC_stack* temp  = (struct CC_stack*) malloc(sizeof(struct CC_stack));
  CC_stack_init(temp, 2);
  cp->rootList = temp;
}

void CC_addToStack (ConcurrentPackage cp, pointer p) {
  if(cp->rootList==NULL) {
    // Its NULL because we won't collect this heap. so no need to snapshot
    return;
    // LOG(LM_HH_COLLECTION, LL_FORCE, "Concurrent Stack is not initialised\n");
    // CC_initStack(cp);
  }
  CC_stack_push(cp->rootList, (void*)p);
}

void CC_clearStack(ConcurrentPackage cp) {
  if(cp->rootList!=NULL) {
    CC_stack_clear(cp->rootList);
  }
}

void CC_freeStack(ConcurrentPackage cp) {
  if(cp->rootList!=NULL) {
    CC_stack_free(cp->rootList);
    free(cp->rootList);
    cp->rootList = NULL;
  }
}

bool CC_isPointerMarked (pointer p) {
  return ((MARK_MASK & getHeader (p)) == MARK_MASK);
}

/** Is it in the from space? */
bool isInScope(HM_chunk chunk, ConcurrentCollectArgs* args) {
  return chunk->tmpHeap == args->fromHead;
}

/** Is it in the to-space? */
bool isChunkSaved(HM_chunk chunk, ConcurrentCollectArgs* args) {
  return chunk->tmpHeap==args->toHead;
}


// JATIN_NOTE: this function should be called only for in scope objects.
// for out of scope objects the assertion and sanctity of *p is uncertain
// This is because of local collection. In general, CC assumes that the access to chunk level info
// about p is safe since it does not depend on (*p). Once it is established that p is in scope,
// we can call getTransitivePtr on it.
// Only after the transitive pointer is reached can we begin to inspect
// (*p). Otherwise, all bets are off because of races
pointer getTransitivePtr(pointer p, ConcurrentCollectArgs* args) {
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
  return p;
}

void markObj(pointer p) {
  GC_header* headerp = getHeaderp(p);
  GC_header header = *headerp;
  header ^= MARK_MASK;
  *headerp = header;
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
  list->usedSize -= HM_getChunkUsedSize(chunk);

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

bool saveNoForward(
  __attribute__((unused)) GC_state s,
  pointer p,
  void* rawArgs)
{
  ConcurrentCollectArgs* args = (ConcurrentCollectArgs*)rawArgs;

  HM_chunk cand_chunk = HM_getChunkOf(p);
  bool chunkSaved = isChunkSaved(cand_chunk, args);
  bool chunkOrig  = (chunkSaved)?TRUE:isInScope(cand_chunk, args);

  if(chunkOrig && !chunkSaved) {
    assert(getTransitivePtr(p, rawArgs) == p);
    saveChunk(cand_chunk, args);
  }
  return (chunkSaved || chunkOrig);
}

void markAndScan(GC_state s, pointer p, void* rawArgs) {
  if(!CC_isPointerMarked(p)) {
    markObj(p);
    ((ConcurrentCollectArgs*)rawArgs)->bytesSaved += sizeofObject(s, p);
    assert(CC_isPointerMarked(p));

    struct GC_foreachObjptrClosure forwardPtrClosure =
    {.fun = forwardPtrChunk, .env = rawArgs};

    foreachObjptrInObject(s, p, &trueObjptrPredicateClosure,
            &forwardPtrClosure, FALSE);
  }
}

// some debugging functions
void printObjPtrInScopeFunction(
  __attribute__((unused)) GC_state s,
  objptr* opp,
  void* rawArgs)
{
  objptr op = *opp;
  assert(isObjptr(op));
  pointer p = objptrToPointer (op, NULL);
  if (isInScope(HM_getChunkOf(p), rawArgs)
     && !isChunkSaved(HM_getChunkOf(p), rawArgs)) {
    printf("%p \n", (void *) *opp);
  }
}

void printObjPtrFunction(
  __attribute__((unused)) GC_state s,
  objptr* opp,
  __attribute__((unused)) void* rawArgs)
{
  printf("\t %p", (void*) *opp);
}

void printDownPtrs(
  __attribute__((unused)) GC_state s,
  objptr dst,
  objptr* field,
  objptr src,
  __attribute__((unused)) void* rawArgs)
{
  printf("(%p, %p, %p) ", (void*)dst, (void*)field, (void*)src);
}

void printChunkListSize(HM_chunkList list) {
  printf("sizes: ");
  for(HM_chunk chunk = list->firstChunk; chunk!=NULL; chunk = chunk->nextChunk) {
    printf("%zu ", HM_getChunkSize(chunk));
  }
  printf("\n");
}

bool isChunkInList(HM_chunk chunk, HM_chunkList list) {
  for(HM_chunk c = list->firstChunk; c!=NULL; c=c->nextChunk) {
    if(c == chunk)
      return TRUE;
  }
  return FALSE;
}

void forwardPtrChunk (GC_state s, objptr *opp, void* rawArgs) {
  objptr op = *opp;
  assert(isObjptr(op));
  pointer p = objptrToPointer (op, NULL);
  if (isInScope(HM_getChunkOf(p), rawArgs)
     || isChunkSaved(HM_getChunkOf(p), rawArgs)) {
    p = getTransitivePtr(p, rawArgs);
  }
  bool saved = saveNoForward(s, p, rawArgs);

  if(saved) {
    markAndScan(s, p, rawArgs);
  }
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
  p = getTransitivePtr(p, rawArgs);

  if(CC_isPointerMarked (p)) {
    assert(chunk->tmpHeap == ((ConcurrentCollectArgs*)rawArgs)->toHead);
    markObj(p);

    assert(!CC_isPointerMarked(p));

    struct GC_foreachObjptrClosure unmarkPtrClosure =
    {.fun = unmarkPtrChunk, .env = rawArgs};
    foreachObjptrInObject(s, p, &trueObjptrPredicateClosure,
            &unmarkPtrClosure, FALSE);
  }
}

void unmarkDownPtrChunk(
  GC_state s,
  objptr dst,
  __attribute__((unused)) objptr* field,
  objptr src,
  void* rawArgs)
{
  unmarkPtrChunk(s, &src, rawArgs);
  unmarkPtrChunk(s, &dst, rawArgs);
}

// This function does more than forwardPtrChunk.
// It scans the object pointed by the pointer even if its not in scope.
// Recursively however it only calls forwardPtrChunk and not itself
void forceForward(GC_state s, objptr *opp, void* rawArgs) {
  pointer p = objptrToPointer(*opp, NULL);

  bool saved = saveNoForward(s, p, rawArgs);

  if(saved && !CC_isPointerMarked(p)) {
    assert(getTransitivePtr(p, rawArgs) == p);
    markObj(p);
    ((ConcurrentCollectArgs*)rawArgs)->bytesSaved += sizeofObject(s, p);
  }

  struct GC_foreachObjptrClosure forwardPtrClosure =
  {.fun = forwardPtrChunk, .env = rawArgs};
  foreachObjptrInObject(s, p, &trueObjptrPredicateClosure,
          &forwardPtrClosure, FALSE);
}

void forceUnmark (GC_state s, objptr* opp, void* rawArgs) {
  pointer p = objptrToPointer(*opp, NULL);
  if(CC_isPointerMarked(p)){
    assert(getTransitivePtr(p, rawArgs) == p);
    markObj(p);
  }
  struct GC_foreachObjptrClosure unmarkPtrClosure =
  {.fun = unmarkPtrChunk, .env = rawArgs};
  foreachObjptrInObject(s, p, &trueObjptrPredicateClosure,
          &unmarkPtrClosure, FALSE);
}

void ensureCallSanity(
  __attribute__((unused)) GC_state s,
  ARG_USED_FOR_ASSERT HM_HierarchicalHeap targetHH,
  ARG_USED_FOR_ASSERT ConcurrentPackage args)
{
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

HM_HierarchicalHeap findHeap (GC_thread thread, uint32_t depth) {

  HM_HierarchicalHeap currentHeap = thread->hierarchicalHeap;
  while(currentHeap!=NULL && HM_HH_getDepth (currentHeap) != depth) {
    currentHeap = currentHeap->nextAncestor;
  }

  if(currentHeap==NULL)
    return NULL;

  assert(HM_HH_getDepth(currentHeap) == depth);
  return currentHeap;
}

bool readyforCollection(ConcurrentPackage cp) {
  bool ready =  (cp != NULL && cp->ccstate == CC_REG);
  if (ready) {
    assert(cp->rootList!=NULL);
    assert(cp->snapLeft != BOGUS_OBJPTR);
    assert(cp->snapRight != BOGUS_OBJPTR);
    assert(cp->stack != BOGUS_OBJPTR);
  }
  return ready;
}

// returns true if claim succeeds
bool claimHeap(HM_HierarchicalHeap heap) {
  if (heap == NULL) {
    return FALSE;
  }

  /** We should never be working on a (root) heap that was split. */
  assert(NULL == heap->subHeapForRootCC);

  ConcurrentPackage cp = HM_HH_getConcurrentPack(heap);
  if(!readyforCollection(cp)) {
    return FALSE;
  }
  else if (casCC (&(cp->ccstate), CC_REG, CC_COLLECTING) != CC_REG) {
    printf("\t %s\n", "returning because someone else claimed collection");
    return FALSE;
  }
  assert(HM_HH_getConcurrentPack(heap)->ccstate == CC_COLLECTING);
  return TRUE;
}

void CC_collectAtRoot(pointer threadp, pointer hhp) {
  GC_state s = pthread_getspecific (gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  HM_HierarchicalHeap heap = (HM_HierarchicalHeap)hhp;

  if (!checkLocalScheduler(s) || thread->currentDepth<=0) {
    return;
  }

  if (!claimHeap(heap)) {
    return;
  }

  // for exiting even if CC is going on.
  assert(!s->amInCC);
  s->amInCC = TRUE;

  size_t beforeSize = HM_getChunkListSize(HM_HH_getChunkList(heap));
  size_t live = CC_collectWithRoots(s, heap, thread);
  size_t afterSize = HM_getChunkListSize(HM_HH_getChunkList(heap));

  size_t diff = beforeSize > afterSize ? beforeSize - afterSize : 0;

  LOG(LM_CC_COLLECTION, LL_INFO,
    "before: %zu after: %zu (-%.01lf%%) live: %zu (%.01lf%% fragmented)",
    beforeSize,
    afterSize,
    100.0 * ((double)diff / (double)beforeSize),
    live,
    100.0 * (1.0 - (double)live / (double)afterSize));

  HM_HH_getConcurrentPack(heap)->ccstate = CC_UNREG;
  s->amInCC = FALSE;
}

uint32_t minPrivateLevel(GC_state s) {
  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t shallowestPrivateLevel = UNPACK_IDX(topval);
  uint32_t level = (shallowestPrivateLevel>0)?(shallowestPrivateLevel-1):0;
  return level;
}

void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth) {
  checkLocalScheduler(s);
  if (thread->currentDepth <= 1
    || depth <= 0
    || depth >= thread->currentDepth
    // Don't collect heaps that are private
    || depth > minPrivateLevel(s)
    ) {
    return;
  }

  HM_HierarchicalHeap heap = findHeap(thread, depth);
  if(!claimHeap(heap)){
    return;
  }

  // collect only if the heap is above a threshold size
  if (HM_getChunkListSize(&(heap->chunkList)) >= 2 * HM_BLOCK_SIZE) {
    assert(getThreadCurrent(s) == thread);
    CC_collectWithRoots(s, heap, thread);
  }

  // Mark that collection is complete
  HM_HH_getConcurrentPack(heap)->ccstate = CC_UNREG;
}

void CC_filterDownPointers(GC_state s, HM_chunkList x, HM_HierarchicalHeap hh){
  /** There is no race here, because for truly concurrent GC (depth 1), the
    * hh has been split.
    */
  assert(NULL == hh->subHeapForRootCC);
  HM_chunkList y = HM_HH_getRemSet(hh);
  struct HM_foreachDownptrClosure bucketIfValidAtListClosure =
  {.fun = bucketIfValidAtList, .env = (void*)x};

  HM_foreachRemembered(s, y, &bucketIfValidAtListClosure);
  HM_appendChunkList(getFreeListSmall(s), y);
  *y = *x;
}

size_t CC_collectWithRoots(GC_state s, HM_HierarchicalHeap targetHH,
                         GC_thread thread) {
  struct timespec startTime;
  struct timespec stopTime;

  timespec_now(&startTime);

  assert(NULL == targetHH->subHeapForRootCC);

  ConcurrentPackage cp = HM_HH_getConcurrentPack(targetHH);
  ensureCallSanity(s, targetHH, cp);
  // At the end of collection, repList will contain all the chunks that have
  // some object that is reachable from the roots. origList will contain the
  // chunks in which all objects are garbage. Before exiting, chunks in
  // origList are added to the free list.

  bool isConcurrent = (HM_HH_getDepth(targetHH) == 1);

  struct HM_chunkList _repList;
  HM_chunkList repList = &(_repList);
  HM_initChunkList(repList);
  HM_chunkList origList = HM_HH_getChunkList(targetHH);

  HM_assertChunkListInvariants(origList);

  ConcurrentCollectArgs lists = {
    .origList = origList,
    .repList  = repList,
    .toHead = (void*)repList,
    .fromHead = (void*) &(origList),
    .bytesSaved = 0
  };

  HH_EBR_enterQuiescentState(s);

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
    T->levelHead = HM_HH_getUFNode(targetHH);
  }

  /** SAM_NOTE: This code is no longer needed, because HH objects are not
    * stored in their own chunks. I'm leaving it commented for now (just for
    * reference), and will remove later.
    */
  // HM_chunk baseChunk = HM_getChunkOf((void *)targetHH);
  // if(isInScope(baseChunk, &lists)) {
  //   saveChunk(baseChunk, &lists);
  // }
  // else if (baseChunk == (targetHH->chunkList).firstChunk && isConcurrent) {}
  // else {
  //   // assert(0);
  // }

  // forward down pointers
  struct HM_chunkList downPtrs;
  HM_initChunkList(&downPtrs);
  CC_filterDownPointers(s, &downPtrs, targetHH);

  struct HM_foreachDownptrClosure forwardDownPtrChunkClosure =
  {.fun = forwardDownPtrChunk, .env = &lists};
  HM_foreachRemembered(s, &downPtrs, &forwardDownPtrChunkClosure);

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
  saveNoForward(s, (void*)(thread->stack), &lists);
  saveNoForward(s, (void*)thread, &lists);
  forEachObjptrinStack(s, cp->rootList, forwardPtrChunk, &lists);

#if ASSERT
  if (HM_HH_getDepth(targetHH) != 1){
    struct GC_foreachObjptrClosure printObjPtrInScopeClosure =
    {.fun = printObjPtrInScopeFunction, .env =  &lists};
    foreachObjptrInObject(
      s,
      objptrToPointer(thread->stack, NULL),
      &trueObjptrPredicateClosure,
      &printObjPtrInScopeClosure,
      FALSE);
  }
#endif

  struct HM_foreachDownptrClosure unmarkDownPtrChunkClosure =
  {.fun = unmarkDownPtrChunk, .env = &lists};
  HM_foreachRemembered(s, &downPtrs, &unmarkDownPtrChunkClosure);

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
    assert(HM_getLevelHead(Q) == targetHH);
    Q = Q->nextChunk;
  }
  assert(lenRep+lenFree == lenOrig);
  printf("%s %d \n", "Chunks Collected = ", lenFree);

  printf("%s", "collected chunks: ");
  for(HM_chunk chunk = origList->firstChunk; chunk!=NULL; chunk = chunk->nextChunk){
    printf("%p", chunk);
    assert(chunk->tmpHeap == lists.fromHead);
    assert(HM_getLevelHead(chunk) == targetHH);
  }
  printf("\n");
  // safety code
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

  HH_EBR_leaveQuiescentState(s);

  /** This is safe (no race with any zips), because `targetHH` is split off
    * from the main spine of the program.
    */
  HM_HH_freeAllDependants(s, targetHH, TRUE);

  HM_assertChunkListInvariants(origList);

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);

  if (isConcurrent) {
    timespec_add(&(s->cumulativeStatistics->timeRootCC), &stopTime);
    s->cumulativeStatistics->numRootCCs++;
    s->cumulativeStatistics->bytesReclaimedByRootCC += bytesScanned-bytesSaved;
  } else {
    timespec_add(&(s->cumulativeStatistics->timeInternalCC), &stopTime);
    s->cumulativeStatistics->numInternalCCs++;
    s->cumulativeStatistics->bytesReclaimedByInternalCC += bytesScanned-bytesSaved;
  }

  return lists.bytesSaved;

}

#endif
