/* Copyright (C) 2018-2021 Sam Westrick
 * Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the utility functions for the HierarchicalHeap object
 * described in hierarchical-heap.h.
 */

#include "hierarchical-heap.h"

/******************************/
/* Static Function Prototypes */
/******************************/

static void assertInvariants(GC_thread thread);

/** Update representative/dependant pointers for the HH union-find tree.
  * The left heap is made the representative, and the right heap is made
  * dependant.
  */
static inline void linkInto(
  GC_state s,
  HM_HierarchicalHeap left,
  HM_HierarchicalHeap right
);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))

HM_HierarchicalHeap HM_HH_zip(
  GC_state s,
  HM_HierarchicalHeap hh1,
  HM_HierarchicalHeap hh2)
{

#if ASSERT
  assert(NULL != hh1 || NULL != hh2);
  /* do some setup to remember what the state of the world was before the zip.
   * then after the zip, check what we can to make sure it worked correctly. */
  uint32_t maxd1 = (NULL == hh1 ? 0 : HM_HH_getDepth(hh1));
  uint32_t maxd2 = (NULL == hh2 ? 0 : HM_HH_getDepth(hh2));
  uint32_t maxd = max(maxd1, maxd2);
  HM_UnionFindNode nodes1[maxd+1];
  HM_UnionFindNode nodes2[maxd+1];
  for (uint32_t i = 0; i <= maxd; i++) { nodes1[i] = NULL; nodes2[i] = NULL; }
  for (HM_HierarchicalHeap h = hh1; NULL != h; h = h->nextAncestor)
    nodes1[HM_HH_getDepth(h)] = HM_HH_getUFNode(h);
  for (HM_HierarchicalHeap h = hh2; NULL != h; h = h->nextAncestor)
    nodes2[HM_HH_getDepth(h)] = HM_HH_getUFNode(h);
#endif

  HM_HierarchicalHeap result = NULL;

  HM_HierarchicalHeap *cursor = &result;
  while (NULL != hh1 && NULL != hh2)
  {
    uint32_t depth1 = HM_HH_getDepth(hh1);
    uint32_t depth2 = HM_HH_getDepth(hh2);

    if (depth1 == depth2)
    {
      HM_appendChunkList(HM_HH_getChunkList(hh1), HM_HH_getChunkList(hh2));
      HM_appendChunkList(HM_HH_getRemSet(hh1), HM_HH_getRemSet(hh2));

      // This has to happen before linkInto (which frees hh2)
      HM_HierarchicalHeap hh2anc = hh2->nextAncestor;

      linkInto(s, hh1, hh2);

      *cursor = hh1;
      cursor = &(hh1->nextAncestor);

      hh1 = hh1->nextAncestor;
      hh2 = hh2anc;
    }
    else if (depth1 > depth2)
    {
      *cursor = hh1;
      cursor = &(hh1->nextAncestor);

      hh1 = hh1->nextAncestor;
    }
    else /* depth1 < depth2 */
    {
      *cursor = hh2;
      cursor = &(hh2->nextAncestor);

      hh2 = hh2->nextAncestor;
    }
  }
  if (NULL != hh1) *cursor = hh1;
  if (NULL != hh2) *cursor = hh2;

#if ASSERT
  assert(NULL != result);
  assert(HM_HH_getDepth(result) == maxd);
  HM_HierarchicalHeap heapsResult[maxd+1];
  for (uint32_t i = 0; i <= maxd; i++) { heapsResult[i] = NULL; }
  for (HM_HierarchicalHeap h = result; NULL != h; h = h->nextAncestor)
    heapsResult[HM_HH_getDepth(h)] = h;

  HM_HierarchicalHeap prev = NULL;
  for (uint32_t i = 0; i <= maxd; i++)
  {
    /* check that the result is a proper linked list, sorted by depth */
    if (NULL != heapsResult[i])
    {
      assert(heapsResult[i]->nextAncestor == prev);
      prev = heapsResult[i];
    }

    /* check that the result contains exactly the heaps of the two inputs */
    if (NULL != nodes1[i] && NULL != nodes2[i])
    {
      assert(HM_HH_getUFNode(heapsResult[i]) == nodes1[i]);
      assert(nodes2[i]->representative == nodes1[i]);
    }
    else if (NULL != nodes1[i])
    {
      assert(HM_HH_getUFNode(heapsResult[i]) == nodes1[i]);
    }
    else if (NULL != nodes2[i])
    {
      assert(HM_HH_getUFNode(heapsResult[i]) == nodes2[i]);
    }
    else
    {
      assert(heapsResult[i] == NULL);
    }
  }
#endif

  return result;
}

void HM_HH_merge(
  GC_state s,
  GC_thread parentThread,
  GC_thread childThread)
{
  assert(parentThread->hierarchicalHeap != NULL);
  assert(childThread->hierarchicalHeap != NULL);

  HM_HierarchicalHeap parentHH = parentThread->hierarchicalHeap;
  HM_HierarchicalHeap childHH = childThread->hierarchicalHeap;

  assertInvariants(parentThread);
  assertInvariants(childThread);
  /* can only merge at join point! */
  assert(childThread->currentDepth == parentThread->currentDepth);
  assert(childThread->currentDepth >= 1);

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)childHH);

  // free stack of joining heap
  CC_freeStack(HM_HH_getConcurrentPack(childHH));

  /* Merge levels. */
  parentThread->hierarchicalHeap = HM_HH_zip(s, parentHH, childHH);

  parentThread->bytesSurvivedLastCollection +=
    childThread->bytesSurvivedLastCollection;
  parentThread->bytesAllocatedSinceLastCollection +=
    childThread->bytesAllocatedSinceLastCollection;

  assertInvariants(parentThread);
}

void HM_HH_promoteChunks(
  GC_state s,
  GC_thread thread)
{
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;

  if (HM_HH_getDepth(hh) < thread->currentDepth)
  {
    /* no need to do anything; this function only guarantees that the
     * current depth has been completely evacuated. */
    return;
  }

  uint32_t currentDepth = thread->currentDepth;
  assert(HM_HH_getDepth(hh) == currentDepth);

  if (NULL == hh->nextAncestor ||
      HM_HH_getDepth(hh->nextAncestor) < currentDepth-1)
  {
    /* There is no heap immediately above the leaf, so we can leave the current
     * structure intact and just decrement the recorded depth. */
    hh->depth--;
  }
  else
  {
    /* There is a heap immediately above the leaf, so merge into that heap. */
    assert(NULL != hh->nextAncestor);
    assert(HM_HH_getDepth(hh->nextAncestor) == currentDepth-1);
    HM_appendChunkList(HM_HH_getChunkList(hh->nextAncestor), HM_HH_getChunkList(hh));
    HM_appendChunkList(HM_HH_getRemSet(hh->nextAncestor), HM_HH_getRemSet(hh));

    /* shortcut.  */
    thread->hierarchicalHeap = hh->nextAncestor;
    /* don't need the snapshot for this heap now. */
    CC_freeStack(HM_HH_getConcurrentPack(hh));
    linkInto(s, hh->nextAncestor, hh);
  }

  assert(HM_HH_getDepth(thread->hierarchicalHeap) < thread->currentDepth);
  assertInvariants(thread);
}

bool HM_HH_isLevelHead(HM_HierarchicalHeap hh)
{
  return (NULL != hh)
      && (NULL != HM_HH_getUFNode(hh))
      && (HM_HH_getUFNode(hh)->payload == hh)
      && (NULL == HM_HH_getUFNode(hh)->representative);
}

HM_HierarchicalHeap HM_HH_new(GC_state s, uint32_t depth)
{
  HM_UnionFindNode uf = allocateFixedSize(getUFAllocator(s));
  HM_HierarchicalHeap hh = allocateFixedSize(getHHAllocator(s));

  uf->dependant1 = NULL;
  uf->dependant2 = NULL;
  uf->representative = NULL;
  uf->payload = hh;

  HM_HH_getConcurrentPack(hh)->rootList = NULL;
  HM_HH_getConcurrentPack(hh)->snapLeft = BOGUS_OBJPTR;
  HM_HH_getConcurrentPack(hh)->snapRight = BOGUS_OBJPTR;
  HM_HH_getConcurrentPack(hh)->stack = BOGUS_OBJPTR;
  HM_HH_getConcurrentPack(hh)->ccstate = CC_UNREG;
  HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection = 0;
  HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection = 0;

  // hh->representative = NULL;
  hh->ufNode = uf;
  hh->subHeapForRootCC = NULL;
  hh->depth = depth;
  hh->nextAncestor = NULL;
  // hh->dependant1 = NULL;
  // hh->dependant2 = NULL;
  hh->numDependants = 0;
  hh->heightDependants = 0;

  HM_initChunkList(HM_HH_getChunkList(hh));
  HM_initChunkList(HM_HH_getRemSet(hh));

  return hh;
}

uint32_t HM_HH_getDepth(HM_HierarchicalHeap hh)
{
  return hh->depth;
}

HM_HierarchicalHeap HM_HH_getHeapAtDepth(
  __attribute__((unused)) GC_state s,
  GC_thread thread,
  uint32_t depth)
{
  assert(depth <= thread->currentDepth);

  /* walk up while (a) not end of list and (b) still deeper than desired */
  HM_HierarchicalHeap *cursor = &(thread->hierarchicalHeap);
  while (NULL != *cursor &&
         HM_HH_getDepth(*cursor) > depth)
  {
    cursor = &((*cursor)->nextAncestor);
  }

  /* check if found it and return */
  HM_HierarchicalHeap hh = *cursor;
  if (NULL != hh && HM_HH_getDepth(hh) == depth)
    return hh;

  /* otherwise, either missed it or at end of list. */
  assert(NULL == hh || HM_HH_getDepth(hh) < depth);

  HM_HierarchicalHeap newhh = HM_HH_new(s, depth);
  newhh->nextAncestor = hh;
  *cursor = newhh;
  return newhh;
}

void HM_HH_ensureNotEmpty(GC_state s, GC_thread thread) {
  if (NULL != thread->currentChunk) return;

#if ASSERT
  assert(NULL != thread->hierarchicalHeap);
  for (HM_HierarchicalHeap cursor = thread->hierarchicalHeap;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    assert(NULL != HM_HH_getChunkList(cursor));
    assert(NULL == HM_HH_getChunkList(cursor)->firstChunk);
  }
#endif

  /* add in one chunk */
  if (!HM_HH_extend(s, thread, GC_HEAP_LIMIT_SLOP)) {
    DIE("Ran out of space for Hierarchical Heap!");
  }
}

bool HM_HH_extend(GC_state s, GC_thread thread, size_t bytesRequested)
{
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  uint32_t currentDepth = thread->currentDepth;

  assert(NULL != hh);
  assert(NULL != HM_HH_getChunkList(hh));
  assert(HM_HH_getDepth(hh) <= currentDepth);

  HM_chunk chunk;

  if (HM_HH_getDepth(hh) < currentDepth)
  {
    HM_HierarchicalHeap newhh = HM_HH_new(s, currentDepth);
    newhh->nextAncestor = hh;
    thread->hierarchicalHeap = newhh;

    hh = newhh;
  }

  chunk = HM_allocateChunk(HM_HH_getChunkList(hh), bytesRequested);

  if (NULL == chunk) {
    return FALSE;
  }

  chunk->levelHead = HM_HH_getUFNode(hh);

  thread->currentChunk = chunk;
  HM_HH_addRecentBytesAllocated(thread, HM_getChunkSize(chunk));

  return TRUE;
}

void HM_HH_forceLeftHeap(
  ARG_USED_FOR_ASSERT uint32_t processor,
  pointer threadp)
{
  GC_state s = pthread_getspecific (gcstate_key);
  assert(processor < s->numberOfProcs);
  GC_MayTerminateThread(s);
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  beginAtomic(s);
  assert(getThreadCurrent(s)->hierarchicalHeap != NULL);
  assert(threadAndHeapOkay(s));
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  HM_HH_updateValues(thread, s->frontier);

  if (s->limitPlusSlop < s->frontier) {
    DIE("s->limitPlusSlop (%p) < s->frontier (%p)",
        ((void*)(s->limit)),
        ((void*)(s->frontier)));
  }

  // JATIN_TODO: have a better bytesRequested here.
  size_t bytesRequested = GC_HEAP_LIMIT_SLOP;
  if (!HM_HH_extend (s, thread, bytesRequested)){
    assert(0);
  }

  s->frontier = HM_HH_getFrontier(thread);
  s->limitPlusSlop = HM_HH_getLimit(thread);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
  endAtomic(s);
}

/** Migrate all chunks of the thread's HH into the side heap (subHeapForRootCC),
  * used for concurrent collection. Also allocate a fresh hierarchical heap
  * for the thread.
  */
void mergeHeapForRootCC(GC_state s, GC_thread thread) {
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  HM_HierarchicalHeap subhh = hh->subHeapForRootCC;
  assert(HM_HH_getDepth(hh) == 1);
  assert(NULL != subhh);

  HM_appendChunkList(HM_HH_getChunkList(subhh), HM_HH_getChunkList(hh));
  HM_appendChunkList(HM_HH_getRemSet(subhh), HM_HH_getRemSet(hh));
  linkInto(s, subhh, hh);

  HM_HierarchicalHeap newHH = HM_HH_new(s, 1);
  thread->hierarchicalHeap = newHH;
  HM_chunk chunk =
    HM_allocateChunk(HM_HH_getChunkList(newHH), GC_HEAP_LIMIT_SLOP);
  chunk->levelHead = HM_HH_getUFNode(newHH);
  thread->currentChunk = chunk;
  newHH->subHeapForRootCC = subhh;
}

bool checkPolicyforRoot(
  __attribute__((unused)) GC_state s,
  GC_thread thread)
{
  assert(HM_HH_getDepth(thread->hierarchicalHeap) == 1);
  assert(NULL != thread->hierarchicalHeap);
  HM_HierarchicalHeap hh = thread->hierarchicalHeap->subHeapForRootCC;
  assert(NULL != hh);
  assert(NULL == hh->subHeapForRootCC);
  HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection =
    HM_getChunkListSize(HM_HH_getChunkList(hh));
  // return true;
  // thread->bytesAllocatedSinceLastCollection = 0;
  // thread->bytesSurvivedLastCollection s= (HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection)/2;
  // HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection/=2;
  // HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection;
  if((2*HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection) >
      (HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection)
    || HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection == 0) {
    // if (!HM_HH_getConcurrentPack(hh)->shouldCollect) {
      // HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection/=2;
    // }
    HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection +=4;
    return FALSE;
  }
  return TRUE;
}

objptr copyCurrentStack(GC_state s, GC_thread thread) {
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  pointer stackPtr = objptrToPointer(getStackCurrentObjptr(s), NULL);
  GC_stack stackP = (GC_stack) stackPtr;

  size_t objectSize, copySize, metaDataSize;
  metaDataSize = GC_STACK_METADATA_SIZE;
  copySize = sizeof(struct GC_stack) + stackP->used + metaDataSize;
  // objectSize = sizeofObject(s, stackPtr);
  objectSize = copySize;
  // copyObject can add a chunk to the list. It updates the frontier but not the
  // thread current chunk. Also it returns the pointer to the header part.
  pointer stackCopy = copyObject(stackPtr - metaDataSize,
                                 objectSize, copySize, hh);
  thread->currentChunk = HM_getChunkListLastChunk(HM_HH_getChunkList(hh));
  stackCopy += metaDataSize;
  ((GC_stack)stackCopy)->reserved = ((GC_stack)stackCopy)->used;
  return pointerToObjptr(stackCopy, NULL);
}

pointer HM_HH_getRoot(ARG_USED_FOR_ASSERT pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);

  /** Superfluous argument to function... */
#if ASSERT
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(getThreadCurrent(s) == thread);
#endif

  if (getThreadCurrent(s)->currentDepth != 1) {
    DIE("not at root");
  }

  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  HM_ensureHierarchicalHeapAssurances(s, FALSE, GC_HEAP_LIMIT_SLOP, TRUE);
  assert(HM_HH_getDepth(getThreadCurrent(s)->hierarchicalHeap) == 1);

  if (NULL == getThreadCurrent(s)->hierarchicalHeap->subHeapForRootCC) {
    getThreadCurrent(s)->hierarchicalHeap->subHeapForRootCC = HM_HH_new(s, 1);
  }

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  return (void*)(getThreadCurrent(s)->hierarchicalHeap->subHeapForRootCC);
}

// Story: ccstate for each hh has three values
// 1. CC_UNREG: This means that the root-set for the next collection (if we want to collect) needs to be constructed.
//              Either the previous root-set has already been used for a collection (which has finished) or this
//              is the first time a root-set is being made for this hh.
// 2. CC_REG:   This means that the root-set has been constructed but the collection hasn't started
// 3. CC_COLLECTING: The collector sets this value to ccstate when it starts collecting using cas.
//                   After its finished, the flag is set to CC_UNREG indicating that a new root set is needed.
Bool HM_HH_registerCont(pointer kl, pointer kr, pointer k, pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  // So, we should really get rid of the extra argument.
  assert(thread == getThreadCurrent(s));

  GC_MayTerminateThread(s);
  beginAtomic(s);
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(thread, s->frontier);
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  if (s->limitPlusSlop < s->frontier) {
    DIE("s->limitPlusSlop (%p) < s->frontier (%p)",
        ((void*)(s->limit)),
        ((void*)(s->frontier)));
  }

  /** At depth 1, there is a special secondary "subHeapForRootCC" which is
    * used instead of the thread's hh at depth 1. This is to avoid races
    * between the processor evaluating the thread, and the processor working
    * on root CC.
    */
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  if (1 == HM_HH_getDepth(hh)) {
    assert(NULL != hh->subHeapForRootCC);
    hh = hh->subHeapForRootCC;
  }
  assert(NULL == hh->subHeapForRootCC);

  CC_initStack(HM_HH_getConcurrentPack(hh));

  if (HM_HH_getDepth(hh) == 1) {
    if (HM_HH_getConcurrentPack(hh)->ccstate != CC_UNREG) {
      // the CC at depth 1 hasn't completed yet, so don't
      // do anything.
      assert(invariantForMutatorFrontier (s));
      assert(invariantForMutatorStack (s));
      endAtomic(s);
      return FALSE;
    }
  }
  else {
    HM_HH_getConcurrentPack(hh)->ccstate = CC_UNREG;
  }

  if (HM_HH_getDepth(hh) == 1) {
    mergeHeapForRootCC(s, thread);
    if (!checkPolicyforRoot(s, thread)) {
      // Not collecting, so keep ccstate = CC_UNREG
      s->frontier = HM_HH_getFrontier(thread);
      s->limitPlusSlop = HM_HH_getLimit(thread);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
      assert(invariantForMutatorFrontier (s));
      assert(invariantForMutatorStack (s));
      endAtomic(s);
      return FALSE;
    }
  }
  assert(HM_getLevelHead(HM_getChunkOf(kl)) == hh);
  assert(HM_getLevelHead(HM_getChunkOf(kr)) == hh);
  assert(HM_HH_getConcurrentPack(hh) != NULL);

  /** This is subtle. Notice that we always copy the stack, regardless of
    * whether or not this heap is going to be collected truly concurrently,
    * or by the left-hand-side processor.
    *
    * It might seem like we could get away with using the left-hand-side stack
    * for roots, at least for CCs in a processor's spine, because this stack
    * is used for the continuation, after the join. HOWEVER, THIS IS NOT SAFE!
    * Why? Because we currently don't trace up-pointers outside of local
    * collections. So, an object might be reachable via current stack at fork,
    * but then later only be reachable via an up-pointer (removed from stack
    * and "hidden" in a data-structure).
    *
    * In the future, if we do proper spine collections as described in the
    * POPL'21 paper, then all up-pointers would be properly tracked, and we
    * could get away with no copying the stack here. This would require
    * integrating local (copying) with internal (mark-sweep) collections,
    * because a local collection might discover an up-pointer which should be
    * treated as a root for mark-sweep.
    */
  HM_HH_getConcurrentPack(hh)->snapLeft  =  pointerToObjptr(kl, NULL);
  HM_HH_getConcurrentPack(hh)->snapRight =  pointerToObjptr(kr, NULL);
  HM_HH_getConcurrentPack(hh)->snapTemp =   pointerToObjptr(k, NULL);
  HM_HH_getConcurrentPack(hh)->stack = copyCurrentStack(s, thread);

  CC_clearStack(HM_HH_getConcurrentPack(hh));
  HM_HH_getConcurrentPack(hh)->ccstate = CC_REG;

  s->frontier = HM_HH_getFrontier(thread);
  s->limitPlusSlop = HM_HH_getLimit(thread);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
  endAtomic(s);

  return TRUE;
}

HM_HierarchicalHeap HM_HH_getCurrent(GC_state s) {
  return getThreadCurrent(s)->hierarchicalHeap;
}

pointer HM_HH_getFrontier(GC_thread thread) {
  assert(blockOf(HM_getChunkFrontier(thread->currentChunk)) == (pointer)thread->currentChunk);
  return HM_getChunkFrontier(thread->currentChunk);
}

pointer HM_HH_getLimit(GC_thread thread) {
  return HM_getChunkLimit(thread->currentChunk);
}

void HM_HH_updateValues(GC_thread thread, pointer frontier) {
  HM_updateChunkFrontierInList(
    HM_HH_getChunkList(HM_getLevelHeadPathCompress(thread->currentChunk)),
    thread->currentChunk,
    frontier);
}

size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize) {
  size_t threshold =
    (size_t)((double)survivingSize * s->controls->hhConfig.collectionThresholdRatio);
  if (threshold < s->controls->hhConfig.minCollectionSize) {
    threshold = s->controls->hhConfig.minCollectionSize;
  }
  return threshold;
}

size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes) {
  thread->bytesAllocatedSinceLastCollection += bytes;
  return thread->bytesAllocatedSinceLastCollection;
}

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  if (s->wsQueueTop == BOGUS_OBJPTR)
    return thread->currentDepth+1; /* don't collect */

  if (thread->bytesAllocatedSinceLastCollection <
      (s->controls->hhConfig.collectionThresholdRatio * thread->bytesSurvivedLastCollection))
  {
    return thread->currentDepth+1; /* don't collect */
  }

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);

  size_t budget = 4 * thread->bytesAllocatedSinceLastCollection;

  if (budget < s->controls->hhConfig.minCollectionSize ||
      potentialLocalScope > thread->currentDepth ||
      potentialLocalScope > HM_HH_getDepth(hh) ||
      HM_getChunkListSize(HM_HH_getChunkList(hh)) > budget)
  {
    return thread->currentDepth+1; /* don't collect */
  }

  HM_HierarchicalHeap cursor = hh;
  size_t sz = HM_getChunkListSize(HM_HH_getChunkList(hh));
  while (NULL != cursor->nextAncestor &&
         HM_HH_getDepth(cursor->nextAncestor) >= potentialLocalScope &&
         HM_getChunkListSize(HM_HH_getChunkList(cursor->nextAncestor)) + sz < budget)
  {
    cursor = cursor->nextAncestor;
    sz += HM_getChunkListSize(HM_HH_getChunkList(cursor));
  }

#if ASSERT
  uint32_t minDepthOkayForBudget = HM_HH_getDepth(cursor);
#endif

  if (sz < s->controls->hhConfig.minCollectionSize)
    return thread->currentDepth+1; /* don't collect if too small */

  /* It's likely that the shallower levels are mostly empty, so let's see if
   * we can skip some of them without ignoring too much data. */
  size_t newBudget = 0.75 * sz;
  cursor = hh;
  sz = HM_getChunkListSize(HM_HH_getChunkList(hh));
  while (NULL != cursor->nextAncestor &&
         HM_getChunkListSize(HM_HH_getChunkList(cursor->nextAncestor)) + sz < newBudget)
  {
    cursor = cursor->nextAncestor;
    sz += HM_getChunkListSize(HM_HH_getChunkList(cursor));
  }
  uint32_t desiredMinDepth = HM_HH_getDepth(cursor);

  assert(desiredMinDepth >= minDepthOkayForBudget);
  assert(desiredMinDepth >= potentialLocalScope);
  assert(desiredMinDepth <= thread->currentDepth);

  return desiredMinDepth;
}

bool HM_HH_isCCollecting(HM_HierarchicalHeap hh) {
  assert(hh!=NULL);
  if (HM_HH_getConcurrentPack(hh) != NULL)
    return  ((HM_HH_getConcurrentPack(hh))->ccstate == CC_COLLECTING);
  return false;
}

void HM_HH_addRootForCollector(HM_HierarchicalHeap hh, pointer p) {
  assert(hh!=NULL);
  if(HM_HH_getConcurrentPack(hh)!=NULL){
    CC_addToStack(HM_HH_getConcurrentPack(hh), p);
  }
}


void HM_HH_freeAllDependants(
  GC_state s,
  HM_HierarchicalHeap hh,
  bool retireInsteadOfFree)
{
  assert(HM_HH_isLevelHead(hh));
  FixedSizeAllocator myUFAllocator = getUFAllocator(s);

  HM_UnionFindNode parent = HM_HH_getUFNode(hh);
  HM_UnionFindNode child = parent->dependant1;
  parent->dependant1 = NULL;

  size_t numFreed = 0;

  /** Invariant: parent (and every node above it) has an inverted dependant1
    * pointer, which is pointing to its ancestor. There must be NO pointer
    * in memory from child to parent. It's possible that the parent may have
    * some right child, still given by dependant2.
    *
    *                ???              Legend:
    *                 ^ \             @@@  node (definitely non-NULL)
    *                 ^  \            ???  possibly NULL node
    *     parent --> @@@  ???         \    normal pointer
    *                   \             ^    inverted pointer
    *                    \
    *      child --> ???  ???
    *
    * There are four cases.
    *   1) child NULL, sibling NULL:
    *        slide up to parent
    *   2) child NULL, sibling non-NULL:
    *        swap child and sibling
    *   3) child non-NULL, left grandchild NULL:
    *        free child and switch to right grandchild
    *   4) child non-NULL, left grandchild non-NULL:
    *        slide down to grandchild
    */
  while (parent != NULL) {
    if (NULL == child) {
      if (NULL == parent->dependant2) {
        // go back UP the tree (note that parent->dependant1 is INVERTED)
        child = parent;
        parent = parent->dependant1;
        child->dependant1 = NULL;
      }
      else {
        // switch to other child
        child = parent->dependant2;
        parent->dependant2 = NULL;
      }
    }
    else {
      if (child->dependant1 == NULL) {
        // free and jump to other grandchild
        HM_UnionFindNode grandchild = child->dependant2;

        if (retireInsteadOfFree) {
          HH_EBR_retire(s, child);
        } else {
          freeFixedSize(myUFAllocator, child);
        }
        numFreed++;

        child = grandchild;
      }
      else {
        // move DOWN the tree (careful to INVERT the pointer)
        HM_UnionFindNode grandchild = child->dependant1;
        child->dependant1 = parent;
        parent = child;
        child = grandchild;
      }
    }
  }

  assert(numFreed == hh->numDependants);
  hh->numDependants = 0;
  hh->heightDependants = 0;

  assert(HM_HH_isLevelHead(hh));
}


#endif /* MLTON_GC_INTERNAL_FUNCS */

/*******************************/
/* Static Function Definitions */
/*******************************/

static inline void linkInto(
  GC_state s,
  HM_HierarchicalHeap left,
  HM_HierarchicalHeap right)
{
  assert(NULL == HM_HH_getUFNode(right)->representative);
  assert(NULL == HM_HH_getUFNode(left)->dependant2);
  assert(NULL == HM_HH_getUFNode(right)->dependant2);

  HM_HH_getUFNode(right)->representative = HM_HH_getUFNode(left);
  HM_HH_getUFNode(right)->dependant2 = HM_HH_getUFNode(left)->dependant1;
  HM_HH_getUFNode(left)->dependant1 = HM_HH_getUFNode(right);

  left->numDependants += 1 + right->numDependants;

  size_t lh = left->heightDependants;
  size_t rh = right->heightDependants;
  left->heightDependants = 1 + (lh > rh ? lh : rh);

  assert(NULL == HM_HH_getUFNode(left)->dependant2);

  HM_HH_getUFNode(right)->payload = NULL;
  freeFixedSize(getHHAllocator(s), right);

  assert(HM_HH_isLevelHead(left));
}

#if ASSERT

void assertInvariants(GC_thread thread)
{
  // return;
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  assert(NULL != hh);

  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL;
       cursor = cursor->nextAncestor)
  {
    assert(HM_HH_isLevelHead(cursor));
    HM_chunkList list = HM_HH_getChunkList(cursor);
    assert(NULL != list);
    HM_assertChunkListInvariants(list);

    /* verify levelHeads */
    for (HM_chunk chunk = list->firstChunk;
         NULL != chunk;
         chunk = chunk->nextChunk)
    {
      assert(HM_getLevelHead(chunk) == cursor);
    }
  }

  /* check sorted by depth */
  uint32_t lastDepth = UINT32_MAX;
  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL;
       cursor = cursor->nextAncestor)
  {
    uint32_t thisDepth = HM_HH_getDepth(cursor);
    assert(thisDepth < lastDepth);
    lastDepth = thisDepth;
  }

  /* Check that we haven't exceeded the currentDepth */
  uint32_t heapDepth = HM_HH_getDepth(hh);
  assert(heapDepth <= thread->currentDepth);

  /* make sure that the current chunk is owned by this thread. */
  if (NULL != thread->currentChunk) {
    HM_HierarchicalHeap levelHead = HM_getLevelHead(thread->currentChunk);
    bool foundChunk = FALSE;
    for (HM_chunk chunk = HM_HH_getChunkList(levelHead)->firstChunk;
         chunk != NULL;
         chunk = chunk->nextChunk)
    {
      if (chunk == thread->currentChunk) {
        foundChunk = TRUE;
        break;
      }
    }
    assert(foundChunk);
  }
}

#else

void assertInvariants(ARG_USED_FOR_ASSERT GC_thread thread) {
  return;
}

#endif /* ASSERT */
