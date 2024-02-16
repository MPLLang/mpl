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

void assertCCChainInvariants(ARG_USED_FOR_ASSERT HM_HierarchicalHeap hh) {
#if ASSERT
  assert(hh != NULL);
  assert(HM_HH_isLevelHead(hh));

  HM_HierarchicalHeap cursor = hh->subHeapForCC;
  while (cursor != NULL) {
    assert(HM_HH_getConcurrentPack(cursor)->ccstate != CC_UNREG);
    assert(HM_HH_isLevelHead(cursor));
    assert(cursor->nextAncestor == NULL);
    assert(cursor->depth == hh->depth);
    /** Make sure its "completed" pointer is in the completed chain. */
    bool foundIt = FALSE;
    for (HM_HierarchicalHeap completedCursor = hh->subHeapCompletedCC;
         completedCursor != NULL;
         completedCursor = completedCursor->subHeapCompletedCC)
    {
      if (completedCursor == cursor->subHeapCompletedCC) {
        foundIt = TRUE;
        break;
      }
    }
    assert(foundIt);
    cursor = cursor->subHeapForCC;
  }

  cursor = hh->subHeapCompletedCC;
  while (cursor != NULL) {
    assert(HM_HH_getConcurrentPack(cursor)->ccstate == CC_DONE);
    assert(HM_HH_isLevelHead(cursor));
    assert(cursor->nextAncestor == NULL);
    assert(cursor->depth == hh->depth);
    assert(NULL == cursor->subHeapForCC);
    cursor = cursor->subHeapCompletedCC;
  }
#endif
}

size_t lengthOfCCChain(HM_HierarchicalHeap hh) {
  assert(NULL != hh);
  HM_HierarchicalHeap cursor = hh->subHeapForCC;
  size_t count = 0;
  while (cursor != NULL) {
    count++;
    cursor = cursor->subHeapForCC;
  }
  return count;
}

size_t lengthOfCompletedCCChain(HM_HierarchicalHeap hh) {
  assert(NULL != hh);
  HM_HierarchicalHeap cursor = hh->subHeapCompletedCC;
  size_t count = 0;
  while (cursor != NULL) {
    count++;
    cursor = cursor->subHeapCompletedCC;
  }
  return count;
}

void setCCChainDepth(HM_HierarchicalHeap hh, uint32_t newDepth) {
  for (HM_HierarchicalHeap cursor = hh->subHeapForCC;
       NULL != cursor;
       cursor = cursor->subHeapForCC)
  {
    cursor->depth = newDepth;
  }
}

void setCompletedCCChainDepth(HM_HierarchicalHeap hh, uint32_t newDepth) {
  for (HM_HierarchicalHeap cursor = hh->subHeapCompletedCC;
       NULL != cursor;
       cursor = cursor->subHeapCompletedCC)
  {
    cursor->depth = newDepth;
  }
}

/** link the CC-chain of hh2 into hh1. */
void linkCCChains(
  __attribute__((unused)) GC_state s,
  HM_HierarchicalHeap hh1,
  HM_HierarchicalHeap hh2)
{
#if ASSERT
  assert(hh1 != NULL);
  assert(hh2 != NULL);
  assertCCChainInvariants(hh1);
  assertCCChainInvariants(hh2);
  size_t len1 = lengthOfCCChain(hh1);
  size_t len2 = lengthOfCCChain(hh2);
  size_t lenC1 = lengthOfCompletedCCChain(hh1);
  size_t lenC2 = lengthOfCompletedCCChain(hh2);
#endif

  setCCChainDepth(hh2, hh1->depth);
  setCompletedCCChainDepth(hh2, hh1->depth);

  if (NULL == hh1->subHeapForCC) {
    assert(NULL == hh1->subHeapCompletedCC);
    hh1->subHeapForCC = hh2->subHeapForCC;
    hh1->subHeapCompletedCC = hh2->subHeapCompletedCC;
    hh2->subHeapForCC = NULL;
    hh2->subHeapCompletedCC = NULL;
    assertCCChainInvariants(hh1);
    assertCCChainInvariants(hh2);
    assert(lengthOfCCChain(hh1) == len1+len2);
    assert(lengthOfCCChain(hh2) == 0);
    assert(lengthOfCompletedCCChain(hh1) == lenC1+lenC2);
    assert(lengthOfCompletedCCChain(hh2) == 0);
    return;
  }

  if (NULL == hh2->subHeapForCC) {
    assert(NULL == hh2->subHeapCompletedCC);
    assertCCChainInvariants(hh1);
    assertCCChainInvariants(hh2);
    assert(lengthOfCCChain(hh1) == len1+len2);
    assert(lengthOfCCChain(hh2) == 0);
    assert(lengthOfCompletedCCChain(hh1) == lenC1+lenC2);
    assert(lengthOfCompletedCCChain(hh2) == 0);
    return;
  }

  assert(NULL != hh1->subHeapForCC);
  assert(NULL != hh1->subHeapCompletedCC);
  assert(NULL != hh2->subHeapForCC);
  assert(NULL != hh2->subHeapCompletedCC);

  /** Link the two chains:
    *   1. Find the end of the chain on hh2
    *   2. Set the next pointer to the front of the chain of hh1
    *   3. Set the front pointer of the chain hh1 to front of chain2
    * And do this for both subHeapForCC and subHeapCompletedCC chains.
    */
  HM_HierarchicalHeap endOfChain2 = hh2->subHeapForCC;
  while (NULL != endOfChain2->subHeapForCC)
    endOfChain2 = endOfChain2->subHeapForCC;
  assert(NULL == endOfChain2->subHeapForCC);
  endOfChain2->subHeapForCC = hh1->subHeapForCC;
  hh1->subHeapForCC = hh2->subHeapForCC;
  hh2->subHeapForCC = NULL;

  HM_HierarchicalHeap endOfCompletedChain2 = hh2->subHeapCompletedCC;
  while (NULL != endOfCompletedChain2->subHeapCompletedCC)
    endOfCompletedChain2 = endOfCompletedChain2->subHeapCompletedCC;
  assert(NULL == endOfCompletedChain2->subHeapCompletedCC);
  endOfCompletedChain2->subHeapCompletedCC = hh1->subHeapCompletedCC;
  hh1->subHeapCompletedCC = hh2->subHeapCompletedCC;
  hh2->subHeapCompletedCC = NULL;

  assertCCChainInvariants(hh1);
  assertCCChainInvariants(hh2);
  assert(lengthOfCCChain(hh1) == len1+len2);
  assert(lengthOfCCChain(hh2) == 0);
  assert(lengthOfCompletedCCChain(hh1) == lenC1+lenC2);
  assert(lengthOfCompletedCCChain(hh2) == 0);
}


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
      // This has to happen before linkInto (which frees hh2)
      HM_HierarchicalHeap hh2anc = hh2->nextAncestor;
      CC_freeStack(s, HM_HH_getConcurrentPack(hh2));
      linkCCChains(s, hh1, hh2);
      linkInto(s, hh1, hh2);

      HM_appendChunkList(HM_HH_getChunkList(hh1), HM_HH_getChunkList(hh2));
      ES_move(HM_HH_getSuspects(hh1), HM_HH_getSuspects(hh2));
      HM_appendRemSet(HM_HH_getRemSet(hh1), HM_HH_getRemSet(hh2));


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
  CC_freeStack(s, HM_HH_getConcurrentPack(childHH));

  /* Merge levels. */
  parentThread->hierarchicalHeap = HM_HH_zip(s, parentHH, childHH);

  parentThread->spareHeartbeats += childThread->spareHeartbeats;
  //parentThread->spareHeartbeats = min(parentThread->spareHeartbeats, 100);

  parentThread->bytesSurvivedLastCollection +=
    childThread->bytesSurvivedLastCollection;
  parentThread->bytesAllocatedSinceLastCollection +=
    childThread->bytesAllocatedSinceLastCollection;

  assertInvariants(parentThread);
}


void HM_HH_clearSuspectsAtDepth(
  GC_state s,
  GC_thread thread,
  uint32_t targetDepth)
{
  // walk to find heap; only clear suspects at the target depth
  for (HM_HierarchicalHeap cursor = thread->hierarchicalHeap;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    uint32_t d = HM_HH_getDepth(cursor);
    if (d <= targetDepth) {
      if (d == targetDepth) ES_clear(s, cursor);
      return;
    }
  }
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
    setCCChainDepth(hh, currentDepth-1);
    setCompletedCCChainDepth(hh, currentDepth-1);
  }
  else
  {
    /* There is a heap immediately above the leaf, so merge into that heap. */
    assert(NULL != hh->nextAncestor);
    assert(HM_HH_getDepth(hh->nextAncestor) == currentDepth-1);
    HM_HierarchicalHeap parent = hh->nextAncestor;

    assert(hh == thread->hierarchicalHeap);
    mergeCompletedCCs(s, hh);
    assert(hh == thread->hierarchicalHeap);

    if (NULL == hh->subHeapForCC) {
      assert(NULL == hh->subHeapCompletedCC);
      /* don't need the snapshot for this heap now. */
      CC_freeStack(s, HM_HH_getConcurrentPack(hh));
      linkCCChains(s, parent, hh);
      linkInto(s, parent, hh);

      HM_appendChunkList(HM_HH_getChunkList(parent), HM_HH_getChunkList(hh));
      ES_move(HM_HH_getSuspects(parent), HM_HH_getSuspects(hh));
      HM_appendRemSet(HM_HH_getRemSet(parent), HM_HH_getRemSet(hh));
      /* shortcut.  */
      thread->hierarchicalHeap = parent;
      hh = parent;
    }
    else
    {
      assert(HM_getLevelHead(thread->currentChunk) == hh);

#if ASSERT
      size_t lenParent = lengthOfCCChain(parent);
      size_t lenCParent = lengthOfCompletedCCChain(parent);
      size_t len = lengthOfCCChain(hh);
      size_t lenC = lengthOfCompletedCCChain(hh);
#endif

      /** It's not safe to trigger new CCs in the parent space until all CCs
        * at the child have completed (otherwise there could be an immutable
        * internal-pointer from a CCed region into the new space which is not
        * tracked by the snapshot.
        *
        * So, since there are outstanding CCs in the child, we need to prevent
        * new CCs at the parent. We do this by adding the parent to the
        * "completed" chain. It will be merged in later, when all CCs have
        * finished.
        */
      HM_HierarchicalHeap nextAncestor = parent->nextAncestor;
      assert(NULL == nextAncestor || HM_HH_getDepth(nextAncestor) < currentDepth-1);

      linkCCChains(s, parent, hh);
      assert(NULL == hh->subHeapForCC);
      assert(NULL == hh->subHeapCompletedCC);

      hh->nextAncestor = nextAncestor;
      parent->nextAncestor = NULL;
      HM_HH_getConcurrentPack(parent)->ccstate = CC_DONE;

      hh->subHeapForCC = parent->subHeapForCC;
      hh->subHeapCompletedCC = parent;
      parent->subHeapForCC = NULL;

      hh->depth--;
      /* in this case, hh becomes the primary, so we store suspects in hh instead.
      */
      ES_move(HM_HH_getSuspects(hh), HM_HH_getSuspects(parent));

#if 0
      linkCCChains(s, hh, parent);

      /** update parent info so that it matches the invariants of a
        * "completed" subheap.
        */
      HM_HH_getConcurrentPack(parent)->ccstate = CC_DONE;
      parent->nextAncestor = NULL;

      /** Link parent into completed list. */
      parent->subHeapCompletedCC = hh->subHeapCompletedCC;
      hh->subHeapCompletedCC = parent;

      /** Do promotion of the main heap. */
      hh->depth--;
      hh->nextAncestor = nextAncestor;
#endif

      assert(lengthOfCCChain(hh) == len + lenParent);
      assert(lengthOfCompletedCCChain(hh) == lenC + lenCParent + 1);
    }

    assert(HM_HH_getDepth(hh) == currentDepth-1);
  }
  assert(hh == thread->hierarchicalHeap);

#if ASSERT
  assert(hh == thread->hierarchicalHeap);
  uint32_t newDepth = HM_HH_getDepth(hh);
  assert(newDepth < thread->currentDepth);
  assertCCChainInvariants(thread->hierarchicalHeap);
  assertInvariants(thread);
#endif
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
  HM_HH_getConcurrentPack(hh)->additionalStack = BOGUS_OBJPTR;
  HM_HH_getConcurrentPack(hh)->ccstate = CC_UNREG;
  HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection = 0;
  HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection = 0;

  // hh->representative = NULL;
  hh->ufNode = uf;
  hh->subHeapForCC = NULL;
  hh->subHeapCompletedCC = NULL;
  hh->depth = depth;
  hh->nextAncestor = NULL;
  // hh->dependant1 = NULL;
  // hh->dependant2 = NULL;
  hh->numDependants = 0;
  hh->heightDependants = 0;

  HM_initChunkList(HM_HH_getChunkList(hh));
  HM_initRemSet(HM_HH_getRemSet(hh));
  HM_initChunkList(HM_HH_getSuspects(hh));

  return hh;
}

uint32_t HM_HH_getDepth(HM_HierarchicalHeap hh)
{
  return hh->depth;
}

HM_HierarchicalHeap HM_HH_getHeapAtDepth(
  GC_state s,
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

  chunk = HM_allocateChunkWithPurpose(
    HM_HH_getChunkList(hh),
    bytesRequested,
    BLOCK_FOR_HEAP_CHUNK);

  if (NULL == chunk) {
    return FALSE;
  }

#ifdef DETECT_ENTANGLEMENT
  chunk->decheckState = thread->decheckState;
#else
  chunk->decheckState = DECHECK_BOGUS_TID;
#endif

  chunk->levelHead = HM_HH_getUFNode(hh);
  // hh->chunkList <--> og
  // toList --> hh
  // 1. in-place collection of unionFind nodes?
  // 2. How do you make the hh fully concurrent?
  // how do you make the union-find fully concurrent and collectible?
      // what is the hh?? list of heaps
          // ->
          // ->
          // ->
          // ->
  // 3.

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
  enter(s);

  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

#if 0
  if (Proc_isInitialized(s) /*&& !s->signalsInfo.amInSignalHandler*/) {
    // s->signalsInfo.amInSignalHandler = TRUE;
    int64_t astackSize = CheckActivationStack();
    if (astackSize > 0) {
      LOG(LM_THREAD, LL_INFO,
        "current activation stack size: %"PRId64,
        astackSize);
    }
    // s->signalsInfo.amInSignalHandler = FALSE;
  }
#endif

  if (s->limitPlusSlop < s->frontier) {
    DIE("s->limitPlusSlop (%p) < s->frontier (%p)",
        ((void*)(s->limitPlusSlop)),
        ((void*)(s->frontier)));
  }

  // JATIN_TODO: have a better bytesRequested here.
  size_t bytesRequested = GC_HEAP_LIMIT_SLOP;
  if (!HM_HH_extend (s, thread, bytesRequested)){
    assert(0);
  }

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
  leave(s);
}

void HM_HH_forceNewChunk(GC_state s) {
  enter(s);

  if (!HM_HH_extend(s, getThreadCurrent(s), GC_HEAP_LIMIT_SLOP)) {
    DIE("Ran out of space for new chunk");
  }

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  assert(invariantForMutatorFrontier(s));
  assert(invariantForMutatorStack(s));
  leave(s);
}

void splitHeapForCC(GC_state s, GC_thread thread) {
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  assert(HM_HH_isLevelHead(hh));
  assert(HM_HH_getConcurrentPack(hh)->ccstate == CC_REG);

  HM_HierarchicalHeap completed = hh->subHeapCompletedCC;
  if (NULL == completed) {
    assert(NULL == hh->subHeapForCC);
    completed = HM_HH_new(s, HM_HH_getDepth(hh));
    HM_HH_getConcurrentPack(completed)->ccstate = CC_DONE;
    hh->subHeapCompletedCC = completed;
  }

  HM_HierarchicalHeap newHH = HM_HH_new(s, HM_HH_getDepth(hh));
  thread->hierarchicalHeap = newHH;
  HM_chunk chunk = HM_allocateChunkWithPurpose(
    HM_HH_getChunkList(newHH),
    GC_HEAP_LIMIT_SLOP,
    BLOCK_FOR_HEAP_CHUNK);
    
  chunk->levelHead = HM_HH_getUFNode(newHH);

#ifdef DETECT_ENTANGLEMENT
  chunk->decheckState = thread->decheckState;
#else
  chunk->decheckState = DECHECK_BOGUS_TID;
#endif

  thread->currentChunk = chunk;
  newHH->subHeapForCC = hh;
  newHH->subHeapCompletedCC = completed;
  newHH->nextAncestor = hh->nextAncestor;
  hh->nextAncestor = NULL;
  ES_move(HM_HH_getSuspects(newHH), HM_HH_getSuspects(hh));
  assertCCChainInvariants(newHH);
}


void mergeCompletedCCs(GC_state s, HM_HierarchicalHeap hh) {
  // HM_HierarchicalHeap hh = thread->hierarchicalHeap;
#if ASSERT
  assert(HM_HH_isLevelHead(hh));
  assertCCChainInvariants(hh);
  size_t lenBefore = lengthOfCCChain(hh);
  size_t lenCBefore = lengthOfCompletedCCChain(hh);
#endif

  size_t numMerged = 0;
  size_t numInProgress = 0;

  HM_HierarchicalHeap *cursor = &(hh->subHeapForCC);
  HM_HierarchicalHeap subhh = *cursor;
  while (subhh != NULL) {
    assert(HM_HH_isLevelHead(subhh));
    assert(HM_HH_getDepth(hh) == HM_HH_getDepth(subhh));
    assert(NULL != subhh->subHeapCompletedCC);

    if (HM_HH_getConcurrentPack(subhh)->ccstate != CC_DONE) {
      // This CC still in progress. Skip.
      cursor = &(subhh->subHeapForCC);
      subhh = *cursor;
      numInProgress++;
      continue;
    }

    /** Otherwise, CC on this subheap is done. Put it in the "completed" chain,
      * to be merged as soon as all subheap CCs have completed.
      */
    HM_HierarchicalHeap next = subhh->subHeapForCC;
    *cursor = next;
    subhh->subHeapForCC = NULL;

    subhh->subHeapCompletedCC = hh->subHeapCompletedCC;
    hh->subHeapCompletedCC = subhh;
    // HM_HierarchicalHeap completed = hh->subHeapCompletedCC;
    // subhh->subHeapCompletedCC = completed->subHeapCompletedCC;
    // completed->subHeapCompletedCC = subhh;

    subhh = next;
    numMerged++;
  }

#if ASSERT
  assertCCChainInvariants(hh);
  size_t lenAfter = lengthOfCCChain(hh);
  size_t lenCAfter = lengthOfCompletedCCChain(hh);
  assert(numMerged + numInProgress == lenBefore);
  assert(numInProgress == lenAfter);
  assert(numMerged + lenCBefore == lenCAfter);
#endif

  /** All subheap CCs finished, so now we can merge the "completed" chain back
    * into main heap.
    */
  if (NULL == hh->subHeapForCC && NULL != hh->subHeapCompletedCC) {

    HM_HierarchicalHeap completed = hh->subHeapCompletedCC;
    while (completed != NULL) {
      HM_HierarchicalHeap next = completed->subHeapCompletedCC;
      
      /* consider using max instead of addition */
      HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection +=
        HM_HH_getConcurrentPack(completed)->bytesSurvivedLastCollection;
      
      /*
      HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection =
        max(HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection,
            HM_HH_getConcurrentPack(completed)->bytesSurvivedLastCollection);
      */

      CC_freeStack(s, HM_HH_getConcurrentPack(completed));
      linkInto(s, hh, completed);
      HM_appendChunkList(HM_HH_getChunkList(hh), HM_HH_getChunkList(completed));
      HM_appendRemSet(HM_HH_getRemSet(hh), HM_HH_getRemSet(completed));
      completed = next;
    }

    hh->subHeapCompletedCC = NULL;
  }

  if (numMerged > 0 || numInProgress > 0) {
    LOG(LM_CC_COLLECTION, LL_INFO,
      "depth %u: merged %zu finished CCs; still %zu in progress",
      HM_HH_getDepth(hh),
      numMerged,
      numInProgress);
  }

  assert(HM_HH_isLevelHead(hh));
  assertCCChainInvariants(hh);
  return;
}


bool checkPolicyforRoot(
  GC_state s,
  GC_thread thread)
{
  assert(NULL != thread->hierarchicalHeap);
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  assert(NULL != hh);

  if (HM_getChunkListSize(HM_HH_getChunkList(hh))
      < s->controls->hhConfig.minCCSize)
  {
    return FALSE;
  }

  HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection =
    HM_getChunkListSize(HM_HH_getChunkList(hh));

  size_t chainLen = 0;
  for (HM_HierarchicalHeap cursor = hh->subHeapForCC;
       NULL != cursor;
       cursor = cursor->subHeapForCC)
  {
    chainLen++;
    if (chainLen > s->controls->hhConfig.maxCCChainLength)
      return FALSE;
  }

  size_t bytesSurvived = HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection;
  
  /* consider removing this: */
  for (HM_HierarchicalHeap cursor = hh->subHeapCompletedCC;
       NULL != cursor;
       cursor = cursor->subHeapCompletedCC)
  {
    bytesSurvived +=
      HM_HH_getConcurrentPack(cursor)->bytesSurvivedLastCollection;
  }

  if((s->controls->hhConfig.ccThresholdRatio * bytesSurvived) >
      (HM_HH_getConcurrentPack(hh)->bytesAllocatedSinceLastCollection)
    || bytesSurvived == 0) {
    // if (!HM_HH_getConcurrentPack(hh)->shouldCollect) {
      // HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection/=2;
    // }
    HM_HH_getConcurrentPack(hh)->bytesSurvivedLastCollection +=4;
    return FALSE;
  }
  return TRUE;
}

/*
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
*/

objptr copyStackOfThread(GC_state s, GC_thread thread) {
  HM_HierarchicalHeap hh = getThreadCurrent(s)->hierarchicalHeap;
  GC_stack stackFrom = (GC_stack)objptrToPointer(thread->stack, NULL);

  size_t reserved = stackFrom->reserved;
  assert(isStackReservedAligned(s, reserved));
  size_t stackSize = sizeofStackWithMetaData(s, reserved);

  HM_chunk newChunk = HM_allocateChunkWithPurpose(
    HM_HH_getChunkList(hh),
    stackSize,
    BLOCK_FOR_HEAP_CHUNK);

  if (NULL == newChunk) {
    DIE("Ran out of space to copy stack!");
  }
  assert(stackSize < HM_getChunkSizePastFrontier(newChunk));
  newChunk->mightContainMultipleObjects = FALSE;
  newChunk->levelHead = HM_HH_getUFNode(hh);

#ifdef DETECT_ENTANGLEMENT
  newChunk->decheckState = getThreadCurrent(s)->decheckState;
#else
  newChunk->decheckState = DECHECK_BOGUS_TID;
#endif

  pointer frontier = HM_getChunkFrontier(newChunk);
  assert(frontier == HM_getChunkStart(newChunk));
  assert(GC_STACK_METADATA_SIZE == GC_HEADER_SIZE);
  *((GC_header*)frontier) = GC_STACK_HEADER;
  GC_stack stack = (GC_stack)(frontier + GC_HEADER_SIZE);
  stack->reserved = reserved;
  stack->used = 0;
  HM_updateChunkFrontierInList(
    HM_HH_getChunkList(hh),
    newChunk,
    frontier + stackSize);
  copyStack(s, stackFrom, stack);

  return pointerToObjptr((pointer)stack, NULL);
}

pointer HM_HH_getRoot(ARG_USED_FOR_ASSERT pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  enter(s);

  /** Superfluous argument to function... */
#if ASSERT
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(getThreadCurrent(s) == thread);
#endif

  /* SAM_NOTE: ensureHierarchicalHeapAssurances was tripping it's invariant
   * for the mutator frontier, due to (limit-frontier)<bytesNeeded. But
   * this runtime call doesn't need to ensure bytes free..
   *
   * TODO: ensureHierarchicalHeapAssurances should take a boolean, to indicate
   * whether or not it should check that invariant...?
   */
  size_t bytesRequested =
    max(GC_HEAP_LIMIT_SLOP, getThreadCurrent(s)->bytesNeeded);
  HM_ensureHierarchicalHeapAssurances(s, FALSE, bytesRequested, TRUE);

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  leave(s);
  return (void*)(getThreadCurrent(s)->hierarchicalHeap);
}

// ============================================================================

#if 0
void HM_HH_cancelCC(GC_state s, pointer threadp, pointer hhp) {
  HM_HierarchicalHeap heap = (HM_HierarchicalHeap)hhp;
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(getThreadCurrent(s) == thread);

  bool gotIt = claimHeap(heap);
#if ASSERT
  assert(gotIt);
#else
  ((void)gotIt);
#endif

  HM_HierarchicalHeap mainhh =
    HM_HH_getHeapAtDepth(s, thread, HM_HH_getDepth(heap));
  assert(mainhh->subHeapForCC == heap);

  CC_freeStack(HM_HH_getConcurrentPack(heap));

  pointer stackP = objptrToPointer(HM_HH_getConcurrentPack(heap)->stack, NULL);
  HM_chunk stackChunk = HM_getChunkOf(stackP);
  assert(HM_getLevelHead(stackChunk) == heap);
  assert(!(stackChunk->mightContainMultipleObjects));
  HM_unlinkChunk(HM_HH_getChunkList(HM_getLevelHead(stackChunk)), stackChunk);
  HM_freeChunk(s, stackChunk);
  HM_HH_getConcurrentPack(heap)->stack = BOGUS_OBJPTR;

  mainhh->subHeapForCC = heap->subHeapForCC;
  HM_appendChunkList(HM_HH_getChunkList(mainhh), HM_HH_getChunkList(heap));
  HM_appendRemSet(HM_HH_getRemSet(mainhh), HM_HH_getRemSet(heap));
  linkInto(s, mainhh, heap);


  /** preserve annoying subHeapForCC and subHeapCompletedCC chain invariants */
  if (NULL == mainhh->subHeapForCC && NULL != mainhh->subHeapCompletedCC) {

    HM_HierarchicalHeap completed = mainhh->subHeapCompletedCC;
    while (completed != NULL) {
      HM_HierarchicalHeap next = completed->subHeapCompletedCC;
      HM_HH_getConcurrentPack(mainhh)->bytesSurvivedLastCollection +=
        HM_HH_getConcurrentPack(completed)->bytesSurvivedLastCollection;
      HM_appendChunkList(HM_HH_getChunkList(mainhh), HM_HH_getChunkList(completed));
      HM_appendRemSet(HM_HH_getRemSet(mainhh), HM_HH_getRemSet(completed));
      linkInto(s, mainhh, completed);
      completed = next;
    }

    mainhh->subHeapCompletedCC = NULL;
  }

  assertCCChainInvariants(mainhh);
  return;
}
#endif

void HM_HH_cancelCC(GC_state s, pointer threadp, pointer hhp) {
  (void)s;
  (void)threadp;
  (void)hhp;
  DIE("HM_HH_cancelCC deprecated");
}


// =============================================================================

Bool HM_HH_registerCont(pointer kl, pointer kr, pointer k, pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  enter(s);

  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  // So, we should really get rid of the extra argument.
  assert(thread == getThreadCurrent(s));

  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  CC_initStack(s, HM_HH_getConcurrentPack(hh));

  mergeCompletedCCs(s, hh);
  assert(thread->hierarchicalHeap == hh);

#if ASSERT
  if (NULL == thread->hierarchicalHeap->subHeapForCC)
    assert(NULL == thread->hierarchicalHeap->subHeapCompletedCC);
#endif

  if (!checkPolicyforRoot(s, thread)) {
    HM_HierarchicalHeap heap = thread->hierarchicalHeap;
    uint32_t depth = heap->depth;
    size_t lastSurvived = heap->concurrentPack.bytesSurvivedLastCollection;
    size_t freshAlloc = heap->concurrentPack.bytesAllocatedSinceLastCollection;
    size_t size = HM_getChunkListSize(HM_HH_getChunkList(heap));
    LOG(LM_CC_COLLECTION, LL_DEBUG,
      "skipping at depth %u. last-survived: %zu new-alloc: %zu size: %zu",
      depth,
      lastSurvived,
      freshAlloc,
      size);

    leave(s);
    return FALSE;
  }

  assert(HM_getLevelHead(HM_getChunkOf(kl)) == hh);
  assert(HM_getLevelHead(HM_getChunkOf(kr)) == hh);
  assert(HM_getLevelHead(HM_getChunkOf(k)) == hh);
  assert(HM_HH_getConcurrentPack(hh) != NULL);

  HM_HH_getConcurrentPack(hh)->snapLeft = pointerToObjptr(kl, NULL);
  HM_HH_getConcurrentPack(hh)->snapRight = pointerToObjptr(kr, NULL);
  HM_HH_getConcurrentPack(hh)->snapTemp = pointerToObjptr(k, NULL);
  objptr snapstack = copyStackOfThread(s, thread);
  HM_HH_getConcurrentPack(hh)->stack = snapstack;

  objptr additionalStack = BOGUS_OBJPTR;
  if (s->savedThreadDuringSignalHandler != BOGUS_OBJPTR) {
    additionalStack = copyStackOfThread(s,
      threadObjptrToStruct(s, s->savedThreadDuringSignalHandler));
    HM_HH_getConcurrentPack(hh)->additionalStack = additionalStack;
  }

#if ASSERT
  HM_assertChunkListInvariants(HM_HH_getChunkList(hh));
  pointer snapstackp = objptrToPointer(snapstack, NULL);
  assert(listContainsChunk(HM_HH_getChunkList(hh), HM_getChunkOf(snapstackp)));
  if (additionalStack != BOGUS_OBJPTR) {
    assert(listContainsChunk(HM_HH_getChunkList(hh),
      HM_getChunkOf(objptrToPointer(additionalStack, NULL))));
  }
#endif

  CC_clearStack(s, HM_HH_getConcurrentPack(hh));
  assert(HM_HH_getConcurrentPack(hh)->ccstate == CC_UNREG);
  __atomic_store_n(&(HM_HH_getConcurrentPack(hh)->ccstate), CC_REG, __ATOMIC_SEQ_CST);
  // HM_HH_getConcurrentPack(hh)->ccstate = CC_REG;

  splitHeapForCC(s, thread);
  CC_initStack(s, HM_HH_getConcurrentPack(thread->hierarchicalHeap));
  assert(thread->hierarchicalHeap->subHeapForCC == hh);

  HM_assertChunkListInvariants(HM_HH_getChunkList(hh));
  assert(listContainsChunk(HM_HH_getChunkList(hh), HM_getChunkOf(snapstackp)));

  s->frontier = HM_HH_getFrontier(thread);
  s->limitPlusSlop = HM_HH_getLimit(thread);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));

  LOG(LM_CC_COLLECTION, LL_INFO,
    "registered CC for heap %p at depth %u",
    (void*)hh,
    HM_HH_getDepth(hh));

  leave(s);
  return TRUE;
}

HM_HierarchicalHeap HM_HH_getCurrent(GC_state s) {
  return getThreadCurrent(s)->hierarchicalHeap;
}

pointer HM_HH_getFrontier(GC_thread thread) {
  pointer result = HM_getChunkFrontier(thread->currentChunk);
  assert(inFirstBlockOfChunk(thread->currentChunk, result)
         || HM_getChunkLimit(thread->currentChunk) == result);
  return result;
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

void HM_HH_addRootForCollector(GC_state s, HM_HierarchicalHeap hh, pointer p) {
  assert(hh!=NULL);
  if(HM_HH_getConcurrentPack(hh)!=NULL){
    CC_addToStack(s, HM_HH_getConcurrentPack(hh), p);
  }
}

void HM_HH_rememberAtLevel(HM_HierarchicalHeap hh, HM_remembered remElem, bool conc) {
  assert(hh != NULL);
  if (!conc) {
    HM_remember(HM_HH_getRemSet(hh), remElem, conc);
  } else {
    HM_UnionFindNode cursor = HM_HH_getUFNode(hh);
    while(true) {
      while (NULL != cursor->representative) {
        cursor = cursor->representative;
      }
      hh = cursor->payload;
      if (hh == NULL){
        /* race with a join that changed the cursor, iterate again */
        /*should not happen if we retire hh just like ufnodes*/
        assert (false);
        continue;
      }
      HM_remember(HM_HH_getRemSet(hh), remElem, conc);
      if (NULL == cursor->representative) {
        return;
      }
    }
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
  __attribute__((unused)) GC_state s,
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

  // HM_HH_getUFNode(right)->payload = NULL;
  // freeFixedSize(getHHAllocator(s), right);
  // HH_EBR_retire(s, HM_HH_getUFNode(right));

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
      assert(HM_getLevelHead(chunk) == cursor);    }
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
