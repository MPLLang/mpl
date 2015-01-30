/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* newObject (s, header, bytesRequested, allocInOldGen)
 *
 * Allocate a new object in the heap.
 * bytesRequested includes the size of the header.
 */
/* XXX DOC spoons must hold the runtime lock if allocInOldGen is true! */
pointer newObject (GC_state s,
                   GC_header header,
                   size_t bytesRequested,
                   bool allocInOldGen) {
  pointer frontier;
  pointer result;

  assert (isAligned (bytesRequested, s->alignment));
  assert (allocInOldGen
          ? hasHeapBytesFree (s, bytesRequested, 0)
          : hasHeapBytesFree (s, 0, bytesRequested));
  if (allocInOldGen) {
    /* NB you must have exclusive access to the runtime state
       if you are allocating in the older generation! */
    frontier = s->heap->start + s->heap->oldGenSize;
    s->heap->oldGenSize += bytesRequested;
    s->cumulativeStatistics->bytesAllocated += bytesRequested;
  } else {
    if (DEBUG_DETAILED)
      fprintf (stderr, "frontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->frontier,
               (uintptr_t)(s->frontier + bytesRequested));
    frontier = s->frontier;
    s->frontier += bytesRequested;
  }
  /* XXX unprotected concurrent access */
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)frontier) = header;
  result = frontier + GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((size_t)result, s->alignment));
  if (DEBUG)
    fprintf (stderr, FMTPTR " = newObject ("FMTHDR", %"PRIuMAX", %s)\n",
             (uintptr_t)result,
             header,
             (uintmax_t)bytesRequested,
             boolToString (allocInOldGen));
  return result;
}

GC_stack newStack (GC_state s,
                   size_t reserved,
                   bool allocInOldGen) {
  GC_stack stack;

#pragma message "Figure out a way to put this in the hierarchical heaps"
  HM_enterGlobalHeap(FALSE);

  assert (isStackReservedAligned (s, reserved));
  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;
  stack = (GC_stack)(newObject (s, GC_STACK_HEADER,
                                sizeofStackWithHeader (s, reserved),
                                allocInOldGen));
  stack->reserved = reserved;
  stack->used = 0;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%"PRIuMAX")\n",
             (uintptr_t)stack,
             (uintmax_t)reserved);

  HM_exitGlobalHeap(FALSE);

  return stack;
}

GC_thread newThread (GC_state s, size_t reserved) {
  GC_stack stack;
  GC_thread thread;
  pointer res;

  HM_enterGlobalHeap(FALSE);

  assert (isStackReservedAligned (s, reserved));
  ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, FALSE, FALSE, 0, sizeofStackWithHeader (s, reserved) + sizeofThread (s));
  stack = newStack (s, reserved, FALSE);
  res = newObject (s, GC_THREAD_HEADER,
                   sizeofThread (s),
                   FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->inGlobalHeapCounter = 1;
  thread->stack = pointerToObjptr((pointer)stack, s->heap->start);
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%"PRIuMAX")\n",
             (uintptr_t)thread, (uintmax_t)reserved);;

  HM_exitGlobalHeap(FALSE);

  return thread;
}

pointer HM_newHierarchicalHeap (GC_state s) {
  /* allocate the object */
  ensureHasHeapBytesFreeAndOrInvariantForMutator (
      s, FALSE, FALSE, FALSE, 0, HM_sizeofHierarchicalHeap (s));
  pointer hhObject = newObject (s,
                                GC_HIERARCHICAL_HEAP_HEADER,
                                HM_sizeofHierarchicalHeap (s),
                                FALSE);
  struct HM_HierarchicalHeap* hh =
      ((struct HM_HierarchicalHeap*)(hhObject +
                                     HM_offsetofHierarchicalHeap (s)));

  /* initialize the object with a small chunk */
  hh->lastAllocatedChunk = NULL;
  hh->savedFrontier = NULL;
  hh->chunkList = NULL;
  hh->parentHH = BOGUS_OBJPTR;
  hh->nextChildHH = BOGUS_OBJPTR;
  hh->childHHList = BOGUS_OBJPTR;
  if (!HM_extendHierarchicalHeap(hh, GC_HEAP_LIMIT_SLOP)) {
    die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
  }

  if (DEBUG_HEAP_MANAGEMENT) {
    fprintf (stderr, "%p = newHierarchicalHeap ()\n", ((void*)(hh)));
  }

  return hhObject;
}

static inline void setFrontier (GC_state s, pointer p,
                                ARG_USED_FOR_ASSERT size_t bytes) {
  p = alignFrontier (s, p);
  assert ((size_t)(p - s->frontier) <= bytes);
  GC_profileAllocInc (s, (size_t)(p - s->frontier));
  /* SPOONHOWER_NOTE: unsafe concurrent access */
  s->cumulativeStatistics->bytesAllocated += (size_t)(p - s->frontier);
  s->frontier = p;
  assert (s->frontier <= s->limitPlusSlop);
  assert (s->start <= s->frontier);
}
