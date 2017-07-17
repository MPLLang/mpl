/* Copyright (C) 2011-2012 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT
void assertIsObjptrInFromSpace (GC_state s, objptr *opp, void* ignored) {
  /* silence compiler warning */
  ((void)(ignored));

  assert (isObjptrInFromSpace (s, *opp));
  unless (isObjptrInFromSpace (s, *opp))
    die ("gc.c: assertIsObjptrInFromSpace "
         "opp = "FMTPTR"  "
         "*opp = "FMTOBJPTR"\n",
         (uintptr_t)opp, *opp);
  /* The following checks that intergenerational pointers have the
   * appropriate card marked.  Unfortunately, it doesn't work because
   * for stacks, the card containing the beginning of the stack is
   * marked, but any remaining cards aren't.
   */
  if (FALSE and s->mutatorMarksCards
      and isPointerInOldGen (s, (pointer)opp)
      and isObjptrInNursery (s, *opp)
      and not isCardMarked (s, (pointer)opp)) {
    displayGCState (s, stderr);
    die ("gc.c: intergenerational pointer from "FMTPTR" to "FMTOBJPTR" with unmarked card.\n",
         (uintptr_t)opp, *opp);
  }
}

static inline void assertIsObjptrReachable (GC_state s,
                                            objptr *opp,
                                            void* ignored) {
  /* silence compiler warning */
  ((void)(ignored));

  assert (isObjptrInFromSpace (s, *opp) ||
          HM_HH_objptrInHierarchicalHeap(s, *opp));

  if (!isObjptrInFromSpace (s, *opp) &&
      !HM_HH_objptrInHierarchicalHeap(s, *opp)) {
    die ("gc.c: assertIsObjptrReachable "
         "opp = "FMTPTR"  "
         "*opp = "FMTOBJPTR"\n",
         (uintptr_t)opp, *opp);
  }
}

bool invariantForGC (GC_state s) {
  uint32_t proc;
  if (DEBUG)
    fprintf (stderr, "invariantForGC\n");
  /* Frame layouts */
  for (unsigned int i = 0; i < s->frameLayoutsLength; ++i) {
    GC_frameLayout layout;

    layout = &(s->frameLayouts[i]);
    if (layout->size > 0) {
      GC_frameOffsets offsets;

      assert (layout->size <= s->maxFrameSize);
      offsets = layout->offsets;
      for (unsigned int j = 0; j < offsets[0]; ++j)
        assert (offsets[j + 1] < layout->size);
    }
  }
  /* Generational */
  if (s->mutatorMarksCards) {
    assert (s->generationalMaps.cardMap ==
            &(s->generationalMaps.cardMapAbsolute
              [pointerToCardMapIndexAbsolute(s->heap->start)]));
    assert (&(s->generationalMaps.cardMapAbsolute
              [pointerToCardMapIndexAbsolute(s->heap->start + s->heap->size - 1)])
            < (s->generationalMaps.cardMap
               + (s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE)));
  }
  assert (isAligned (s->heap->size, s->sysvals.pageSize));
  assert (isAligned ((size_t)s->heap->start, CARD_SIZE));
  assert (isFrontierAligned (s, s->heap->start + s->heap->oldGenSize));
  assert (isFrontierAligned (s, s->heap->nursery));
  assert (isFrontierAligned (s, s->frontier));
  assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
  assert (s->heap->nursery <= s->heap->start + s->heap->size);
  assert (s->heap->nursery <= s->heap->start + s->heap->availableSize);
  assert (s->heap->nursery <= s->frontier or 0 == s->frontier);
  assert (s->heap->availableSize <= s->heap->size);
  assert (s->start <= s->frontier);
  unless (0 == s->heap->size or 0 == s->frontier) {
    assert (s->frontier <= s->limitPlusSlop);
    assert (s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
    assert (hasHeapBytesFree (s, 0, 0));
  }
  assert (s->secondaryHeap->start == NULL
          or s->heap->size == s->secondaryHeap->size);
  /* Check that all pointers are into from space. */
  foreachGlobalObjptr (s, assertIsObjptrInFromSpace, NULL);
  pointer back = s->heap->start + s->heap->oldGenSize;
  if (DEBUG_DETAILED)
    fprintf (stderr, "Checking old generation.\n");
  foreachObjptrInRange (s,
                        alignFrontier (s, s->heap->start),
                        &back,
                        FALSE,
                        NULL,
                        trueObjptrPredicate,
                        NULL,
                        assertIsObjptrReachable,
                        NULL);
  if (DEBUG_DETAILED)
    fprintf (stderr, "Checking nursery.\n");
  if (s->procStates) {
    pointer firstStart = s->heap->frontier;
    for (proc = 0; proc < s->numberOfProcs; proc++) {
      foreachObjptrInRange (s,
                            s->procStates[proc].start,
                            &s->procStates[proc].frontier,
                            FALSE,
                            NULL,
                            trueObjptrPredicate,
                            NULL,
                            assertIsObjptrReachable,
                            NULL);
      if (s->procStates[proc].start
          and s->procStates[proc].start < firstStart)
        firstStart = s->procStates[proc].start;
    }
    foreachObjptrInRange (s,
                          s->heap->nursery,
                          &firstStart,
                          FALSE,
                          NULL,
                          trueObjptrPredicate,
                          NULL,
                          assertIsObjptrReachable,
                          NULL);
  }
  else {
    foreachObjptrInRange (s,
                          s->start,
                          &s->frontier,
                          FALSE,
                          NULL,
                          trueObjptrPredicate,
                          NULL,
                          assertIsObjptrReachable,
                          NULL);
  }
  /* Current thread. */
  GC_stack stack = getStackCurrent(s);
  assert (isStackReservedAligned (s, stack->reserved));
  assert (s->stackBottom == getStackBottom (s, stack));
  assert (s->stackTop == getStackTop (s, stack));
  assert (s->stackLimit == getStackLimit (s, stack));
  assert (s->stackBottom <= s->stackTop);
  assert (stack->used == sizeofGCStateCurrentStackUsed (s));
  assert (stack->used <= stack->reserved);
  if (DEBUG)
    fprintf (stderr, "invariantForGC passed\n");
  return TRUE;
}
#endif

bool invariantForMutatorFrontier (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return (thread->bytesNeeded
          <= (size_t)(s->limitPlusSlop - s->frontier));
}

bool invariantForMutatorStack (GC_state s) {
  GC_stack stack = getStackCurrent(s);
  return (getStackTop (s, stack)
          <= getStackLimit (s, stack) + getStackTopFrameSize (s, stack));
}

#if ASSERT
bool invariantForMutator (GC_state s, bool frontier, bool stack) {
  if (DEBUG)
    displayGCState (s, stderr);
  if (frontier)
    assert (invariantForMutatorFrontier(s));
  if (stack)
    assert (invariantForMutatorStack(s));
  assert (invariantForGC (s));
  return TRUE;
}
#endif
