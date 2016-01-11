/* Copyright (C) 2009,2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayGCState (GC_state s, FILE *stream) {
  fprintf (stream,
           "GC state\n");
  fprintf (stream, "\tcurrentThread = "FMTOBJPTR"\n", s->currentThread);
  displayThread (s, (GC_thread)(objptrToPointer (s->currentThread, s->heap->start)
                                + offsetofThread (s)),
                 stream);
  fprintf (stream, "\tgenerational\n");
  displayGenerationalMaps (s, &s->generationalMaps,
                           stream);
  fprintf (stream, "\theap\n");
  displayHeap (s, s->heap,
               stream);
  fprintf (stream,
           "\tstart = "FMTPTR"\n"
           "\tfrontier = "FMTPTR"\n"
           "\tlimit = "FMTPTR"\n"
           "\tlimitPlusSlop = "FMTPTR"\n"
           "\tstackBottom = "FMTPTR"\n"
           "\tstackTop = "FMTPTR"\n",
           (uintptr_t)s->start,
           (uintptr_t)s->frontier,
           (uintptr_t)s->limit,
           (uintptr_t)s->limitPlusSlop,
           (uintptr_t)s->stackBottom,
           (uintptr_t)s->stackTop);
}

size_t sizeofGCStateCurrentStackUsed (GC_state s) {
  return (size_t)(s->stackTop - s->stackBottom);
}

void setGCStateCurrentThreadAndStack (GC_state s) {
  GC_thread thread;
  GC_stack stack;

  thread = getThreadCurrent (s);
  s->exnStack = thread->exnStack;
  stack = getStackCurrent (s);
  s->stackBottom = getStackBottom (s, stack);
  s->stackTop = getStackTop (s, stack);
  s->stackLimit = getStackLimit (s, stack);
  markCard (s, (pointer)stack);
}

void setGCStateCurrentHeap (GC_state s,
                            size_t oldGenBytesRequested,
                            size_t nurseryBytesRequested,
                            bool duringInit) {
  GC_heap h;
  pointer nursery;
  size_t nurserySize;
  pointer genNursery;
  size_t genNurserySize;
  pointer limit;
  pointer frontier;
  size_t bonus = GC_BONUS_SLOP * s->numberOfProcs;

  if (not duringInit) {
    nurseryBytesRequested = 0;
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      GC_thread thread = getThreadCurrent(&s->procStates[proc]);
      if (thread)
        nurseryBytesRequested += thread->bytesNeeded;
    }
  }

  if (DEBUG_DETAILED)
    fprintf (stderr, "setGCStateCurrentHeap(%s, %s)\n",
             uintmaxToCommaString(oldGenBytesRequested),
             uintmaxToCommaString(nurseryBytesRequested));
  h = s->heap;
  assert (isFrontierAligned (s, h->start + h->oldGenSize + oldGenBytesRequested));
#warning What happens to s->limit{,PlusSlop}?
  limit = h->start + h->size - bonus;
  nurserySize = h->size - (h->oldGenSize + oldGenBytesRequested) - bonus;
  assert (isFrontierAligned (s, limit - nurserySize));
  nursery = limit - nurserySize;
  genNursery = alignFrontier (s, limit - (nurserySize / 2));
  genNurserySize = limit - genNursery;
  if (/* The mutator marks cards. */
      s->mutatorMarksCards
      /* There is enough space in the generational nursery. */
      and (nurseryBytesRequested <= genNurserySize)
      /* The nursery is large enough to be worth it. */
      and (((float)(h->size - s->lastMajorStatistics->bytesLive)
            / (float)nurserySize)
           <= s->controls->ratios.nursery)
      and /* There is a reason to use generational GC. */
      (
       /* We must use it for debugging purposes. */
       FORCE_GENERATIONAL
       /* We just did a mark compact, so it will be advantageous to to use it. */
       or (s->lastMajorStatistics->kind == GC_MARK_COMPACT)
       /* The live ratio is low enough to make it worthwhile. */
       or ((float)h->size / (float)s->lastMajorStatistics->bytesLive
           <= (h->withMapsSize < s->sysvals.ram
               ? s->controls->ratios.copyGenerational
               : s->controls->ratios.markCompactGenerational))
       )) {
    s->canMinor = TRUE;
    nursery = genNursery;
    nurserySize = genNurserySize;
    clearCardMap (s);
    /* XXX copy card map to other processors? */
  } else {
    unless (nurseryBytesRequested <= nurserySize)
      die ("Out of memory.  Insufficient space in nursery.");
    s->canMinor = FALSE;
  }

#warning What does this do?
  if (s->controls->restrictAvailableSize
      and
      (s->cumulativeStatistics->maxBytesLiveSinceReset > 0)) {
    float actualRatio;
    h->availableSize =
      (size_t)(s->controls->ratios.available
               * s->cumulativeStatistics->maxBytesLiveSinceReset);

    if ((h->oldGenSize + oldGenBytesRequested + nurserySize + bonus)
        > h->availableSize) {
      /* Limit allocation in this round */
      if ((h->oldGenSize + oldGenBytesRequested + nurseryBytesRequested + bonus)
          > h->availableSize) {
        /* We can't limit as much as we'd like, so offer enough space to
           satisfy the current request. */
        h->availableSize = h->oldGenSize + oldGenBytesRequested
          + nurseryBytesRequested + bonus;
      }
      if (h->availableSize > h->size) {
        /* Can't offer more than we have. */
        h->availableSize = h->size;
      }
      limit = h->start + h->availableSize - bonus;
      nurserySize = h->availableSize - (h->oldGenSize + oldGenBytesRequested) - bonus;
      assert (isFrontierAligned (s, limit - nurserySize));
      nursery = limit - nurserySize;

      if (s->canMinor) {
        /* If we are planning for a minor collection, we must also adjust the
           start of the nursery */
        nursery = alignFrontier (s, limit - (nurserySize / 2));
        nurserySize = limit - nursery;
      }
      if (DEBUG) {
        fprintf (stderr,
                 "[GC: Restricted nursery at "FMTPTR" of %s bytes (%.1f%%).]\n",
                 (uintptr_t)nursery, uintmaxToCommaString(limit - nursery),
                 100.0 * ((double)(limit - nursery)
                          / (double)h->availableSize));
      }
    }
    else {
      /* No need to limit in this round... reset availableSize. */
      h->availableSize = h->size;
    }

    actualRatio = (float)h->availableSize
      / s->cumulativeStatistics->maxBytesLiveSinceReset;
    if ((DEBUG or s->controls->messages)
        and
        (actualRatio > s->controls->ratios.available)) {
      fprintf (stderr,
               "[GC: Can't restrict available ratio to %f, using %f; worst-case max-live is %s bytes.]\n",
               s->controls->ratios.available, actualRatio,
               uintmaxToCommaString(h->oldGenSize + oldGenBytesRequested + nurserySize));
    }
  }
  else {
    /* Otherwise, make all unused space available */
    h->availableSize = h->size;
  }

  assert (nurseryBytesRequested <= nurserySize);
  s->heap->nursery = nursery;
  frontier = nursery;

  if (not duringInit) {
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      s->procStates[proc].canMinor = s->canMinor;
      assert (isFrontierAligned (s, frontier));
      s->procStates[proc].start = s->procStates[proc].frontier = frontier;
      s->procStates[proc].limitPlusSlop = s->procStates[proc].start +
        getThreadCurrent(&s->procStates[proc])->bytesNeeded;
      s->procStates[proc].limit = s->procStates[proc].limitPlusSlop - GC_HEAP_LIMIT_SLOP;
      assert (s->procStates[proc].frontier <= s->procStates[proc].limitPlusSlop);
#warning Probably not necessary, remove after confirmation
      /* XXX clearCardMap (?) */

#warning Might want to remove this after cleanup
      if (DEBUG)
        for (size_t i = 0; i < GC_BONUS_SLOP; i++)
          *(s->procStates[proc].limitPlusSlop + i) = 0xBF;

      frontier = s->procStates[proc].limitPlusSlop + GC_BONUS_SLOP;
    }
  }
  else {
    assert (Proc_processorNumber (s) == 0);
    /* SPOONHOWER_NOTE: this is a lot of copy-paste */
    for (int proc = 1; proc < s->numberOfProcs; proc++) {
      s->procStates[proc].canMinor = s->canMinor;
      assert (isFrontierAligned (s, frontier));
      s->procStates[proc].start = s->procStates[proc].frontier = frontier;
      s->procStates[proc].limitPlusSlop = s->procStates[proc].start +
        GC_HEAP_LIMIT_SLOP;
      s->procStates[proc].limit = s->procStates[proc].limitPlusSlop - GC_HEAP_LIMIT_SLOP;
      assert (s->procStates[proc].frontier <= s->procStates[proc].limitPlusSlop);
#warning Probably not necessary, remove after confirmation
      /* XXX clearCardMap (?) */

#warning Might want to remove this after cleanup
      if (DEBUG)
        for (size_t i = 0; i < GC_BONUS_SLOP; i++)
          *(s->procStates[proc].limitPlusSlop + i) = 0xBF;

      frontier = s->procStates[proc].limitPlusSlop + GC_BONUS_SLOP;
    }

    s->start = s->frontier = frontier;
    s->limitPlusSlop = limit;
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
#warning Probably not necessary, remove after confirmation
    /* XXX clearCardMap (?) */

    if (DEBUG)
      for (size_t i = 0; i < GC_BONUS_SLOP; i++)
        *(s->limitPlusSlop + i) = 0xBF;

    frontier = s->limitPlusSlop + GC_BONUS_SLOP;
  }
  h->frontier = frontier;
  assert (h->frontier <= h->start + h->availableSize);

  if (not duringInit) {
    assert (getThreadCurrent(s)->bytesNeeded <= (size_t)(s->limitPlusSlop - s->frontier));
    assert (hasHeapBytesFree (s, oldGenBytesRequested, getThreadCurrent(s)->bytesNeeded));
  }
  else {
    assert (nurseryBytesRequested <= (size_t)(s->limitPlusSlop - s->frontier));
    assert (hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
  }
  assert (isFrontierAligned (s, s->frontier));
}

bool GC_getAmOriginal (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->amOriginal;
}
void GC_setAmOriginal (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->amOriginal = b;
}

void GC_setControlsMessages (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->messages = b;
}

void GC_setControlsSummary (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->summary = b;
}

void GC_setControlsRusageMeasureGC (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->rusageMeasureGC = b;
}

uintmax_t GC_getCumulativeStatisticsBytesAllocated (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->bytesAllocated;
}

uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numCopyingGCs;
}

uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numMarkCompactGCs;
}

uintmax_t GC_getCumulativeStatisticsNumMinorGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numMinorGCs;
}

size_t GC_getCumulativeStatisticsMaxBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->maxBytesLive;
}

void GC_setHashConsDuringGC (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->hashConsDuringGC = b;
}

size_t GC_getLastMajorStatisticsBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->lastMajorStatistics->bytesLive;
}

pointer GC_getCallFromCHandlerThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->callFromCHandlerThread, s->heap->start);
  return p;
}

void GC_setCallFromCHandlerThreads (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(getArrayLength (p) == s->numberOfProcs);
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].callFromCHandlerThread = ((objptr*)p)[proc];
  }
}

pointer GC_getCurrentThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->currentThread, s->heap->start);
  return p;
}

pointer GC_getSavedThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p;

  assert(s->savedThread != BOGUS_OBJPTR);
  p = objptrToPointer (s->savedThread, s->heap->start);
  s->savedThread = BOGUS_OBJPTR;
  return p;
}

void GC_setSavedThread (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  objptr op;

  assert(s->savedThread == BOGUS_OBJPTR);
  op = pointerToObjptr (p, s->heap->start);
  s->savedThread = op;
}

void GC_setSignalHandlerThreads (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(getArrayLength (p) == s->numberOfProcs);
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].signalHandlerThread = ((objptr*)p)[proc];
  }
}

struct rusage* GC_getRusageGCAddr (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->cumulativeStatistics->ru_gc);
}

// Signal disposition is per-process; use primary to maintain handled set.
sigset_t* GC_getSignalsHandledAddr (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->procStates[0].signalsInfo.signalsHandled);
}

sigset_t* GC_getSignalsPendingAddr (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->signalsInfo.signalsPending);
}

// Signal disposition is per-process; use primary to maintain handled set.
void GC_setGCSignalHandled (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->procStates[0].signalsInfo.gcSignalHandled = b;
}

bool GC_getGCSignalPending (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (s->signalsInfo.gcSignalPending);
}

void GC_setGCSignalPending (bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->signalsInfo.gcSignalPending = b;
}
