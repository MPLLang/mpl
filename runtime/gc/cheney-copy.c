/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

void updateWeaksForCheneyCopy (GC_state s) {
  pointer p;
  GC_weak w;

  for (w = s->weaks; w != NULL; w = w->link) {
    assert (BOGUS_OBJPTR != w->objptr);

    if (DEBUG_WEAK)
      fprintf (stderr, "updateWeaksForCheneyCopy  w = "FMTPTR"  ", (uintptr_t)w);
    p = objptrToPointer (w->objptr, s->heap->start);
    if (hasFwdPtr(p)) {
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarded from "FMTOBJPTR" to "FMTOBJPTR"\n",
                 w->objptr, getFwdPtr(p));
      w->objptr = getFwdPtr(p);
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared\n");
      *(getHeaderp((pointer)w - offsetofWeak (s))) = GC_WEAK_GONE_HEADER;
      w->objptr = BOGUS_OBJPTR;
    }
  }
  s->weaks = NULL;
}

void swapHeapsForCheneyCopy (GC_state s) {
  GC_heap tempHeap;

  for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
    tempHeap = s->procStates[proc].secondaryHeap;
    s->procStates[proc].secondaryHeap = s->procStates[proc].heap;
    s->procStates[proc].heap = tempHeap;
    setCardMapAndCrossMap (&s->procStates[proc]);
  }
}

void majorCheneyCopyGC (GC_state s) {
  size_t bytesCopied;
  struct rusage ru_start;
  pointer toStart;

  assert (s->secondaryHeap->size >= s->heap->oldGenSize);
  if (detailedGCTime (s))
    startTiming (RUSAGE_THREAD, &ru_start);
  s->cumulativeStatistics->numCopyingGCs++;
  s->forwardState.amInMinorGC = FALSE;
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Starting major Cheney-copy;]\n");
    fprintf (stderr,
             "[GC:\tfrom heap at "FMTPTR" of size %s bytes,]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
    fprintf (stderr,
             "[GC:\tto heap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->secondaryHeap->start),
             uintmaxToCommaString(s->secondaryHeap->size));
  }
  s->forwardState.toStart = s->secondaryHeap->start;
  s->forwardState.toLimit = s->secondaryHeap->start + s->secondaryHeap->size;
  assert (s->secondaryHeap->start != (pointer)NULL);
  /* The next assert ensures there is enough space for the copy to
   * succeed.  It does not assert
   *   (s->secondaryHeap->size >= s->heap->size)
   * because that is too strong.
   */
  assert (s->secondaryHeap->size >= s->heap->oldGenSize);
  toStart = alignFrontier (s, s->secondaryHeap->start);
  s->forwardState.back = toStart;
  foreachGlobalObjptr (s, forwardObjptr, NULL);
  foreachObjptrInRange (s,
                        toStart,
                        &s->forwardState.back,
                        TRUE,
                        NULL,
                        trueObjptrPredicate,
                        NULL,
                        forwardObjptr,
                        NULL);
  updateWeaksForCheneyCopy (s);
  s->secondaryHeap->oldGenSize = (size_t)(s->forwardState.back - s->secondaryHeap->start);
  bytesCopied = s->secondaryHeap->oldGenSize;
  s->cumulativeStatistics->bytesCopied += bytesCopied;
  swapHeapsForCheneyCopy (s);
  s->lastMajorStatistics->kind = GC_COPYING;
  if (detailedGCTime (s))
    stopTiming (RUSAGE_THREAD, &ru_start, &s->cumulativeStatistics->ru_gcCopying);
  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Finished major Cheney-copy; copied %s bytes.]\n",
             uintmaxToCommaString(bytesCopied));
}

/* ---------------------------------------------------------------- */
/*                 Minor Cheney Copying Collection                  */
/* ---------------------------------------------------------------- */

void minorCheneyCopyGC (GC_state s) {
  size_t bytesAllocated;
  size_t bytesFilled = 0;
  size_t bytesCopied;
  struct rusage ru_start;

  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "minorGC  nursery = "FMTPTR"  frontier = "FMTPTR"\n",
             (uintptr_t)s->heap->nursery, (uintptr_t)s->frontier);
  assert (invariantForGC (s));
  bytesAllocated = (size_t)(s->heap->frontier - s->heap->nursery);
  if (bytesAllocated == 0)
    return;
  s->cumulativeStatistics->bytesAllocated += bytesAllocated;
  if (not s->canMinor) {
    for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
      /* Add in the bonus slop now since we need to fill it */
      s->procStates[proc].limitPlusSlop += GC_BONUS_SLOP;
      if (s->procStates[proc].limitPlusSlop != s->heap->frontier) {
        /* Fill to avoid an uninitialized gap in the middle of the heap */
        bytesFilled += fillGap (s->procStates[proc].frontier,
                                s->procStates[proc].limitPlusSlop);
      }
      else {
        /* If this is at the end of the heap there is no need to fill the gap
         -- there will be no break in the initialized portion of the
         heap.  Also, this is the last chunk allocated in the nursery, so it is
         safe to use the frontier from this processor as the global frontier.  */
        s->heap->oldGenSize = s->procStates[proc].frontier - s->heap->start;
      }
    }
    bytesCopied = 0;
  } else {
    if (detailedGCTime (s))
      startTiming (RUSAGE_THREAD, &ru_start);
    s->cumulativeStatistics->numMinorGCs++;
    s->forwardState.amInMinorGC = TRUE;
    if (DEBUG_GENERATIONAL or s->controls->messages) {
      fprintf (stderr,
               "[GC: Starting minor Cheney-copy;]\n");
      fprintf (stderr,
               "[GC:\tfrom nursery at "FMTPTR" of size %s bytes.]\n",
               (uintptr_t)(s->heap->nursery),
               uintmaxToCommaString(bytesAllocated));
    }
    s->forwardState.toStart = s->heap->start + s->heap->oldGenSize;
    assert (isFrontierAligned (s, s->forwardState.toStart));
    s->forwardState.toLimit = s->forwardState.toStart + bytesAllocated;
    assert (invariantForGC (s));
    s->cumulativeStatistics->numMinorGCs++;
    s->lastMajorStatistics->numMinorGCs++;
    s->forwardState.back = s->forwardState.toStart;
    /* Forward all globals.  Would like to avoid doing this once all
     * the globals have been assigned.
     */
    foreachGlobalObjptr (s, forwardObjptrIfInNursery, NULL);
    forwardInterGenerationalObjptrs (s);
    foreachObjptrInRange (s,
                          s->forwardState.toStart,
                          &s->forwardState.back,
                          TRUE,
                          NULL,
                          trueObjptrPredicate,
                          NULL,
                          forwardObjptrIfInNursery,
                          NULL);
    updateWeaksForCheneyCopy (s);
    bytesCopied = (size_t)(s->forwardState.back - s->forwardState.toStart);
    s->cumulativeStatistics->bytesCopiedMinor += bytesCopied;
    s->heap->oldGenSize += bytesCopied;
    s->lastMajorStatistics->numMinorGCs++;
    if (detailedGCTime (s))
      stopTiming (RUSAGE_THREAD, &ru_start, &s->cumulativeStatistics->ru_gcMinor);
    if (DEBUG_GENERATIONAL or s->controls->messages)
      fprintf (stderr,
               "[GC: Finished minor Cheney-copy; copied %s bytes.]\n",
               uintmaxToCommaString(bytesCopied));
  }
  bytesAllocated -= bytesFilled;
  s->cumulativeStatistics->bytesAllocated += bytesAllocated;
  s->cumulativeStatistics->bytesFilled += bytesFilled;
}
