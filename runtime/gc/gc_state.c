/* Copyright (C) 2009,2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void displayGCState (GC_state s, FILE *stream) {
  fprintf (stream,
           "GC state\n");

  fprintf (stream, "\tcurrentThread = "FMTOBJPTR"\n", s->currentThread);
  displayThread (s, (GC_thread)(objptrToPointer (s->currentThread, NULL)
                                + offsetofThread (s)),
                 stream);

  fprintf (stream, "\tgenerational\n");

  fprintf (stream, "\theap\n");

  fprintf (stream,
           "\tfrontier = "FMTPTR"\n"
           "\tlimit = "FMTPTR"\n"
           "\tlimitPlusSlop = "FMTPTR"\n"
           "\tstackBottom = "FMTPTR"\n"
           "\tstackTop = "FMTPTR"\n",
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

// SAM_NOTE: TODO: remove this and replace with blocks statistics
size_t GC_getMaxChunkPoolOccupancy (void) {
  return 0;
}

size_t GC_getGlobalCumulativeStatisticsMaxHeapOccupancy (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  return s->globalCumulativeStatistics->maxHeapOccupancy;
}

uintmax_t GC_getCumulativeStatisticsBytesAllocated (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* return sum across all processors */
  size_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->bytesAllocated;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsBytesPromoted (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* sum over all procs */
  uintmax_t retVal = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++) {
    retVal += s->procStates[p].cumulativeStatistics->bytesPromoted;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numCopyingGCs;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numMarkCompactGCs;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumMinorGCs (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numMinorGCs;
  }

  return retVal;
}

size_t GC_getCumulativeStatisticsMaxBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* return max across all processors */
  size_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    size_t candidate = s->procStates[i].cumulativeStatistics->maxBytesLive;
    if (candidate > retVal) {
      retVal = candidate;
    }
  }

  return retVal;
}

uintmax_t GC_getLocalGCMillisecondsOfProc(uint32_t proc) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeLocalGC);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

uintmax_t GC_getPromoMillisecondsOfProc(uint32_t proc) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeLocalPromo);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

__attribute__((noreturn))
void GC_setHashConsDuringGC(bool b) {
  DIE("GC_setHashConsDuringGC unsupported");
}

size_t GC_getLastMajorStatisticsBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->lastMajorStatistics->bytesLive;
}

pointer GC_getCallFromCHandlerThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->callFromCHandlerThread, NULL);
  return p;
}

void GC_setCallFromCHandlerThreads (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(getSequenceLength (p) == s->numberOfProcs);
  for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].callFromCHandlerThread = ((objptr*)p)[proc];
  }
}

pointer GC_getCurrentThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer(s->currentThread, NULL);
  return p;
}

pointer GC_getSavedThread (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p;

  assert(s->savedThread != BOGUS_OBJPTR);
  p = objptrToPointer (s->savedThread, NULL);
  s->savedThread = BOGUS_OBJPTR;
  return p;
}

void GC_setSavedThread (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  objptr op;

  assert(s->savedThread == BOGUS_OBJPTR);
  op = pointerToObjptr (p, NULL);
  s->savedThread = op;
}

void GC_setSignalHandlerThreads (pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(getSequenceLength (p) == s->numberOfProcs);
  for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].signalHandlerThread = ((objptr*)p)[proc];
  }
}

struct TLSObjects* GC_getTLSObjects(void) {
  GC_state s = pthread_getspecific (gcstate_key);

  return &(s->tlsObjects);
}

void GC_getGCRusageOfProc (int32_t p, struct rusage* rusage) {
  GC_state s = pthread_getspecific (gcstate_key);

  if (p < 0) {
    /* get process gc rusage */
    rusageZero(rusage);
    for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
      /* global heap collection is stop-the-world, so multiply by P */
      struct rusage stwGC;
      rusageZero(&stwGC);

      rusagePlusMax(&stwGC,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcCopying),
                    &stwGC);
      rusagePlusMax(&stwGC,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcMarkCompact),
                    &stwGC);
      rusagePlusMax(&stwGC,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcMinor),
                    &stwGC);
      rusageMultiply(&stwGC,
                     s->numberOfProcs,
                     &stwGC);

      rusagePlusMax(rusage,
                    &stwGC,
                    rusage);

      /* HHLocal collection is parallel, so just add it in */
      rusagePlusMax(rusage,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcHHLocal),
                    rusage);
    }
  } else {
    /* get processor gc rusage */
    rusageZero(rusage);

    if ((uint32_t)p >= s->numberOfProcs) {
      /* proc doesn't exist so return zero */
      return;
    }

    for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
      /* global heap collection is stop-the-world, so gather from all procs */
      rusagePlusMax(rusage,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcCopying),
                    rusage);
      rusagePlusMax(rusage,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcMarkCompact),
                    rusage);
      rusagePlusMax(rusage,
                    &(s->procStates[proc].cumulativeStatistics->ru_gcMinor),
                    rusage);
    }

    rusagePlusMax(rusage,
                  &(s->procStates[p].cumulativeStatistics->ru_gcHHLocal),
                  rusage);
  }
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
