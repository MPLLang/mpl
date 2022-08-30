/* Copyright (C) 2018-2021 Sam Westrick.
 * Copyright (C) 2009,2012,2019,2021 Matthew Fluet.
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

struct FixedSizeAllocator* getHHAllocator(GC_state s) {
  return &(s->hhAllocator);
}

struct FixedSizeAllocator* getUFAllocator(GC_state s) {
  return &(s->hhUnionFindAllocator);
}

Bool_t GC_getAmOriginal (GC_state s) {
  return (Bool_t)(s->amOriginal);
}
void GC_setAmOriginal (GC_state s, Bool_t b) {
  s->amOriginal = (bool)b;
}

void GC_setControlsMessages (GC_state s, Bool_t b) {
  s->controls->messages = (bool)b;
}

void GC_setControlsSummary (GC_state s, Bool_t b) {
  s->controls->summary = (bool)b;
}

void GC_setControlsRusageMeasureGC (GC_state s, Bool_t b) {
  s->controls->rusageMeasureGC = (bool)b;
}

uint32_t GC_getControlMaxCCDepth(GC_state s) {
  return (uint32_t)s->controls->hhConfig.maxCCDepth;
}

// SAM_NOTE: TODO: remove this and replace with blocks statistics
size_t GC_getMaxChunkPoolOccupancy (void) {
  return 0;
}

size_t GC_getGlobalCumulativeStatisticsMaxHeapOccupancy (GC_state s) {
  return s->globalCumulativeStatistics->maxHeapOccupancy;
}

uintmax_t GC_getCumulativeStatisticsBytesAllocatedOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->bytesAllocated;
}

uintmax_t GC_getCumulativeStatisticsLocalBytesReclaimedOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->bytesReclaimedByLocal;
}

uintmax_t GC_getCumulativeStatisticsBytesAllocated (GC_state s) {
  /* return sum across all processors */
  size_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->bytesAllocated;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsBytesPromoted (GC_state s) {
  /* sum over all procs */
  uintmax_t retVal = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++) {
    retVal += s->procStates[p].cumulativeStatistics->bytesPromoted;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (GC_state s) {
  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numCopyingGCs;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (GC_state s) {
  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numMarkCompactGCs;
  }

  return retVal;
}

uintmax_t GC_getCumulativeStatisticsNumMinorGCs (GC_state s) {
  /* return sum across all processors */
  uintmax_t retVal = 0;
  for (size_t i = 0; i < s->numberOfProcs; i++) {
    retVal += s->procStates[i].cumulativeStatistics->numMinorGCs;
  }

  return retVal;
}

size_t GC_getCumulativeStatisticsMaxBytesLive (GC_state s) {
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

uintmax_t GC_getCumulativeStatisticsNumLocalGCsOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->numHHLocalGCs;
}

uintmax_t GC_getNumRootCCsOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->numRootCCs;
}

uintmax_t GC_getNumInternalCCsOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->numInternalCCs;
}

uintmax_t GC_getRootCCMillisecondsOfProc(GC_state s, uint32_t proc) {
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeRootCC);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

uintmax_t GC_getInternalCCMillisecondsOfProc(GC_state s, uint32_t proc) {
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeInternalCC);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

uintmax_t GC_getRootCCBytesReclaimedOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->bytesReclaimedByRootCC;
}

uintmax_t GC_getInternalCCBytesReclaimedOfProc(GC_state s, uint32_t proc) {
  return s->procStates[proc].cumulativeStatistics->bytesReclaimedByInternalCC;
}

uintmax_t GC_getLocalGCMillisecondsOfProc(GC_state s, uint32_t proc) {
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeLocalGC);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

uintmax_t GC_getPromoMillisecondsOfProc(GC_state s, uint32_t proc) {
  struct timespec *t = &(s->procStates[proc].cumulativeStatistics->timeLocalPromo);
  return (uintmax_t)t->tv_sec * 1000 + (uintmax_t)t->tv_nsec / 1000000;
}

uintmax_t GC_numDisentanglementChecks(GC_state s) {
  uintmax_t count = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++) {
    count += s->procStates[p].cumulativeStatistics->numDisentanglementChecks;
  }
  return count;
}

uintmax_t GC_numChecksSkipped(GC_state s)
{
  uintmax_t count = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++)
  {
    count += s->procStates[p].cumulativeStatistics->numChecksSkipped;
  }
  return count;
}

uintmax_t GC_numSuspectsMarked(GC_state s)
{
  uintmax_t count = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++)
  {
    count += s->procStates[p].cumulativeStatistics->numSuspectsMarked;
  }
  return count;
}

uintmax_t GC_numSuspectsCleared(GC_state s)
{
  uintmax_t count = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++)
  {
    count += s->procStates[p].cumulativeStatistics->numSuspectsCleared;
  }
  return count;
}

uintmax_t GC_numEntanglementsDetected(GC_state s) {
  uintmax_t count = 0;
  for (uint32_t p = 0; p < s->numberOfProcs; p++) {
    count += s->procStates[p].cumulativeStatistics->numEntanglementsDetected;
  }
  return count;
}

__attribute__((noreturn))
void GC_setHashConsDuringGC(__attribute__((unused)) GC_state s, __attribute__((unused)) Bool_t b) {
  DIE("GC_setHashConsDuringGC unsupported");
}

size_t GC_getLastMajorStatisticsBytesLive (GC_state s) {
  return s->lastMajorStatistics->bytesLive;
}

pointer GC_getCallFromCHandlerThread (GC_state s) {
  pointer p = objptrToPointer (s->callFromCHandlerThread, NULL);
  return p;
}

void GC_setCallFromCHandlerThreads (GC_state s, pointer p) {
  assert(getSequenceLength (p) == s->numberOfProcs);
  for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].callFromCHandlerThread = ((objptr*)p)[proc];
  }
}

pointer GC_getCallFromCOpArgsResPtr (GC_state s) {
  return s->callFromCOpArgsResPtr;
}

pointer GC_getCurrentThread (GC_state s) {
  pointer p = objptrToPointer (s->currentThread, NULL);
  return p;
}

pointer GC_getSavedThread (GC_state s) {
  pointer p;

  assert(s->savedThread != BOGUS_OBJPTR);
  p = objptrToPointer (s->savedThread, NULL);
  s->savedThread = BOGUS_OBJPTR;
  return p;
}

void GC_setSavedThread (GC_state s, pointer p) {
  objptr op;

  assert(s->savedThread == BOGUS_OBJPTR);
  op = pointerToObjptr (p, NULL);
  s->savedThread = op;
}

void GC_setSignalHandlerThreads (GC_state s, pointer p) {
  assert(getSequenceLength (p) == s->numberOfProcs);
  for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].signalHandlerThread = ((objptr*)p)[proc];
  }
}

struct TLSObjects* GC_getTLSObjects(GC_state s) {
  return &(s->tlsObjects);
}

void GC_getGCRusageOfProc (GC_state s, int32_t p, struct rusage* rusage) {
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
sigset_t* GC_getSignalsHandledAddr (GC_state s) {
  return &(s->procStates[0].signalsInfo.signalsHandled);
}

sigset_t* GC_getSignalsPendingAddr (GC_state s) {
  return &(s->signalsInfo.signalsPending);
}

// Signal disposition is per-process; use primary to maintain handled set.
void GC_setGCSignalHandled (GC_state s, Bool_t b) {
  s->procStates[0].signalsInfo.gcSignalHandled = (bool)b;
}

Bool_t GC_getGCSignalPending (GC_state s) {
  return (bool)(s->signalsInfo.gcSignalPending);
}

void GC_setGCSignalPending (GC_state s, Bool_t b) {
  s->signalsInfo.gcSignalPending = (bool)b;
}

void GC_registerQueue(uint32_t processor, pointer queuePointer) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(processor < s->numberOfProcs);
  s->procStates[processor].wsQueue = pointerToObjptr(queuePointer, NULL);
}

void GC_registerQueueTop(uint32_t processor, pointer topPointer) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(processor < s->numberOfProcs);
  s->procStates[processor].wsQueueTop = pointerToObjptr(topPointer, NULL);
}

void GC_registerQueueBot(uint32_t processor, pointer botPointer) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(processor < s->numberOfProcs);
  s->procStates[processor].wsQueueBot = pointerToObjptr(botPointer, NULL);
}
