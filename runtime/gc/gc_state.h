/* Copyright (C) 2012,2014,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef GC_STATE_H_
#define GC_STATE_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_state {
  /* These fields are at the front because they are the most commonly
   * referenced, and having them at smaller offsets may decrease code
   * size and improve cache performance.
   */
  pointer frontier; /* start <= frontier < limit */
  pointer limit; /* limit = heap.start + heap.size */
  pointer stackTop; /* Top of stack in current thread. */
  pointer stackLimit; /* stackBottom + stackSize - maxFrameSize */
  size_t exnStack;
  /* Alphabetized fields follow. */
  size_t alignment; /* */
  bool amInGC;
  bool amOriginal;
  char **atMLtons; /* Initial @MLton args, processed before command line. */
  int atMLtonsLength;
  uint32_t atomicState;
  objptr callFromCHandlerThread; /* Handler for exported C calls (in heap). */
  pointer callFromCOpArgsResPtr; /* Pass op, args, and res from exported C call */
  struct GC_callStackState callStackState;
  bool canMinor; /* TRUE iff there is space for a minor gc. */
  struct GC_controls *controls;
  struct GC_globalCumulativeStatistics* globalCumulativeStatistics;
  struct GC_cumulativeStatistics *cumulativeStatistics;
  objptr currentThread; /* Currently executing thread (in heap). */
  objptr wsQueue; /* The work-stealing queue for this processor */
  objptr wsQueueLock; /* The work-stealing queue lock for this processor */
  struct GC_forwardState forwardState;
  GC_frameInfo frameInfos; /* Array of frame infos. */
  uint32_t frameInfosLength; /* Cardinality of frameInfos array. */
  struct GC_generationalMaps generationalMaps;
  pointer globalFrontier;
  pointer globalLimitPlusSlop;
  /* Ordinary globals */
  objptr *globals;
  uint32_t globalsLength;
  bool hashConsDuringGC;
  struct GC_heap *heap;
  struct GC_lastMajorStatistics *lastMajorStatistics;
  pointer limitPlusSlop; /* limit + GC_HEAP_LIMIT_SLOP */
  int (*loadGlobals)(FILE *f); /* loads the globals from the file. */
  uint32_t magic; /* The magic number for this executable. */
  uint32_t maxFrameSize;
  bool mutatorMarksCards;
  /* The maximum amount of concurrency */
  uint32_t numberOfProcs;
  GC_objectHashTable objectHashTable;
  GC_objectType objectTypes; /* Array of object types. */
  uint32_t objectTypesLength; /* Cardinality of objectTypes array. */
  int32_t procNumber;
  /* States for each processor */
  GC_state procStates;
  struct GC_profiling profiling;
  GC_frameIndex (*returnAddressToFrameIndex) (GC_returnAddress ra);
  /* Roots that may be, for example, on the C call stack */
  objptr *roots;
  uint32_t rootsLength;
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  int (*saveGlobals)(FILE *f); /* saves the globals to the file. */
  bool saveWorldStatus; /* */
  struct GC_heap *secondaryHeap; /* Used for major copying collection. */
  objptr signalHandlerThread; /* Handler for signals (in heap). */
  struct GC_signalsInfo signalsInfo;
  struct GC_sourceMaps sourceMaps;
  pointer stackBottom; /* Bottom of stack in current thread. */
  pointer start; /* Like heap.nursery but per processor.  nursery <= start <= frontier */
  pthread_t self; /* thread owning the GC_state */
  atomic_uint32_t terminationLeader;
  int32_t syncReason;
  struct GC_sysvals sysvals;
  struct GC_translateState translateState;
  struct GC_vectorInit *vectorInits;
  uint32_t vectorInitsLength;
  GC_weak weaks; /* Linked list of (live) weak pointers */
  char *worldFile;
  spinlock_t lock;
  struct TracingContext *trace;
  struct TLSObjects tlsObjects;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayGCState (GC_state s, FILE *stream);

static inline size_t sizeofGCStateCurrentStackUsed (GC_state s);
static inline void setGCStateCurrentThreadAndStack (GC_state s);
static void setGCStateCurrentHeap (GC_state s,
                                   size_t oldGenBytesRequested,
                                   size_t nurseryBytesRequested,
                                   bool duringInit);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE bool GC_getAmOriginal (GC_state s);
PRIVATE void GC_setAmOriginal (GC_state s, bool b);
PRIVATE void GC_setControlsMessages (GC_state s, bool b);
PRIVATE void GC_setControlsSummary (GC_state s, bool b);
PRIVATE void GC_setControlsRusageMeasureGC (GC_state s, bool b);
PRIVATE size_t GC_getMaxChunkPoolOccupancy ();
PRIVATE size_t GC_getGlobalCumulativeStatisticsMaxHeapOccupancy (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsBytesAllocated (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMinorGCs (GC_state s);
PRIVATE size_t GC_getCumulativeStatisticsMaxBytesLive (GC_state s);
PRIVATE void GC_setHashConsDuringGC (GC_state s, bool b);
PRIVATE size_t GC_getLastMajorStatisticsBytesLive (GC_state s);

PRIVATE pointer GC_getCallFromCHandlerThread (GC_state s);
PRIVATE void GC_setCallFromCHandlerThreads (GC_state s, pointer p);
PRIVATE pointer GC_getCallFromCOpArgsResPtr (GC_state s);


PRIVATE void GC_setCurrentThreadUseHierarchicalHeap (GC_state s);
/**
 * This function returns the current hierarchical heap
 *
 * @note
 * If no hierarchical heap was set, a new one is created, set as current, and
 * returned.
 *
 * @return pointer to the current hierarchical heap
 */
PRIVATE pointer GC_getCurrentHierarchicalHeap (GC_state s);
PRIVATE void GC_setCurrentHierarchicalHeap (GC_state s, pointer hhPointer);

PRIVATE pointer GC_getCurrentThread (GC_state s);
PRIVATE pointer GC_getSavedThread (GC_state s);
PRIVATE void GC_setSavedThread (GC_state s, pointer p);
PRIVATE void GC_setSignalHandlerThreads (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

PRIVATE struct TLSObjects* GC_getTLSObjects(GC_state s);

PRIVATE void GC_getGCRusageOfProc (GC_state s, int32_t p, struct rusage* rusage);

PRIVATE sigset_t* GC_getSignalsHandledAddr (GC_state s);
PRIVATE sigset_t* GC_getSignalsPendingAddr (GC_state s);
PRIVATE void GC_setGCSignalHandled (GC_state s, bool b);
PRIVATE bool GC_getGCSignalPending (GC_state s);
PRIVATE void GC_setGCSignalPending (GC_state s, bool b);

PRIVATE GC_state MLton_gcState ();

#endif /* GC_STATE_H_ */
