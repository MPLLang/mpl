/* Copyright (C) 2012,2014,2019-2022 Matthew Fluet.
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
  pointer frontier;
  volatile pointer limit;
  volatile pointer stackTop; /* Top of stack in current thread. */
  pointer stackLimit; /* stackBottom + stackSize - maxFrameSize */
  ptrdiff_t exnStack;
  /* Alphabetized fields follow. */
  size_t alignment; /* */
  volatile bool amInGC;
  struct HM_HierarchicalHeap *currentCCTargetHH;
  bool amOriginal;
  char **atMLtons; /* Initial @MLton args, processed before command line. */
  int atMLtonsLength;
  volatile uint32_t atomicState;
  struct BlockAllocator *blockAllocatorGlobal;
  struct BlockAllocator *blockAllocatorLocal;
  objptr callFromCHandlerThread; /* Handler for exported C calls (in heap). */
  pointer callFromCOpArgsResPtr; /* Pass op, args, and res from exported C call */
  struct GC_controls *controls;
  struct GC_globalCumulativeStatistics* globalCumulativeStatistics;
  struct GC_cumulativeStatistics *cumulativeStatistics;
  objptr currentThread; /* Currently executing thread (in heap). */
  objptr wsQueue; /* The work-stealing queue for this processor */
  objptr wsQueueTop;
  objptr wsQueueBot;
  GC_frameInfo frameInfos; /* Array of frame infos. */
  uint32_t frameInfosLength; /* Cardinality of frameInfos array. */
  struct HM_chunkList freeListSmall;
  struct HM_chunkList freeListLarge;
  size_t nextChunkAllocSize;
  /* Ordinary globals */
  objptr *globals;
  uint32_t globalsLength;
  struct FixedSizeAllocator hhAllocator;
  struct FixedSizeAllocator hhUnionFindAllocator;
  struct HH_EBR_shared * hhEBR;
  struct GC_lastMajorStatistics *lastMajorStatistics;
  pointer limitPlusSlop; /* limit + GC_HEAP_LIMIT_SLOP */
  int (*loadGlobals)(FILE *f); /* loads the globals from the file. */
  uint32_t magic; /* The magic number for this executable. */
  uint32_t maxFrameSize;
  /* SAM_NOTE: can remove this */
  bool mutatorMarksCards;
  /* The maximum amount of concurrency */
  uint32_t numberOfProcs;
  size_t numberDisentanglementChecks;  /** TODO: remove. now in cumulativeStatistics */
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
  objptr signalHandlerThread; /* Handler for signals (in heap). */
  struct GC_signalsInfo signalsInfo;
  struct GC_sourceMaps sourceMaps;
  volatile pointer stackBottom; /* Bottom of stack in current thread. */
  pthread_t self; /* thread owning the GC_state */
  struct GC_staticHeaps staticHeaps;
  struct GC_sysvals sysvals;
  uint32_t terminationLeader;
  GC_weak weaks; /* Linked list of (live) weak pointers */
  char *worldFile;
  struct TracingContext *trace;
  struct TLSObjects tlsObjects;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayGCState (GC_state s, FILE *stream);

static inline size_t sizeofGCStateCurrentStackUsed (GC_state s);
static inline void setGCStateCurrentThreadAndStack (GC_state s);

static inline struct FixedSizeAllocator* getHHAllocator(GC_state s);


#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE Bool_t GC_getAmOriginal (GC_state s);
PRIVATE void GC_setAmOriginal (GC_state s, Bool_t b);
PRIVATE void GC_setControlsMessages (GC_state s, Bool_t b);
PRIVATE void GC_setControlsSummary (GC_state s, Bool_t b);
PRIVATE void GC_setControlsRusageMeasureGC (GC_state s, Bool_t b);
// SAM_NOTE: TODO: remove this and replace with blocks statistics
PRIVATE size_t GC_getMaxChunkPoolOccupancy (void);
PRIVATE size_t GC_getGlobalCumulativeStatisticsMaxHeapOccupancy (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsBytesAllocated (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsBytesPromoted (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (GC_state s);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMinorGCs (GC_state s);
PRIVATE size_t GC_getCumulativeStatisticsMaxBytesLive (GC_state s);
PRIVATE void GC_setHashConsDuringGC (GC_state s, Bool_t b);
PRIVATE size_t GC_getLastMajorStatisticsBytesLive (GC_state s);

PRIVATE uintmax_t GC_getCumulativeStatisticsBytesAllocatedOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getCumulativeStatisticsLocalBytesReclaimedOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getLocalGCMillisecondsOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getPromoMillisecondsOfProc(GC_state s, uint32_t proc);

PRIVATE uintmax_t GC_getCumulativeStatisticsNumLocalGCsOfProc(GC_state s, uint32_t proc);

PRIVATE uintmax_t GC_getNumRootCCsOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getNumInternalCCsOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getRootCCMillisecondsOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getInternalCCMillisecondsOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getRootCCBytesReclaimedOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_getInternalCCBytesReclaimedOfProc(GC_state s, uint32_t proc);
PRIVATE uintmax_t GC_numDisentanglementChecks(GC_state s);
PRIVATE uintmax_t GC_numEntanglementsDetected(GC_state s);
PRIVATE uintmax_t GC_numChecksSkipped(GC_state s);
PRIVATE uintmax_t GC_numSuspectsMarked(GC_state s);
PRIVATE uintmax_t GC_numSuspectsCleared(GC_state s);

PRIVATE uint32_t GC_getControlMaxCCDepth(GC_state s);

PRIVATE pointer GC_getCallFromCHandlerThread (GC_state s);
PRIVATE void GC_setCallFromCHandlerThreads (GC_state s, pointer p);
PRIVATE pointer GC_getCurrentThread (GC_state s);

PRIVATE pointer GC_getSavedThread (GC_state s);
PRIVATE void GC_setSavedThread (GC_state s, pointer p);
PRIVATE void GC_setSignalHandlerThreads (GC_state s, pointer p);

PRIVATE void GC_registerQueue(uint32_t processor, pointer queuePointer);
PRIVATE void GC_registerQueueTop(uint32_t processor, pointer topPointer);
PRIVATE void GC_registerQueueBot(uint32_t processor, pointer botPointer);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

PRIVATE struct TLSObjects* GC_getTLSObjects(GC_state s);

PRIVATE void GC_getGCRusageOfProc (GC_state s, int32_t p, struct rusage* rusage);

PRIVATE sigset_t* GC_getSignalsHandledAddr (GC_state s);
PRIVATE sigset_t* GC_getSignalsPendingAddr (GC_state s);
PRIVATE void GC_setGCSignalHandled (GC_state s, Bool_t b);
PRIVATE Bool_t GC_getGCSignalPending (GC_state s);
PRIVATE void GC_setGCSignalPending (GC_state s, Bool_t b);

PRIVATE GC_state MLton_gcState ();

#endif /* GC_STATE_H_ */
