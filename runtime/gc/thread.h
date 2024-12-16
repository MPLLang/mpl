/* Copyright (C) 2018-2023 Sam Westrick
 * Copyright (C) 2014-2016 Ram Raghunathan
 * Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef THREAD_H_
#define THREAD_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define DECHECK_DEPTHS_LEN 32

/*
 * Thread objects are normal objects with the following layout:
 *
 * header ::
 * padding ::
 * bytesNeeded (size_t) ::
 * exnStack (ptrdiff_t) ::
 * hierarchicalHeap (c pointer)
 * stack (object-pointer) ::
 *
 * There may be zero or more bytes of padding for alignment purposes.
 *
 * The bytesNeeded size_t is the number of bytes needed when returning
 * to this thread.
 *
 * The exnStack ptrdiff_t is an offset added to stackBottom that
 * specifies the top of the exnStack.
 *
 * Note that the order of the fields is important.  The non-objptr
 * fields must be first, because a thread object must appear to be a
 * normal object.
 */
typedef struct GC_thread {

  /* unspent heartbeats */
  uint32_t spareHeartbeatTokens;

  int32_t currentProcNum; /* the worker currently executing this thread */
  size_t bytesNeeded;
  ptrdiff_t exnStack;

  /* Current depth (fork depth) of the thread allocating in this heap.
   * Fresh chunks are placed at this level. */
  uint32_t currentDepth;

  uint32_t minLocalCollectionDepth;

#ifdef DETECT_ENTANGLEMENT
  decheck_tid_t decheckState;
  uint32_t decheckSyncDepths[DECHECK_DEPTHS_LEN];
#endif

  size_t bytesAllocatedSinceLastCollection;
  size_t bytesSurvivedLastCollection;

  struct HM_HierarchicalHeap* hierarchicalHeap;

  /* The "current" chunk of the heap, where the frontier is pointing */
  HM_chunk currentChunk;

  objptr stack;
} __attribute__ ((packed)) *GC_thread;

#ifdef DETECT_ENTANGLEMENT

COMPILE_TIME_ASSERT(GC_thread__packed,
                    sizeof(struct GC_thread) ==
                    sizeof(uint32_t) + // spareHeartbeatTokens
                    sizeof(int32_t) +  // currentProcNum
                    sizeof(size_t) +  // bytesNeeded
                    sizeof(ptrdiff_t) +  // exnStack
                    sizeof(uint32_t) + // currentDepth
                    sizeof(uint32_t) + // minLocalCollectionDepth
                    sizeof(decheck_tid_t) + // disentanglement checker state
                    DECHECK_DEPTHS_LEN * sizeof(uint32_t) + // disentanglement checker state
                    sizeof(size_t) +  // bytesAllocatedSinceLastCollection
                    sizeof(size_t) +  // bytesSurvivedLastCollection
                    sizeof(void*) +   // hierarchicalHeap
                    sizeof(void*) +   // currentCheck
                    sizeof(objptr));  // stack

#else

COMPILE_TIME_ASSERT(GC_thread__packed,
                    sizeof(struct GC_thread) ==
                    sizeof(uint32_t) + // spareHeartbeatTokens
                    sizeof(int32_t) +  // currentProcNum
                    sizeof(size_t) +  // bytesNeeded
                    sizeof(ptrdiff_t) +  // exnStack
                    sizeof(uint32_t) + // currentDepth
                    sizeof(uint32_t) + // minLocalCollectionDepth
                    sizeof(size_t) +  // bytesAllocatedSinceLastCollection
                    sizeof(size_t) +  // bytesSurvivedLastCollection
                    sizeof(void*) +   // hierarchicalHeap
                    sizeof(void*) +   // currentCheck
                    sizeof(objptr));  // stack

#endif // DETECT_ENTANGLEMENT

#define BOGUS_EXN_STACK ((ptrdiff_t)(-1))

#else

struct GC_thread;
typedef struct GC_thread *GC_thread;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE Word32 GC_HH_getDepth(pointer thread);
PRIVATE void GC_HH_setDepth(pointer thread, Word32 depth);
PRIVATE void GC_HH_mergeThreads(pointer threadp, pointer childp);
PRIVATE void GC_HH_promoteChunks(pointer thread);
PRIVATE void GC_HH_setMinLocalCollectionDepth(pointer thread, Word32 depth);

// forkThread should only be called if canForkThread returns true
// (and without resuming the thread in between)
PRIVATE bool GC_HH_canForkThread(GC_state s, pointer thread);

// If youngest, then pick the youngest promotable frame. This is used in
// the scheduler for an optimization in a special case.
PRIVATE objptr GC_HH_forkThread(GC_state s, bool youngest, pointer thread, pointer jp);

/* Moves a "new" thread to the appropriate depth, before we switch to it.
 * This essentially puts the thread (and its stack) into the hierarchy.
 * Also sets the depth of the thread.
 */
PRIVATE void GC_HH_moveNewThreadToDepth(pointer thread, Word64 tidParent, Word32 depth);

PRIVATE void GC_HH_clearSuspectsAtDepth(GC_state s, pointer threadp, uint32_t depth);

PRIVATE Word64 GC_HH_numSuspectsAtDepth(GC_state s, pointer threadp, uint32_t depth);
PRIVATE Pointer /*ES_clearSet*/ GC_HH_takeClearSetAtDepth(GC_state s, pointer threadp, uint32_t depth);
PRIVATE Word64 GC_HH_numChunksInClearSet(GC_state s, pointer clearSet);
PRIVATE Pointer /*ES_finishedClearSetGrain*/ GC_HH_processClearSetGrain(GC_state s, pointer clearSet, Word64 start, Word64 stop);
PRIVATE void GC_HH_commitFinishedClearSetGrain(GC_state s, pointer threadp, pointer finClearSetGrain);
PRIVATE void GC_HH_deleteClearSet(GC_state s, pointer clearSet);

PRIVATE Bool GC_HH_checkFinishedCCReadyToJoin(GC_state s);

PRIVATE Bool GC_tryConsumeSpareHeartbeats(GC_state s, uint32_t count);

// returns new count
PRIVATE uint32_t GC_addSpareHeartbeats(GC_state s, uint32_t spares);

PRIVATE void GC_HH_joinIntoParentBeforeFastClone(GC_state s, pointer threadp, uint32_t newDepth, uint64_t tidLeft, uint64_t tidRight);
PRIVATE void GC_HH_joinIntoParent(GC_state s, pointer threadp, pointer rightSideThreadp, uint32_t newDepth, uint64_t tidLeft, uint64_t tidRight);

PRIVATE void setPromoStackOfCurrentThread(GC_state s, pointer newBot, pointer newTop);

#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayThread (GC_state s, GC_thread thread, FILE *stream);
static inline size_t sizeofThread (GC_state s);
static inline size_t offsetofThread (GC_state s);

/**
 * This function converts a thread objptr to the GC_thread.
 *
 * @param s The GC_state to use
 * @param threadObjptr the objptr to convert
 *
 * @return the contained GC_thread if threadObjptr is a valid objptr, NULL
 * otherwise
 */
static inline GC_thread threadObjptrToStruct(GC_state s, objptr threadObjptr);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* THREAD_H_ */
