/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2014-2016 Ram Raghunathan
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

/*
 * Thread objects are normal objects with the following layout:
 *
 * header ::
 * padding ::
 * bytesNeeded (size_t) ::
 * exnStack (size_t) ::
 * hierarchicalHeap (c pointer)
 * stack (object-pointer) ::
 *
 * There may be zero or more bytes of padding for alignment purposes.
 *
 * The bytesNeeded size_t is the number of bytes needed when returning
 * to this thread.
 *
 * The exnStack size_t is an offset added to stackBottom that
 * specifies the top of the exnStack.
 *
 * Note that the order of the fields is important.  The non-objptr
 * fields must be first, because a thread object must appear to be a
 * normal object.
 */
typedef struct GC_thread {
  int32_t currentProcNum; /* the worker currently executing this thread */
  size_t bytesNeeded;
  size_t exnStack;

  /* Current depth (fork depth) of the thread allocating in this heap.
   * Fresh chunks are placed at this level. */
  uint32_t currentDepth;

  size_t bytesAllocatedSinceLastCollection;
  size_t bytesSurvivedLastCollection;

  struct HM_HierarchicalHeap* hierarchicalHeap;

  /* The "current" chunk of the heap, where the frontier is pointing */
  HM_chunk currentChunk;

  objptr stack;
} __attribute__ ((packed)) *GC_thread;

COMPILE_TIME_ASSERT(GC_thread__packed,
                    sizeof(struct GC_thread) ==
                    sizeof(int32_t) +  // currentProcNum
                    sizeof(size_t) +  // bytesNeeded
                    sizeof(size_t) +  // exnStack
                    sizeof(uint32_t) + // currentDepth
                    sizeof(size_t) +  // bytesAllocatedSinceLastCollection
                    sizeof(size_t) +  // bytesSurvivedLastCollection
                    sizeof(void*) +   // hierarchicalHeap
                    sizeof(void*) +   // currentCheck
                    sizeof(objptr));  // stack

#define BOGUS_EXN_STACK ((size_t)(-1))

#else

struct GC_thread;
typedef struct GC_thread *GC_thread;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE Word32 GC_HH_getDepth(pointer thread);
PRIVATE void GC_HH_setDepth(pointer thread, Word32 depth);
PRIVATE void GC_HH_mergeThreads(pointer threadp, pointer childp);
PRIVATE void GC_HH_promoteChunks(pointer thread);
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
