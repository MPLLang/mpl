/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2014-2016 Ram Raghunathan
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
 * inGlobalHeapCounter (Word32) ::
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
  Word32 inGlobalHeapCounter;
  size_t bytesNeeded;
  size_t exnStack;
  struct HM_HierarchicalHeap* hierarchicalHeap;
  objptr stack;
} __attribute__ ((packed)) *GC_thread;

COMPILE_TIME_ASSERT(GC_thread__packed,
                    sizeof(struct GC_thread) ==
                    sizeof(Word32) +  // inGlobalHeapCounter
                    sizeof(size_t) +  // bytesNeeded
                    sizeof(size_t) +  // exnStack
                    sizeof(void*) +   // hierarchicalHeap
                    sizeof(objptr));  // stack

#define BOGUS_EXN_STACK ((size_t)(-1))

#else

struct GC_thread;
typedef struct GC_thread *GC_thread;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))
/**
 * This function sets the current thread to either use or not use the
 * hierarchical heap.
 *
 * @param use TRUE if using the Hierarchical Heap, FALSE otherwise.
 */
// PRIVATE void T_setCurrentThreadUseHierarchicalHeap(Bool use);

PRIVATE Pointer GC_HH_newHeap(void);
PRIVATE void GC_HH_attachHeap(pointer thread, pointer hh);
PRIVATE Word32 GC_HH_getLevel(pointer thread);
PRIVATE void GC_HH_setLevel(pointer thread, Word32 level);
PRIVATE void GC_HH_attachChild(pointer parent, pointer child, Word32 level);
PRIVATE void GC_HH_mergeDeepestChild(pointer thread);
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
