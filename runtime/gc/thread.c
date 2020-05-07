/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 2014-2015 Ram Raghunathan
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "thread.h"
#include "hierarchical-heap.h"

/************************/
/* Function definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))

Word32 GC_HH_getDepth(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  return thread->currentDepth;
}

void GC_HH_setDepth(pointer threadp, Word32 depth) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  thread->currentDepth = depth;
  // printf("%s %d\n", "setting thread depth to ", depth);
  // printf("%s %d\n", "HH depth = ", thread->hierarchicalHeap->depth);
  /* SAM_NOTE: not super relevant here, but if we do eventually decide to
   * control the "use ancestor chunk" optimization, a good sanity check. */
  assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
  assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);
}

void GC_HH_mergeThreads(pointer threadp, pointer childp) {
  GC_state s = pthread_getspecific(gcstate_key);

  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  assert(threadAndHeapOkay(s));

  objptr threadop = pointerToObjptr(threadp, NULL);
  objptr childop = pointerToObjptr(childp, NULL);
  GC_thread thread = threadObjptrToStruct(s, threadop);
  GC_thread child = threadObjptrToStruct(s, childop);

#if ASSERT
  assert(threadop != BOGUS_OBJPTR);
  /* make sure thread is either mine or inactive */
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    if ((int32_t)i != s->procNumber)
      assert(s->procStates[i].currentThread != threadop);
  }

  assert(childop != BOGUS_OBJPTR);
  /* SAM_NOTE there is a race where the following check can raise
   * a false alarm, if a worker delays to mark its current thread as
   * BOGUS_OBJPTR after completing a thread and decrementing the incounter
   * (in the scheduler). However, having the assert seems useful as a
   * sanity check regardless.
   *
   * If this becomes a problem, we can either fix the incounter business
   * (switch away before decrementing incounter) or just remove the sanity
   * check and not worry about it.
   */
  // Make sure child is inactive
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    assert(s->procStates[i].currentThread != childop);
  }
#endif

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  assert(child != NULL);
  assert(child->hierarchicalHeap != NULL);

  beginAtomic(s);
  /* SAM_NOTE: Why do we need to ensure here?? Is it just to ensure current
   * level? */
  HM_ensureHierarchicalHeapAssurances(s, false, GC_HEAP_LIMIT_SLOP, true);
  endAtomic(s);

  /*
   * This should be true, otherwise our call to
   * HM_ensureHierarchicalHeapAssurances() above was on the wrong heap!
   */
  assert(getHierarchicalHeapCurrent(s) == thread->hierarchicalHeap);

  HM_HH_merge(s, thread, child);
}

#pragma message "TODO: do I need to do runtime enter/leave here? what about other primitives?"
void GC_HH_promoteChunks(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_promoteChunks(s, thread);
}


#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
void displayThread (GC_state s,
                    GC_thread thread,
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIuMAX"\n"
          "\t\tbytesNeeded = %"PRIuMAX"\n"
          "\t\thierarchicalHeap = %p\n"
          "\t\tstack = "FMTOBJPTR"\n",
          (uintmax_t)thread->exnStack,
          (uintmax_t)thread->bytesNeeded,
          (void*)thread->hierarchicalHeap,
          thread->stack);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, NULL)),
                stream);
  /* RAM_NOTE: displayHH! */
}

size_t sizeofThread(GC_state s) {
  size_t res;

  res = GC_NORMAL_METADATA_SIZE + sizeof (struct GC_thread);
  res = align (res, s->alignment);
  if (DEBUG) {
    size_t check;
    uint16_t bytesNonObjptrs, numObjptrs;

    splitHeader (s, GC_THREAD_HEADER, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
    check = GC_NORMAL_METADATA_SIZE + (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeofThread: res = %"PRIuMAX"  check = %"PRIuMAX"\n",
               (uintmax_t)res, (uintmax_t)check);
    assert (check == res);
  }
  assert (isAligned (res, s->alignment));
  return res;
}

// SAM_NOTE: padding for alignment.
size_t offsetofThread (GC_state s) {
  return (sizeofThread (s)) - (GC_NORMAL_METADATA_SIZE + sizeof (struct GC_thread));
}

static inline GC_thread threadObjptrToStruct(GC_state s, objptr threadObjptr) {
  if (BOGUS_OBJPTR == threadObjptr) {
    return NULL;
  }

  pointer threadPointer = objptrToPointer (threadObjptr, NULL);
  return ((GC_thread)(threadPointer + offsetofThread(s)));
}
#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
