/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 2014-2015 Ram Raghunathan
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "thread.h"
#include "hierarchical-heap.h"

/************************/
/* Function definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))

pointer GC_HH_newHeap() {
  GC_state s = pthread_getspecific(gcstate_key);
  return (pointer)HM_HH_new(s);
}

void GC_HH_attachHeap(pointer threadp, pointer hhp) {
  GC_state s = pthread_getspecific(gcstate_key);
  objptr threadop = pointerToObjptr(threadp, NULL);
  GC_thread thread = threadObjptrToStruct(s, threadop);

#if ASSERT
  assert(threadop != BOGUS_OBJPTR);
  /* Make sure this is an inactive thread. */
  for (int i = 0; i < s->numberOfProcs; i++) {
    assert(s->procStates[i].currentThread != threadop);
  }
#endif

  if (thread->hierarchicalHeap != NULL) {
    DIE("tried to assign hierarchical heap, but thread already has one");
  }

  thread->hierarchicalHeap = (struct HM_HierarchicalHeap *)hhp;
}

Word32 GC_HH_getLevel(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  return HM_HH_getLevel(s, thread->hierarchicalHeap);
}

void GC_HH_setLevel(pointer threadp, Word32 level) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_setLevel(s, thread->hierarchicalHeap, level);
}

void GC_HH_attachChild(pointer parentp, pointer childp, Word32 level) {
  GC_state s = pthread_getspecific(gcstate_key);
  objptr parentop = pointerToObjptr(parentp, NULL);
  objptr childop = pointerToObjptr(childp, NULL);
  GC_thread parent = threadObjptrToStruct(s, parentop);
  GC_thread child = threadObjptrToStruct(s, childop);

  // struct HM_HierarchicalHeap* childhh = (struct HM_HierarchicalHeap*)childhhp;

#if ASSERT
  assert(parentop != BOGUS_OBJPTR);
  assert(childop != BOGUS_OBJPTR);
  /* Make sure child is an inactive thread. */
  for (int i = 0; i < s->numberOfProcs; i++) {
    assert(s->procStates[i].currentThread != childop);
  }
  /* Make sure parent is either mine, or inactive */
  for (int i = 0; i < s->numberOfProcs; i++) {
    if (i != s->procNumber)
      assert(s->procStates[i].currentThread != parentop);
  }
#endif

  HM_HH_appendChild(s, parent->hierarchicalHeap, child->hierarchicalHeap, level);
}

void GC_HH_mergeDeepestChild(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  objptr threadop = pointerToObjptr(threadp, NULL);
  GC_thread thread = threadObjptrToStruct(s, threadop);

#if ASSERT
  assert(threadop != BOGUS_OBJPTR);
  /* make sure thread is either mine or inactive */
  for (int i = 0; i < s->numberOfProcs; i++) {
    if (i != s->procNumber)
      assert(s->procStates[i].currentThread != threadop);
  }
#endif

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_mergeIntoParent(s, thread->hierarchicalHeap->childHHList);
}

void GC_HH_promoteChunks(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_promoteChunks(s, thread->hierarchicalHeap);
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
