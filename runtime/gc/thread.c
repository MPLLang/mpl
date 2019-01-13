/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 2014-2015 Ram Raghunathan
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "thread.h"

/************************/
/* Function definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))
void T_setCurrentThreadUseHierarchicalHeap(Bool use) {
  GC_state s = pthread_getspecific (gcstate_key);
  GC_thread thread = getThreadCurrent(s);

  if (use) {
    s->heap->usingHierarchicalHeaps = TRUE;
  } else {
    if (thread->hierarchicalHeap != BOGUS_OBJPTR) {
      HM_HH_objptrToStruct(s, thread->hierarchicalHeap)->thread = BOGUS_OBJPTR;
      thread->hierarchicalHeap = BOGUS_OBJPTR;
    }
  }
  thread->useHierarchicalHeap = use;
}
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
void displayThread (GC_state s,
                    GC_thread thread,
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIuMAX"\n"
          "\t\tbytesNeeded = %"PRIuMAX"\n"
          "\t\tinGlobalHeapCounter = %"PRIuMAX"\n"
          "\t\tstack = "FMTOBJPTR"\n"
          "\t\thierarchicalHeap = "FMTOBJPTR"\n",
          (uintmax_t)thread->exnStack,
          (uintmax_t)thread->bytesNeeded,
          ((uintmax_t)(thread->inGlobalHeapCounter)),
          thread->stack,
          thread->hierarchicalHeap);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, NULL)),
                stream);
  /* RAM_NOTE: displayHH! */
}

size_t sizeofThread (GC_state s) {
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
