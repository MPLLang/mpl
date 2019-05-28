/* Copyright (C) 2009-2010,2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "hierarchical-heap.h"

void growStackCurrent(GC_state s) {
  size_t reserved;
  GC_stack stack;

  reserved = sizeofStackGrowReserved(s, getStackCurrent(s));
  if (DEBUG_STACKS or s->controls->messages)
    fprintf (stderr,
             "[GC: Growing stack of size %s bytes to size %s bytes, using %s bytes.]\n",
             uintmaxToCommaString(getStackCurrent(s)->reserved),
             uintmaxToCommaString(reserved),
             uintmaxToCommaString(getStackCurrent(s)->used));
#if ASSERT
  assert(threadAndHeapOkay(s));
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);
  assert(s->frontier == HM_HH_getFrontier(hh));
  assert((size_t)(HM_HH_getLimit(hh) - HM_HH_getFrontier(hh)) >= sizeofStackWithMetaData(s, reserved));
#endif
  stack = newStack(s, reserved);
  copyStack(s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr ((pointer)stack, NULL);
}

void GC_collect (GC_state s, size_t bytesRequested, bool force) {
  Trace0(EVENT_RUNTIME_ENTER);

  /* Exit as soon as termination is requested. */
  GC_MayTerminateThread(s);

  /* SPOONHOWER_NOTE: Used to be enter() here */
  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic(s);

  assert(getThreadCurrent(s)->hierarchicalHeap != NULL);
  assert(threadAndHeapOkay(s));

  /* adjust bytesRequested */
  /*
   * When the mutator requests zero bytes, it may actually need as
   * much as GC_HEAP_LIMIT_SLOP.
   */
  bytesRequested += GC_HEAP_LIMIT_SLOP;

  assert(bytesRequested + sizeof(struct HM_chunk) <= s->controls->minChunkSize);

  getThreadCurrent(s)->bytesNeeded = bytesRequested;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  /* SAM_NOTE: shouldn't this be
   *   getThreadCurrent(s)->bytesNeeded
   * instead of bytesRequested? */
  HM_ensureHierarchicalHeapAssurances(s, force, bytesRequested, FALSE);

  endAtomic(s);

  Trace0(EVENT_RUNTIME_LEAVE);
}

pointer FFI_getArgs (GC_state s) {
  return s->ffiArgs;
}
