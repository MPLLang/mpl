/* Copyright (C) 2020 Sam Westrick.
 * Copyright (C) 2009-2010,2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "hierarchical-heap.h"

void growStackCurrent(GC_state s) {
  size_t reserved;
  size_t stackSize;
  GC_stack stack;

  reserved = sizeofStackGrowReserved(s, getStackCurrent(s));
  assert(isStackReservedAligned (s, reserved));
  stackSize = sizeofStackWithMetaData(s, reserved);
  if (DEBUG_STACKS or s->controls->messages)
    fprintf (stderr,
             "[GC: Growing stack of size %s bytes to size %s bytes, using %s bytes.]\n",
             uintmaxToCommaString(getStackCurrent(s)->reserved),
             uintmaxToCommaString(reserved),
             uintmaxToCommaString(getStackCurrent(s)->used));
  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;

  HM_chunk chunk = HM_getChunkOf((pointer)getStackCurrent(s));
  HM_HierarchicalHeap hh = HM_getLevelHeadPathCompress(chunk);

  if (chunk->mightContainMultipleObjects) {
    DIE("Tried to grow a stack without its own chunk.");
  }

  assert(HM_getChunkFrontier(chunk) == HM_getChunkStart(chunk) +
    sizeofStackWithMetaData(s, getStackCurrent(s)->reserved));

  /* the easy case: plenty of space in the stack's chunk to just grow the
   * stack in place. */
  if (stackSize <= (size_t)(HM_getChunkLimit(chunk) - HM_getChunkStart(chunk))) {
    getStackCurrent(s)->reserved = reserved;
    HM_updateChunkValues(chunk, HM_getChunkStart(chunk) + stackSize);
    return;
  }

  /* in this case, the new stack needs more space, so allocate a new chunk,
   * copy the stack, and throw away the old chunk. */
  HM_chunk newChunk = HM_allocateChunk(HM_HH_getChunkList(hh), stackSize);
  if (NULL == newChunk) {
    DIE("Ran out of space to grow stack!");
  }
  assert(stackSize < HM_getChunkSizePastFrontier(newChunk));
  newChunk->mightContainMultipleObjects = FALSE;
  newChunk->levelHead = HM_HH_getUFNode(hh);

  pointer frontier = HM_getChunkFrontier(newChunk);
  assert(frontier == HM_getChunkStart(newChunk));
  assert(GC_STACK_METADATA_SIZE == GC_HEADER_SIZE);
  *((GC_header*)frontier) = GC_STACK_HEADER;
  stack = (GC_stack)(frontier + GC_HEADER_SIZE);
  stack->reserved = reserved;
  stack->used = 0;
  HM_updateChunkValues(newChunk, frontier + stackSize);

  copyStack(s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr((pointer)stack, NULL);

  assert(getThreadCurrent(s)->currentChunk != chunk);
  HM_unlinkChunk(HM_HH_getChunkList(hh), chunk);
  HM_appendChunk(getFreeListSmall(s), chunk);
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
  HH_EBR_leaveQuiescentState(s);

  assert(getThreadCurrent(s)->hierarchicalHeap != NULL);
  assert(threadAndHeapOkay(s));

  /* adjust bytesRequested */
  /*
   * When the mutator requests zero bytes, it may actually need as
   * much as GC_HEAP_LIMIT_SLOP.
   */
  bytesRequested += GC_HEAP_LIMIT_SLOP;

  assert(bytesRequested + sizeof(struct HM_chunk) <= s->controls->blockSize);

  getThreadCurrent(s)->bytesNeeded = bytesRequested;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  /* SAM_NOTE: shouldn't this be
   *   getThreadCurrent(s)->bytesNeeded
   * instead of bytesRequested? */
  HM_ensureHierarchicalHeapAssurances(s, force, bytesRequested, FALSE);
  // CC_collectWithRoots(s, )

  endAtomic(s);

  Trace0(EVENT_RUNTIME_LEAVE);
}
