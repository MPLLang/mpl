/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void switchToThread (GC_state s, objptr op) {
  if (DEBUG_THREADS) {
    GC_thread thread;
    GC_stack stack;

    thread = (GC_thread)(objptrToPointer (op, s->heap->start)
                         + offsetofThread (s));
    stack = (GC_stack)(objptrToPointer (thread->stack, s->heap->start));

    fprintf (stderr, "switchToThread ("FMTOBJPTR")  used = %"PRIuMAX
             "  reserved = %"PRIuMAX"\n",
             op, (uintmax_t)stack->used, (uintmax_t)stack->reserved);
  }
  s->currentThread = op;
  setGCStateCurrentThreadAndStack (s);
}

void GC_switchToThread (GC_state s, pointer p, size_t ensureBytesFree) {
  if (DEBUG_THREADS)
    fprintf (stderr,
             "GC_switchToThread ("FMTPTR", %"PRIuMAX") [%d]\n",
             (uintptr_t)p,
             (uintmax_t)ensureBytesFree,
             Proc_processorNumber (s));
#warning Switch to other branch when I can
  if (TRUE) {
    /* This branch is slower than the else branch, especially
     * when debugging is turned on, because it does an invariant
     * check on every thread switch.
     * So, we'll stick with the else branch for now.
     */
    //ENTER1 (s, p);
    /* XXX copied from enter() */
    /* used needs to be set because the mutator has changed s->stackTop. */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    beginAtomic (s);

    getThreadCurrent(s)->bytesNeeded = ensureBytesFree;
    switchToThread (s, pointerToObjptr(p, s->heap->start));
    s->atomicState--;
    switchToSignalHandlerThreadIfNonAtomicAndSignalPending (s);
    ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE,
                                                    TRUE, TRUE,
                                                    0, 0);

    endAtomic (s);
    assert (invariantForMutatorFrontier(s));
    assert (invariantForMutatorStack(s));
    //LEAVE0 (s);
  } else {
#warning Why? It looks exactly the same...
    assert (false and "unsafe in a multiprocessor context");
    /* BEGIN: enter(s); */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    beginAtomic (s);
    /* END: enter(s); */
    getThreadCurrent(s)->bytesNeeded = ensureBytesFree;
    switchToThread (s, pointerToObjptr(p, s->heap->start));
    s->atomicState--;
    switchToSignalHandlerThreadIfNonAtomicAndSignalPending (s);
    /* BEGIN: ensureInvariantForMutator */
    if (not (invariantForMutatorFrontier(s))
        or not (invariantForMutatorStack(s))) {
      /* This GC will grow the stack, if necessary. */
      performGC (s, 0, getThreadCurrent(s)->bytesNeeded, FALSE, TRUE);
    }
    /* END: ensureInvariantForMutator */
    /* BEGIN: leave(s); */
    endAtomic (s);
    /* END: leave(s); */
    assert (invariantForMutatorFrontier(s));
    assert (invariantForMutatorStack(s));
  }
}
