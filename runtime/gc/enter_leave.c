/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* enter and leave should be called at the start and end of every GC
 * function that is exported to the outside world.  They make sure
 * that the function is run in a critical section and check the GC
 * invariant.
 */
/* SAM_NOTE: these are no longer used, but perhaps they should be.
 * TODO: check that these work with the new runtime setup. */
void enter (GC_state s) {

  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  if (DEBUG) {
    /* RAM_NOTE: Switch to using LOG */
    displayGCState (s, stderr);
  }

  assert (invariantForGC (s));
}

void leave (GC_state s) {
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  assert(invariantForMutator(s, FALSE, TRUE));

  endAtomic (s);
}
