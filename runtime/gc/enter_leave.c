/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* enter and leave should be called at the start and end of every GC
 * function that is exported to the outside world.  They make sure
 * that the function is run in a critical section and check the GC
 * invariant.
 */
void enter (GC_state s) {
#pragma message "This should be more nuanced with HH collection"
  HM_enterGlobalHeap ();
  LOG(s, DEBUG, true, L_DEBUG, "starting...");
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  Proc_beginCriticalSection(s);
  LOG(s, DEBUG, true, L_DEBUG, "locked");
  if (DEBUG)
    displayGCState (s, stderr);
  beginAtomic (s);
  assert (invariantForGC (s));
  LOG(s, DEBUG, true, L_DEBUG, "okay");
}

void leave (GC_state s) {
  LOG(s, DEBUG, true, L_DEBUG, "starting...");
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  assert (invariantForMutator (s, FALSE, TRUE));
  endAtomic (s);
  s->syncReason = SYNC_NONE;
  LOG(s, DEBUG, true, L_DEBUG, "okay");
  Proc_endCriticalSection(s);
  LOG(s, DEBUG, true, L_DEBUG, "unlocked");
#pragma message "This should be more nuanced with HH collection"
  HM_exitGlobalHeap();
}
