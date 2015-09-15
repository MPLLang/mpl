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
  LOG(DEBUG, true, L_DEBUG, "starting...");

#pragma message "Delete when confirmed correct"
#if 0
#pragma message "This should be more nuanced with HH collection"
  HM_enterGlobalHeap ();
#endif

  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  Proc_beginCriticalSection(s);
  beginAtomic (s);

  LOG(DEBUG, true, L_DEBUG, "locked");
  if (DEBUG)
    displayGCState (s, stderr);

#pragma message "Adjust this invariant to take into account not-in-GH entries"
#if 0
    assert (invariantForGC (s));
#endif

  LOG(DEBUG, true, L_DEBUG, "okay");

  switch (s->syncReason) {
    case SYNC_NONE:
      /* I am just a passenger on this critical section */
      break;
    case SYNC_OLD_GEN_ARRAY:
      s->cumulativeStatistics->syncForOldGenArray++;
      break;
    case SYNC_NEW_GEN_ARRAY:
      s->cumulativeStatistics->syncForNewGenArray++;
      break;
    case SYNC_STACK:
      s->cumulativeStatistics->syncForStack++;
      break;
    case SYNC_HEAP:
      s->cumulativeStatistics->syncForHeap++;
      break;
    case SYNC_FORCE:
      s->cumulativeStatistics->syncMisc++;
      break;
    case SYNC_PACK:
      s->cumulativeStatistics->syncMisc++;
      break;
    case SYNC_SAVE_WORLD:
      s->cumulativeStatistics->syncMisc++;
      break;
    default:
      DIE("Unknown sync reason!");
  }
}

void leave (GC_state s) {
  LOG(DEBUG, true, L_DEBUG, "starting...");
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  /* Taken from assert (invariantForMutator (s, FALSE, TRUE)); */
  if (DEBUG) {
    displayGCState (s, stderr);
  }
  assert (invariantForMutatorStack(s));

#pragma message "Adjust this invariant to take into account not-in-GH entries"
#if 0
  assert (invariantForGC (s));
#endif

  endAtomic (s);
  s->syncReason = SYNC_NONE;
  LOG(DEBUG, true, L_DEBUG, "okay");
  Proc_endCriticalSection(s);
  LOG(DEBUG, true, L_DEBUG, "unlocked");
#pragma message "Delete when confirmed correct"
#if 0
#pragma message "This should be more nuanced with HH collection"
  HM_exitGlobalHeap();
#endif
}
