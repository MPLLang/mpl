/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_share (GC_state s, pointer object) {
  size_t bytesExamined;
  size_t bytesHashConsed;

  s->syncReason = SYNC_STACK;
  ENTER0(s); /* update stack in heap, in case it is reached */
  if (DEBUG_SHARE)
    fprintf (stderr, "GC_share "FMTPTR" [%d]\n", (uintptr_t)object,
             Proc_processorNumber (s));
  if (DEBUG_SHARE or s->controls->messages)
    s->lastMajorStatistics->bytesHashConsed = 0;
  // Don't hash cons during the first round of marking.
  bytesExamined = dfsMarkByMode (s, object, MARK_MODE, FALSE, FALSE);
  s->objectHashTable = allocHashTable (s);
  // Hash cons during the second round of (un)marking.
  dfsMarkByMode (s, object, UNMARK_MODE, TRUE, FALSE);
  freeHashTable (s->objectHashTable);
  bytesHashConsed = s->lastMajorStatistics->bytesHashConsed;
  s->cumulativeStatistics->bytesHashConsed += bytesHashConsed;
  if (DEBUG_SHARE or s->controls->messages)
    printBytesHashConsedMessage (bytesHashConsed, bytesExamined);
  LEAVE0(s);
}
