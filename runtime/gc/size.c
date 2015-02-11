/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t GC_size (GC_state s, pointer root) {
  size_t res;

  s->syncReason = SYNC_STACK;
  ENTER0 (s); /* update stack in heap, in case it is reached */
  if (DEBUG_SIZE)
    fprintf (stderr,
             "GC_size marking [%d]\n",
             Proc_processorNumber (s));
  res = dfsMarkByMode (s, root, MARK_MODE, FALSE, FALSE);
  if (DEBUG_SIZE)
    fprintf (stderr,
             "GC_size unmarking [%d]\n",
             Proc_processorNumber (s));
  dfsMarkByMode (s, root, UNMARK_MODE, FALSE, FALSE);
  LEAVE0 (s);

  return res;
}
