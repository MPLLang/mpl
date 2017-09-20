/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* Concurrent termination is based on a simple protocol: the pthread/processor
 * that wishes to terminate the program tries to write its number into the
 * "terminationLeader" field of the first processor's GC state. Processors may
 * check this field to see if it holds a valid processor number, indicating that
 * termination has been requested.
 */

atomic_uint32_t *pleader(GC_state s);
atomic_uint32_t *pleader(GC_state s) {
  return &s->procStates[0].terminationLeader;
}

void GC_TerminateThread(GC_state s) {
  GC_PthreadAtExit(s);
  Trace0(EVENT_RUNTIME_LEAVE);
  pthread_exit(NULL);
}

bool GC_CheckForTerminationRequest(GC_state s) {
  if (s->procStates == NULL)
    return false;

  uint32_t leader = atomic_load_explicit(pleader(s), memory_order_acquire);
  bool in_progress = leader != INVALID_PROCESSOR_NUMBER;

  if (in_progress)
    Trace0(EVENT_HALT_ACK);

  return in_progress;
}

bool GC_MightCheckForTerminationRequest(GC_state s, size_t *pcounter) {
  (*pcounter)++;
  if (*pcounter < 10000)
    return false;

  *pcounter = 0;
  return GC_CheckForTerminationRequest(s);
}

bool GC_TryToTerminate(GC_state s) {
  Trace0(EVENT_HALT_REQ);

  /* We can always terminate immediately when there is a single processor. */
  if (s->procStates == NULL)
    return true;

  uint32_t myself = Proc_processorNumber(s), inval = INVALID_PROCESSOR_NUMBER;

  /* If the CAS succeeds, we won the race and can tell the other threads to
   * terminate. Otherwise, another processor has won the race and will lead the
   * termination protocol. */
  if (!atomic_compare_exchange_strong(pleader(s), &inval, myself))
    return false;

  /* Force all processors to acknowledge termination. */
  for (uint32_t p = 0; p < s->numberOfProcs; p++)
    s->procStates[p].limit = 0;

  Trace0(EVENT_HALT_WAIT);

  /* Wait for the other processors to terminate. */
  for (uint32_t p = 0; p < s->numberOfProcs; p++)
    if (p != myself)
      if (pthread_join(s->procStates[p].self, NULL) != 0) {
        perror("pthread_join");
        exit(1);
      }

  return true;
}

PRIVATE void GC_PthreadAtExit(GC_state s) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  /*
   * RAM_NOTE: This is for global heap stat tracking. I may need to update HH
   * stats also
   */
  HM_enterGlobalHeap ();
}
