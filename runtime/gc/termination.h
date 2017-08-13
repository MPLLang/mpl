/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* AG_NOTE: probably not here */
#define INVALID_PROCESSOR_NUMBER UINT32_MAX

/* AG_NOTE: probably not here */
typedef _Atomic(uint32_t) atomic_uint32_t;

/* This function terminates the current thread.
 */
PRIVATE __attribute__ ((noreturn)) void GC_TerminateThread(GC_state s);

/* This function returns true when some processor is trying to terminate the
 * program.
 */
PRIVATE bool GC_CheckForTerminationRequest(GC_state s);

/* This function terminates the current thread when some processor is trying to
 * terminate the whole program. */
PRIVATE void GC_MayTerminateThread(GC_state s);

/* This convenience function checks for a termination request only from time to
 * time, when its second reaches some treshold. It should be called in busy
 * waiting loops in order to prevent deadlocks. You do not need to increment the
 * counter yourself. */
PRIVATE bool GC_CheckForTerminationRequestRarely(GC_state s, size_t *pcounter);

/* This function is similar to GC_CheckForTerminationRequestRarely(), but
   actually terminates the current thread when a termination request is
   detected. */
PRIVATE void GC_MayTerminateThreadRarely(GC_state s, size_t *pcounter);

/* This function tries to terminate the whole program by signaling other
 * threads. Fails if another processor beat us to it. Only returns true after
 * all all other processors have exited cleanly.
 */
PRIVATE bool GC_TryToTerminate(GC_state s);
