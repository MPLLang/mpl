/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* AG_NOTE: probably not here */
#define INVALID_PROCESSOR_NUMBER UINT32_MAX

/* AG_NOTE: probably not here */
typedef _Atomic(uint32_t) atomic_uint32_t;

/* This function returns true when some processor is trying to terminate the
 * program.
 */
PRIVATE bool GC_CheckForTerminationRequest(GC_state s);

/* This convenience function checks for a termination request only from time to
 * time, when its second reaches some treshold. It should be called in busy
 * waiting loops in order to prevent deadlocks. You do not need to increment the
 * counter yourself. */
PRIVATE bool GC_MightCheckForTerminationRequest(GC_state s, size_t *pcounter);

/* This function tries to terminate the whole program by signaling other
 * threads. Fails if another processor beat us to it. Only returns true after
 * all all other processors have exited cleanly.
 */
PRIVATE bool GC_TryToTerminate(GC_state s);

/**
 * This function does some wrap-up for statistics. It should be called
 * before the thread exits.
 *
 * @param s The GC_state of the thread that will exit.
 */
PRIVATE void GC_PthreadAtExit(GC_state s);
