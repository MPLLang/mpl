/* Copyright (C) 2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "common-main.h"
#include "c-common.h"

void Parallel_run (void);

PRIVATE C_Pthread_Key_t gcstate_key;

PRIVATE GC_state MLton_gcState() {
  return pthread_getspecific (gcstate_key);
}

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
  return (GC_frameIndex)ra;
}

static inline uintptr_t getNextBlockFromStackTop (GC_state s) {
  return *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);
}

PRIVATE uintptr_t MLton_unreachable() { return -2; }

PRIVATE extern ChunkFnPtr_t const nextChunks[];

static inline void MLton_trampoline (GC_state s, uintptr_t nextBlock, bool mayReturnToC) {
        do {
                nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock);
        } while (!mayReturnToC || nextBlock != (uintptr_t)-1);
}

static void MLtonGCCleanup(void *arg) {
    GC_state s = (GC_state)arg;
    GC_traceFinish(s);
}

#define MLtonCallFromC()                                                \
PRIVATE uintptr_t Thread_returnToC() { return -1; }                     \
static void MLton_callFromC (CPointer localOpArgsResPtr) {              \
  uintptr_t nextBlock;                                                  \
  GC_state s = MLton_gcState();                                         \
  /*printf("[%d] MLton_callFromC\n", s->procNumber);*/                      \
  if (DEBUG_CCODEGEN)                                                   \
    fprintf (stderr, "MLton_callFromC() starting\n");                   \
  s->callFromCOpArgsResPtr = localOpArgsResPtr;                         \
  GC_setSavedThread (s, GC_getCurrentThread (s));                       \
  s->atomicState += 3;                                                  \
  if (s->signalsInfo.signalIsPending)                                   \
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;                   \
  /* Switch to the C Handler thread. */                                 \
  GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);           \
  nextBlock = getNextBlockFromStackTop (s);                             \
  MLton_trampoline (s, nextBlock, TRUE);                                \
  s->atomicState += 1;                                                  \
  GC_switchToThread (s, GC_getSavedThread (s), 0);                      \
  s->atomicState -= 1;                                                  \
  if (0 == s->atomicState                                               \
      && s->signalsInfo.signalIsPending)                                \
    s->limit = 0;                                                       \
  if (DEBUG_CCODEGEN)                                                   \
    fprintf (stderr, "MLton_callFromC done\n");                         \
}

#define MLtonThreadFunc(ml)                                             \
void *MLton_threadFunc (void* arg) {                                    \
  uintptr_t nextBlock;                                                  \
  GC_state s = (GC_state)arg;                                           \
                                                                        \
                                                                        \
  /* Do not set CPU affinity when running on a single processor  */     \
  if (s->controls->setAffinity && s->numberOfProcs > 1) {               \
      uint32_t num = Proc_processorNumber (s)                           \
          * s->controls->affinityStride                                 \
          + s->controls->affinityBase;                                  \
      set_cpu_affinity(num);                                            \
  }                                                                     \
                                                                        \
  /* Save our state locally */                                          \
  if (s->procNumber != 0) {                                             \
    pthread_setspecific (gcstate_key, s);                               \
  }                                                                     \
  if (s->amOriginal) {                                                  \
    nextBlock = ml;                                                     \
  } else {                                                              \
    /* Return to the saved world */                                     \
    nextBlock = getNextBlockFromStackTop (s);                           \
  }                                                                     \
  /* Check to see whether or not we are the last thread; this assigns   \
   * the "main" computation to the last thread, so that we can use the  \
   * first thread as the signal relayer if necessary.                   \
   */                                                                   \
  if (Proc_processorNumber (s) == 0) {                                  \
    Trace0(EVENT_LAUNCH);                                               \
    /* Trampoline */                                                    \
    MLton_trampoline (s, nextBlock, FALSE);                             \
  }                                                                     \
  else if (s->numberOfProcs > s->controls->heartbeatRelayerThreshold    \
           && s->numberOfProcs >= 2                                     \
           && Proc_processorNumber(s) == s->numberOfProcs-1)            \
  {                                                                     \
    Proc_waitForInitialization(s);                                      \
    HH_EBR_enterQuiescentState(s);                                      \
    relayerLoop(s);                                                     \
  }                                                                     \
  else {                                                                \
    Proc_waitForInitialization (s);                                     \
    Trace0(EVENT_LAUNCH);                                               \
    /*printf("[%d] calling Parallel_run\n", s->procNumber);*/           \
    Parallel_run ();                                                    \
  }                                                                     \
  return (void *)1;                                                     \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, ml)                         \
  MLtonThreadFunc(ml)                                                   \
                                                                        \
  PUBLIC int MLton_main (int argc, char* argv[]) {                      \
    int procNo;                                                         \
    GC_state gcState;                                                   \
    pthread_t *threads;                                                 \
    {                                                                   \
      struct GC_state s;                                                \
      /* Initialize with a generic state to read in @MLtons, etc */     \
      Initialize ((&s), al, mg, mfs, mmc, pk, ps);                      \
                                                                        \
      gcState = (GC_state) malloc (s.numberOfProcs * sizeof (struct GC_state)); \
      /* Create key */                                                  \
      if (pthread_key_create(&gcstate_key, MLtonGCCleanup)) {           \
        fprintf (stderr, "pthread_key_create failed: %s\n", strerror (errno)); \
        exit (1);                                                       \
      }                                                                 \
      /* Now copy initialization to the first processor state */        \
      memcpy (&gcState[0], &s, sizeof (struct GC_state));               \
      gcState[0].procStates = gcState;                                  \
      gcState[0].procNumber = 0;                                        \
      pthread_setspecific(gcstate_key, &gcState[0]);                    \
      GC_lateInit (&gcState[0]);                                        \
    }                                                                   \
    /* Fill in per-processor data structures */                         \
    for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) {     \
      Duplicate (&gcState[procNo], &gcState[0]);                        \
      gcState[procNo].procStates = gcState;                             \
      gcState[procNo].procNumber = procNo;                              \
    }                                                                   \
    /* Set up tracing infrastructure */                                 \
    for (procNo = 0; procNo < gcState[0].numberOfProcs; procNo++)       \
        GC_traceInit(&gcState[procNo]);                                 \
    /* Now create the threads */                                        \
    for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) {     \
      if (pthread_create (&gcState[procNo].self, NULL, &MLton_threadFunc, (void *)&gcState[procNo])) { \
        fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
        exit (1);                                                       \
      }                                                                 \
    }                                                                   \
    MLton_threadFunc ((void *)&gcState[0]);                             \
  }

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, ml)                      \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
  uintptr_t nextBlock;                                                  \
  GC_state s = MLton_gcState();                                         \
  Initialize (s, al, mg, mfs, mmc, pk, ps);                             \
  if (s->amOriginal) {                                                  \
    nextBlock = ml;                                                     \
  } else {                                                              \
    /* Return to the saved world */                                     \
    nextBlock = getNextBlockFromStackTop (s);                           \
  }                                                                     \
  MLton_trampoline (s, nextBlock, TRUE);                                \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
  uintptr_t nextBlock;                                                  \
  GC_state s = MLton_gcState();                                         \
  nextBlock = getNextBlockFromStackTop (s);                             \
  MLton_trampoline (s, nextBlock, TRUE);                                \
  GC_done(s);                                                           \
}

#endif /* #ifndef _C_MAIN_H */
