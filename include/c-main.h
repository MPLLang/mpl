/* Copyright (C) 2019 Matthew Fluet.
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

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
  return (GC_frameIndex)ra;
}

static void MLtonGCCleanup(void *arg) {
    GC_state s = (GC_state)arg;
    GC_traceFinish(s);
}

#define MLtonCallFromC()                                                \
static void MLton_callFromC (void* ffiArgs) {                           \
  uintptr_t nextBlock;                                                  \
  GC_state s = pthread_getspecific (gcstate_key);                       \
                                                                        \
  if (DEBUG_CCODEGEN)                                                   \
    fprintf (stderr, "MLton_callFromC() starting\n");                   \
  GC_setSavedThread (GC_getCurrentThread ());                           \
  s->atomicState += 3;                                                  \
  s->ffiArgs = ffiArgs;                                                 \
  if (s->signalsInfo.signalIsPending)                                   \
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;                   \
  /* Switch to the C Handler thread. */                                 \
  GC_switchToThread (s, GC_getCallFromCHandlerThread (), 0);            \
  nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);       \
  do {                                                                  \
    nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
  } while (nextBlock != (uintptr_t)-1);                                 \
  s->atomicState += 1;                                                  \
  GC_switchToThread (s, GC_getSavedThread (), 0);                       \
  s->atomicState -= 1;                                                  \
  if (0 == s->atomicState                                               \
      && s->signalsInfo.signalIsPending)                                \
    s->limit = 0;                                                       \
  if (DEBUG_CCODEGEN)                                                   \
    fprintf (stderr, "MLton_callFromC done\n");                         \
}

#define MLtonThreadFunc(ml)                                             \
void MLton_threadFunc (void* arg) {                                     \
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
    real_Init();                                                        \
    nextBlock = ml;                                                     \
  } else {                                                              \
    /* Return to the saved world */                                     \
    nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);     \
  }                                                                     \
  /* Check to see whether or not we are the first thread */             \
  if (Proc_processorNumber (s) == 0) {                                  \
    Trace0(EVENT_LAUNCH);                                               \
    /* Trampoline */                                                    \
    do {                                                                \
      nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
    } while (1);                                                        \
  }                                                                     \
  else {                                                                \
    Proc_waitForInitialization (s);                                     \
    Trace0(EVENT_LAUNCH);                                               \
    Parallel_run ();                                                    \
  }                                                                     \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, gnr, ml)                    \
  /* Globals */                                                         \
  C_Pthread_Key_t gcstate_key;                                          \
                                                                        \
  MLtonThreadFunc(ml)                                                   \
                                                                        \
  PUBLIC int MLton_main (int argc, char* argv[]) {                      \
    int procNo;                                                         \
    pthread_t *threads;                                                 \
    {                                                                   \
      struct GC_state s;                                                \
      /* Initialize with a generic state to read in @MLtons, etc */     \
      Initialize (s, al, mg, mfs, mmc, pk, ps, gnr);                    \
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

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, mc, ml)                  \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
  uintptr_t nextBlock;                                                  \
  Initialize (al, mg, mfs, mmc, pk, ps);                                \
  if (gcState.amOriginal) {                                             \
    real_Init();                                                        \
    nextBlock = ml;                                                     \
  } else {                                                              \
    /* Return to the saved world */                                     \
    nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);     \
  }                                                                     \
  /* Trampoline */                                                      \
  do {                                                                  \
     nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
   } while (nextBlock != (uintptr_t)-1);                                \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
  uintptr_t nextBlock;                                                  \
  uintptr_t nextBlock;                                                  \
  GC_state s = &gcState;                                                \
  nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);       \
  do {                                                                  \
    nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
  } while (nextBlock != (uintptr_t)-1);                                 \
  GC_done(&gcState);                                                    \
}

#endif /* #ifndef _C_MAIN_H */
