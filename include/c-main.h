/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "common-main.h"
#include "c-common.h"

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
  return (GC_frameIndex)ra;
}

#define MLtonCallFromC                                                  \
static void MLton_callFromC (void* ffiArgs) {                           \
  struct cont cont;                                                     \
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
  cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);    \
  cont.nextChunk = nextChunks[cont.nextFun];                            \
  s->returnToC = FALSE;                                                 \
  do {                                                                  \
    cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);    \
  } while (not s->returnToC);                                           \
  s->returnToC = FALSE;                                                 \
  s->atomicState += 1;                                                  \
  GC_switchToThread (s, GC_getSavedThread (), 0);                       \
  s->atomicState -= 1;                                                  \
  if (0 == s->atomicState                                               \
      && s->signalsInfo.signalIsPending)                                \
    s->limit = 0;                                                       \
  if (DEBUG_CCODEGEN)                                                   \
    fprintf (stderr, "MLton_callFromC done\n");                         \
}

#define MLtonThreadFunc(mc, ml)                                         \
  void threadn (void* arg) {                                            \
  set_cpu_affinity(0);                                                  \
    while(1) { usleep(1000); } }                                        \
void MLton_threadFunc (void* arg) {                                     \
  struct cont cont;                                                     \
  GC_state s = (GC_state)arg;                                           \
  uint32_t num = (Proc_processorNumber (s))        \
      * s->controls->affinityStride                                     \
      + s->controls->affinityBase;                                      \
  if ((s->workersPerProc == 2) &&                                       \
	(Proc_processorNumber(s) == s->numberOfProcs - 1))              \
    num--;                                                              \
  set_cpu_affinity(num);                                                \
                                                                        \
  /* Save our state locally */                                          \
  pthread_setspecific (gcstate_key, s);                                 \
  if (s->amOriginal) {                                                  \
    real_Init();                                                        \
    PrepFarJump(cont, mc, ml);                                          \
  } else {                                                              \
    /* Return to the saved world */                                     \
    cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);  \
    cont.nextChunk = nextChunks[cont.nextFun];                          \
  }                                                                     \
  /* Check to see whether or not we are the first thread */             \
  if (Proc_amPrimary (s)) {                                             \
    /* Trampoline */                                                    \
    while (1) {                                                         \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
      cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun);  \
    }                                                                   \
  }                                                                     \
  else {                                                                \
    printf("Starting on %d!\n", Proc_processorNumber (s));               \
    Proc_waitForInitialization (s);                                     \
    Parallel_run ();                                                    \
  }                                                                     \
}


#define MLtonMain(al, mg, mfs, mmc, pk, ps, gnr, mc, ml)                \
  /* Globals */                                                         \
  C_Pthread_Key_t gcstate_key;                                          \
                                                                        \
  MLtonCallFromC                                                        \
                                                                        \
  MLtonThreadFunc(mc, ml)                                               \
                                                                        \
  PUBLIC int MLton_main (int argc, char* argv[]) {                      \
    int procNo;                                                         \
    pthread_t *threads;                                                 \
    {                                                                   \
      struct GC_state s;                                                \
      /* Initialize with a generic state to read in @MLtons, etc */     \
      Initialize (s, al, mg, mfs, mmc, pk, ps, gnr);                    \
                                                                        \
      threads = (pthread_t *) malloc ((s.numberOfProcs) * sizeof (pthread_t)); \
      gcState = (GC_state) malloc (s.numberOfProcs * sizeof (struct GC_state)); \
      /* Create key */                                                  \
      if (pthread_key_create(&gcstate_key, NULL)) {                     \
        fprintf (stderr, "pthread_key_create failed: %s\n", strerror (errno)); \
        exit (1);                                                       \
      }                                                                 \
      /* Now copy initialization to the first processor state */        \
      memcpy (&gcState[0], &s, sizeof (struct GC_state));               \
      gcState[0].procStates = gcState;                                  \
      GC_lateInit (&gcState[0]);                                        \
    }                                                                   \
    /* Fill in per-processor data structures */                         \
    for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) {     \
      Duplicate (&gcState[procNo], &gcState[0]);                        \
      gcState[procNo].procStates = gcState;                             \
    }                                                                   \
    pthread_attr_t attr;                                                \
    pthread_attr_init(&attr);                                            \
    /* pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED); */  \
    /* pthread_attr_setschedpolicy(&attr, SCHED_RR);  */                \
    /* Now create the threads */                                        \
    for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) {     \
      if (pthread_create (&threads[procNo - 1], &attr, &MLton_threadFunc, (void *)&gcState[procNo])) { \
        fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
        exit (1);                                                       \
      }                                                                 \
    }                                                                   \
    pthread_create (&threads[procNo], NULL, &threadn, NULL);            \
    MLton_threadFunc ((void *)&gcState[0]);                             \
  }

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, mc, ml)                  \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
  struct cont cont;                                                     \
  Initialize (al, mg, mfs, mmc, pk, ps);                                \
  if (gcState.amOriginal) {                                             \
    real_Init();                                                        \
    PrepFarJump(mc, ml);                                                \
  } else {                                                              \
    /* Return to the saved world */                                     \
    nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE);  \
    cont.nextChunk = nextChunks[nextFun];                               \
  }                                                                     \
  /* Trampoline */                                                      \
  returnToC = FALSE;                                                    \
  do {                                                                  \
    cont=(*(struct cont(*)(void))cont.nextChunk)();                     \
  } while (not returnToC);                                              \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
  struct cont cont;                                                     \
  nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE);    \
  cont.nextChunk = nextChunks[nextFun];                                 \
  returnToC = FALSE;                                                    \
  do {                                                                  \
    cont=(*(struct cont(*)(void))cont.nextChunk)();                     \
  } while (not returnToC);                                              \
  GC_done(&gcState);                                                    \
}

#endif /* #ifndef _C_MAIN_H */
