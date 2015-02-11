
#include <pthread.h>

int32_t Proc_processorNumber (GC_state s) {
  for (int proc = 0; proc < s->numberOfProcs; proc ++) {
    if (s == &(s->procStates[proc])) {
      return (int32_t)proc;
    }
  }

  /* SPOONHOWER_NOTE: shouldn't get here */
  fprintf (stderr, "don't know my own processor number (signals?)\n");
  exit (1);
  return 0;
}

bool Proc_amPrimary (GC_state s) {
  return Proc_processorNumber (s) == 0;
}

volatile bool Proc_beginInit = FALSE;
volatile int32_t Proc_initialized = 0;
volatile int32_t Proc_criticalCount;
volatile int32_t Proc_criticalTicket;

void Proc_waitForInitialization (GC_state s) {
  while (!Proc_beginInit) { }

  __sync_fetch_and_add (&Proc_initialized, 1);

  while (!Proc_isInitialized (s)) { }
}

void Proc_signalInitialization (GC_state s) {

  Proc_criticalTicket = -1;
  Proc_criticalCount = 0;

  Proc_initialized = 1;
  Proc_beginInit = TRUE;

  while (!Proc_isInitialized (s)) { }
}

bool Proc_isInitialized (GC_state s) {
  return Proc_initialized == s->numberOfProcs;
}

struct rusage rusage_rt;
struct rusage rusage_sync;

void Proc_beginCriticalSection (GC_state s) {
  if (Proc_isInitialized (s)) {
    int32_t myNumber = Proc_processorNumber (s);
    int32_t p = __sync_add_and_fetch (&Proc_criticalCount, 1);

    if (p == 1) {
      /* We are the first thread in this round. */
      if (needGCTime (s))
        startTiming (RUSAGE_SELF, &rusage_sync);

      switch (s->syncReason) {
      case SYNC_NONE:
        fprintf (stderr, "Got to begin without a reason?\n");
        exit (1);
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
        fprintf (stderr, "Unknown sync reason?\n");
        exit (1);
      }
    }

    if (p == s->numberOfProcs) {
      /* We are the last to syncronize */
      if (needGCTime (s)) {
        stopTiming (RUSAGE_SELF, &rusage_sync, &s->cumulativeStatistics->ru_sync);
        startTiming (RUSAGE_SELF, &rusage_rt);
      }
      Proc_criticalTicket = 0;
    }

    /*
     * RAM_NOTE: Threads unlock one at a time as checking per-thread GC
     * invariants needs to be done one-by-one?
     */
    while (Proc_criticalTicket != myNumber) {}
  }
  else {
    Proc_criticalCount = 1;
  }
}

void Proc_endCriticalSection (GC_state s) {
  if (Proc_isInitialized (s)) {
    int32_t p = __sync_add_and_fetch (&Proc_criticalTicket, 1);
    if (p == s->numberOfProcs) {
      /* We are the last to finish */

      if (needGCTime (s))
        stopTiming (RUSAGE_SELF, &rusage_rt, &s->cumulativeStatistics->ru_rt);

      Proc_criticalCount = 0;
      Proc_criticalTicket = -1;
      __sync_synchronize ();
    }

    while (Proc_criticalTicket >= 0) {}
  }
  else {
    Proc_criticalCount = 0;
  }
}

bool Proc_threadInSection (void) {
  return Proc_criticalCount > 0;
}
