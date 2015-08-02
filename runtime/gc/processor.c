#include "processor.h"

#include <pthread.h>

/***************************/
/* Static Global Variables */
/***************************/

static volatile bool Proc_beginInit = FALSE;
static volatile int32_t Proc_initializedCount = 0;
static volatile int32_t Proc_syncCount;
static volatile int32_t Proc_criticalTicket;

static struct rusage ru_crit;

/*************/
/* Constants */
/*************/
static const int32_t Proc_SYNC_COUNT_INITIALIZER = 0;
static const int32_t Proc_SYNC_COUNT_FIRST = 1;
static const int32_t Proc_CRITICAL_TICKET_INITIALIZER = -1;

/************************/
/* Function definitions */
/************************/

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

void Proc_waitForInitialization (GC_state s) {
  while (!Proc_beginInit) { }

  __sync_fetch_and_add (&Proc_initializedCount, 1);

  while (!Proc_isInitialized (s)) { }
}

void Proc_signalInitialization (GC_state s) {

  /* different start values to allow for comparison without arithmetic */
  Proc_syncCount = Proc_SYNC_COUNT_INITIALIZER;
  Proc_criticalTicket = Proc_CRITICAL_TICKET_INITIALIZER;

  Proc_initializedCount = 1;
  Proc_beginInit = TRUE;

  while (!Proc_isInitialized (s)) { }
}

bool Proc_isInitialized (GC_state s) {
  return Proc_initializedCount == s->numberOfProcs;
}

void Proc_beginCriticalSection (GC_state s) {
  static struct rusage ru_sync;

  if (Proc_isInitialized (s)) {
    int32_t myTicket = Proc_processorNumber (s);
    int32_t mySyncCount = __sync_add_and_fetch (&Proc_syncCount, 1);

    if ((Proc_SYNC_COUNT_FIRST == mySyncCount) && needGCTime(s)) {
      /* first thread in this round, and need to keep track of sync time */
      startTiming (RUSAGE_SELF, &ru_sync);
    }

    if (mySyncCount == s->numberOfProcs) {
      /* We are the last to synchronize, so signal this */
      if (needGCTime (s)) {
        /* deal with the timers */
        stopTiming (RUSAGE_SELF, &ru_sync, &s->cumulativeStatistics->ru_sync);
        startTiming (RUSAGE_SELF, &ru_crit);
      }
      Proc_criticalTicket = 0;
    }

    /*
     * This allows for each processor to have its own critical section at each
     * round
     */
    /* RAM_NOTE: This really should be a condition variable */
    while (Proc_criticalTicket != myTicket) {}
  }
  else {
    Proc_syncCount = 1;
  }
}

void Proc_endCriticalSection (GC_state s) {
  if (Proc_isInitialized (s)) {
    int32_t myTicket = __sync_add_and_fetch (&Proc_criticalTicket, 1);
    if (myTicket == s->numberOfProcs) {
      /* We are the last to finish, so allow everyone to leave */

      if (needGCTime (s)) {
        /* deal with timing */
        stopTiming (RUSAGE_SELF, &ru_crit, &s->cumulativeStatistics->ru_crit);
      }

      /* reset for next round */
      Proc_syncCount = Proc_SYNC_COUNT_INITIALIZER;
      Proc_criticalTicket = Proc_CRITICAL_TICKET_INITIALIZER;
      __sync_synchronize ();
    }

    /* RAM_NOTE: This should also be a condition variable */
    while (Proc_criticalTicket >= 0) {}
  }
  else {
    Proc_syncCount = 0;
  }
}

bool Proc_threadInSection (void) {
  return Proc_syncCount > Proc_SYNC_COUNT_INITIALIZER;
}
