#include "processor.h"

#include <pthread.h>

/****************/
/* Global Types */
/****************/


/***************************/
/* Static Global Variables */
/***************************/
/* variables used in processor initialization */
static volatile bool Proc_beginInit = FALSE;
static volatile uint32_t Proc_initializedCount = 0;

/* variables used in Proc_{begin,end}CriticalSection() */
volatile uint32_t Proc_syncCount; /*
                                   * not static -- exposed for inlining
                                   * Proc_threadInSection()
                                   */
static volatile bool Proc_criticalTicketActive;
static volatile uint32_t Proc_criticalTicket;

/* variables used in Proc_BSP() */
static volatile enum BSPState Proc_bspState;

/* variables used in multiple places */
static struct rusage ru_crit;

/*************/
/* Constants */
/*************/
/* different start values to allow for comparison without arithmetic */
#define Proc_SYNC_COUNT_INITIALIZER 0
#define Proc_SYNC_COUNT_FIRST 1
#define Proc_CRITICAL_TICKET_ACTIVE_INITIALIZER FALSE
#define Proc_CRITICAL_TICKET_INITIALIZER 0
#define Proc_BSP_COUNT_INITIALIZER 0
#define Proc_BSP_COUNT_FIRST 1

/********************/
/* Static Functions */
/********************/
ATOMIC_STORE(BSPState, enum BSPState)
ATOMIC_LOAD(BSPState, enum BSPState)

/************************/
/* Function definitions */
/************************/

/* RAM_NOTE: Lack of barriers in these functions only works on x86! */

int32_t Proc_processorNumber (GC_state s) {
  return s->procNumber;
}

void Proc_waitForInitialization (GC_state s) {
  size_t pcounter = 0;
  while (!Proc_beginInit) {
    if (GC_MightCheckForTerminationRequest(s, &pcounter)) {
      GC_TerminateThread(s);
    }
  }

  __sync_add_and_fetch (&Proc_initializedCount, 1);

  while (!Proc_isInitialized (s)) {
    if (GC_MightCheckForTerminationRequest(s, &pcounter)) {
      GC_TerminateThread(s);
    }
  }
}

void Proc_signalInitialization (GC_state s) {
  Proc_syncCount = Proc_SYNC_COUNT_INITIALIZER;
  Proc_criticalTicketActive = Proc_CRITICAL_TICKET_ACTIVE_INITIALIZER;
  Proc_criticalTicket = Proc_CRITICAL_TICKET_INITIALIZER;
  Proc_bspState = DONE;

  Proc_initializedCount = 1;
  Proc_beginInit = TRUE;

  while (!Proc_isInitialized (s)) { }
}

bool Proc_isInitialized (GC_state s) {
  return Proc_initializedCount == s->numberOfProcs;
}

volatile uint8_t notifyAll = 0;
volatile int tot_cnt = 128; //YIFAN: for test
volatile int firstWakeThd = 0;

volatile int inCriticalSection = 0;

void Proc_beginCriticalSection (GC_state s, bool wakeAll, bool gcSleep) {
  static pthread_mutex_t Proc_syncCountLock = PTHREAD_MUTEX_INITIALIZER;
  static struct rusage ru_sync;

  Trace0(EVENT_GSECTION_BEGIN_ENTER);

  if (Proc_isInitialized (s)) {
    while (inCriticalSection) {}
    uint32_t myTicket = Proc_processorNumber (s);

    pthread_mutex_lock_safe(&Proc_syncCountLock);
    uint32_t mySyncCount = __sync_add_and_fetch(&Proc_syncCount, 1);

    if ((Proc_SYNC_COUNT_FIRST == mySyncCount) && needGCTime(s)) {
      /* first thread in this round, and need to keep track of sync time */
      startTiming (RUSAGE_SELF, &ru_sync);
    }

    //YIFAN added
    if (!__sync_fetch_and_or(&notifyAll, 1)) {
      firstWakeThd = 0;
      tot_cnt = s->numberOfProcs;
      bool flag = !wakeAll;
      for (int i = 0; i < s->numberOfProcs; i++) {
        GC_state si = &(s->procStates[i]);
        pthread_mutex_lock(&(si->slpMutex));
        if (si->mailSuspending) {
          if (wakeAll) {
            // sem_post(&(s->procStates[i].mailSem));
            pthread_cond_signal(&(si->mailCond));
          } else {
            tot_cnt --;
          }
        }
        if (si->llFlag == -1) {
          if (wakeAll) {
            pthread_cond_signal(&(si->llCond));
          } else {
            tot_cnt--;
          }
        }
        if (si->sleeping) {
          if (wakeAll) { // we can do nothing but wait for the thread
            pthread_mutex_unlock (&(si->slpMutex));
          } else {
            tot_cnt --;
          }
        }
        if (flag && !(si->mailSuspending || si->llFlag == -1 || si->sleeping)) {
          firstWakeThd = i;
          flag = false;
        }
        if (si->mailSuspending || si->llFlag == -1 || si->sleeping) { // 1st stage work when allocating array globally
        // printf("mailSuspend thd %d, status: %d, %d, %d\n", i, si->mailSuspending, si->llFlag, si->sleeping);
          GC_collect_sleep_1st(si, 0, 1);
        }
        if (!si->sleeping) {
          pthread_mutex_unlock (&(si->slpMutex));
        }
      }
      notifyAll = 3;
    }

    while (notifyAll != 3) {}
    if (mySyncCount == tot_cnt) {
      /* We are the last to synchronize, so signal this */
      if (needGCTime (s)) {
        /* deal with the timers */
        stopTiming (RUSAGE_SELF, &ru_sync, &s->cumulativeStatistics->ru_sync);
        startTiming (RUSAGE_SELF, &ru_crit);
      }
      Proc_criticalTicket = firstWakeThd;
      Proc_criticalTicketActive = TRUE;
      __sync_fetch_and_and(&notifyAll, 0); //reset lock var
      inCriticalSection = 1;
    }
    pthread_mutex_unlock_safe(&Proc_syncCountLock);

    /*
     * This allows for each processor to have its own critical section at each
     * round
     */
    /* RAM_NOTE: This really should be a condition variable */
    while ((!Proc_criticalTicketActive) || (Proc_criticalTicket != myTicket)) {}
  }
  else {
    Proc_syncCount = 1;
  }

  Trace0(EVENT_GSECTION_BEGIN_LEAVE);
}

void Proc_endCriticalSection (GC_state s, bool wakeAll) {
  Trace0(EVENT_GSECTION_END_ENTER);
  if (Proc_isInitialized (s)) {
    uint32_t myTicket = __sync_add_and_fetch (&Proc_criticalTicket, 1);
    GC_state si = &(s->procStates[myTicket]);

    if (!wakeAll) {
      while (myTicket < s->numberOfProcs && (si->mailSuspending || si->llFlag == -1 || si->sleeping)) {
        myTicket = __sync_add_and_fetch(&Proc_criticalTicket, 1);
        si++;
      }
      tot_cnt --;
    }

    bool flag = (!wakeAll && tot_cnt == 0) || (myTicket == s->numberOfProcs);
    if (flag) {
      /* We are the last to finish, so allow everyone to leave */

      if (needGCTime (s)) {
        /* deal with timing */
        stopTiming (RUSAGE_SELF, &ru_crit, &s->cumulativeStatistics->ru_crit);
      }
      
      if (!wakeAll) { // 3rd stage work. Port from global array allocation
        for (int i = 0; i < s->numberOfProcs; i++) {
          GC_state si = &(s->procStates[i]);
          if ((si->mailSuspending || si->llFlag == -1 || si->sleeping) && si->gcFlag) {
            si->gcFlag = false;
            HM_exitGlobalHeap_spec(si);
          }
          if (si->sleeping) {
            // we release lock here after sync to avoid threads' sleeping state changed during the sync
            pthread_mutex_unlock(&(si->slpMutex));
          }
        } 
      }

      /* reset for next round */
      Proc_syncCount = Proc_SYNC_COUNT_INITIALIZER;
      Proc_criticalTicket = Proc_CRITICAL_TICKET_INITIALIZER;
      Proc_criticalTicketActive = Proc_CRITICAL_TICKET_ACTIVE_INITIALIZER;
      __sync_synchronize ();
      inCriticalSection = 0;
    }

    /* RAM_NOTE: This should also be a condition variable */
    while (Proc_criticalTicketActive) {}
  }
  else {
    Proc_syncCount = 0;
  }
  Trace0(EVENT_GSECTION_END_LEAVE);
}

bool Proc_threadInSection (void) {
  return Proc_syncCount > Proc_SYNC_COUNT_INITIALIZER;
}

bool Proc_BSP(GC_state s,
              bspFunction* functions,
              size_t numFunctions,
              void** args) {
  static pthread_mutex_t Proc_bspCountLock = PTHREAD_MUTEX_INITIALIZER;
  static volatile uint32_t Proc_bspCount = Proc_BSP_COUNT_INITIALIZER;

  static struct rusage ru_sync;
  static struct rusage ru_bsp;

  static volatile bool initiatorStart = FALSE;
  static volatile bool participantStart = FALSE;
  static volatile size_t numParticipants;
  static volatile size_t numParticipantsFinished;
  static bspFunction * volatile sharedFunctions;
  static volatile size_t sharedNumFunctions;
  static void* * volatile sharedArgs;

  if (!Proc_isInitialized(s)) {
    DIE("Processors are not initialized!");
  }

  bool amInitiator = (NULL != functions);
  enum BSPState bspState = Proc_BSPState();
  if ((IN_PROGRESS == bspState) ||
      (amInitiator && (WAITING == bspState)) ||
      (!amInitiator && (DONE == bspState))) {
    return FALSE;
  }

  pthread_mutex_lock_safe(&Proc_bspCountLock);
  uint32_t myBSPCount = __sync_add_and_fetch(&Proc_bspCount, 1);

  if ((Proc_BSP_COUNT_FIRST != myBSPCount) && amInitiator) {
    /* I lost the BSP race */
    assert(WAITING == Proc_BSPState());
    __sync_sub_and_fetch(&Proc_bspCount, 1);
    pthread_mutex_unlock_safe(&Proc_bspCountLock);
    return FALSE;
  }

  if (Proc_BSP_COUNT_FIRST == myBSPCount) {
    if (!amInitiator) {
      /* participant joined a non-existent BSP round */
      pthread_mutex_unlock_safe(&Proc_bspCountLock);
      return FALSE;
    } else {
      assert(amInitiator);

      atomicStoreBSPState(&Proc_bspState, WAITING);

      if (needGCTime(s)) {
        /* first thread in this round, and need to keep track of sync time */
        startTiming (RUSAGE_SELF, &ru_sync);
      }
    }
  }

  // YIFAN added
  for (int i = 0; i < s->numberOfProcs; i++) {
    GC_state si = &(s->procStates[i]);
    if (si->mailSuspending) {
      pthread_cond_signal(&(si->mailCond));
    }
    if (si->llFlag == -1) {
      si->llFlag = -2;
      pthread_cond_signal(&(si->llCond));
    }
  }

  if (myBSPCount == s->numberOfProcs) {
    /* We are the last to synchronize, so signal this */
    if (needGCTime (s)) {
      /* deal with the timers */
      stopTiming (RUSAGE_SELF, &ru_sync, &s->cumulativeStatistics->ru_sync);
      startTiming (RUSAGE_SELF, &ru_bsp);
    }
    atomicStoreBSPState(&Proc_bspState, IN_PROGRESS);
    initiatorStart = TRUE;
  }
  pthread_mutex_unlock_safe(&Proc_bspCountLock);

  if (amInitiator) {
    /* I am the initiator for this BSP round */

    /* wait until everyone is synchronized */
    /* RAM_NOTE: This should also be a condition variable */
    while (FALSE == initiatorStart) { }

    /* setup the BSP */
    /* All BSP rounds start with all processors being participants */
    numParticipants = s->numberOfProcs;
    numParticipantsFinished = 0;
    sharedFunctions = functions;
    sharedNumFunctions = numFunctions;
    sharedArgs = args;

    /* start the BSP round */
    participantStart = TRUE;
  }

  /* wait until initiator starts the round */
  /* RAM_NOTE: This should also be a condition variable */
  while (FALSE == participantStart) { }

  /* cache constant shared values */
  functions = ((bspFunction*)(sharedFunctions));
  numFunctions = ((size_t)(sharedNumFunctions));
  args = ((void**)(sharedArgs));
  for (size_t i = 0; i < numFunctions; i++) {
    if (functions[i](args[i])) {
      /* I continue being a participant */
      __sync_add_and_fetch(&numParticipantsFinished, 1);
    } else {
      /* I am no longer a participant */
      break;
    }

    /*
     * wait until all participants have finished before moving onto the next
     * function
     */
    while (numParticipantsFinished < numParticipants) { }
  }
  /* I am done, so decrement number of participants */
  __sync_sub_and_fetch(&numParticipants, 1);

  if (amInitiator) {
    /*
     * As initiator, I need to stick around until the BSP is finished, even if
     * my participant "alter-ego" finished early
     */
    while (0 != numParticipants) { }

    /* reset for next BSP */
    initiatorStart = FALSE;
    participantStart = FALSE;
    numParticipants = 0;
    numParticipantsFinished = 0;
    sharedFunctions = NULL;
    sharedNumFunctions = 0;
    sharedArgs = NULL;

    /* stop timing */
    stopTiming (RUSAGE_SELF, &ru_bsp, &s->cumulativeStatistics->ru_bsp);

    atomicStoreBSPState(&Proc_bspState, DONE);
  }

  return TRUE;
}

enum BSPState Proc_BSPState(void) {
  return atomicLoadBSPState(&Proc_bspState);
}
