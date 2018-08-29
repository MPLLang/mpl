#include <pthread.h>
#include <time.h>
#include "platform.h"
#include <unistd.h>
#include <semaphore.h>
#include <time.h>
#include <math.h>

/* num of holding thread or -1 if no one*/
volatile int32_t *Parallel_mutexes;
//YIFAN: stats for probing the work stealing scheduler
static int waitCnt = 0;
static int probe = 0;

void Parallel_init (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  //YIFAN: init the probe stats vars
  waitCnt = 0;
  probe = 0;

  if (!Proc_isInitialized (s)) {
    Parallel_mutexes = (int32_t *) malloc (s->numberOfProcs * sizeof (int32_t));

    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      Parallel_mutexes[proc] = -1;
    }
    /* Now wake them up! */
    Proc_signalInitialization (s);
  }
}

void Parallel_yield (void) {
  if (Proc_threadInSection ()) {
    GC_state s = pthread_getspecific (gcstate_key);
    ENTER0 (s);
    LEAVE0 (s);
  }
}

/* lock = int* >= 0 if held or -1 if no one */

void Parallel_lockInit (Pointer arg) {
  spinlock_t* lock = ((spinlock_t*)(arg));
  spinlock_init(lock);
}

void Parallel_lockTake (Pointer arg) {
  spinlock_t* lock = ((spinlock_t*)(arg));
  uint32_t lockValue = Proc_processorNumber(pthread_getspecific(gcstate_key));
  GC_state s = pthread_getspecific (gcstate_key);
  size_t cpoll = 0;

  LOG(LM_PARALLEL, LL_DEBUG,
      "trying to lock %p to %u",
      ((volatile void*)(lock)),
      lockValue);
  Trace1(EVENT_LOCK_TAKE_ENTER, (EventInt)lock);

  do {
    if (GC_MightCheckForTerminationRequest(s, &cpoll)) {
      Trace1(EVENT_LOCK_TAKE_LEAVE, (EventInt)lock);
      GC_TerminateThread(s);
    }

    if (Proc_threadInSection ()) {
      ENTER1(s, arg);
      LEAVE1(s, arg);
      lock = ((spinlock_t*)(arg));
    }
  } while (!spinlock_trylock(lock, lockValue));

  LOG(LM_PARALLEL, LL_DEBUG,
      "locked");
  Trace1(EVENT_LOCK_TAKE_LEAVE, (EventInt)lock);
}

void Parallel_lockRelease (Pointer arg) {
  spinlock_t* lock = ((spinlock_t*)(arg));

  LOG(LM_PARALLEL, LL_DEBUG,
      "releasing %p",
      ((volatile void*)(lock)));
  WITH_GCSTATE(Trace1(EVENT_LOCK_RELEASE, (EventInt)lock));

  spinlock_unlock(lock);
}

bool Parallel_alreadyLockedByMe (Pointer arg) {
  spinlock_t* lock = ((spinlock_t*)(arg));
  uint32_t lockValue = Proc_processorNumber(pthread_getspecific(gcstate_key));

  return (lockValue == spinlock_value(lock));
}

void Parallel_dekkerTake (Bool amLeft, Pointer left, Pointer right, Pointer leftsTurn_)
{
  Bool *mine, *other, *leftsTurn;
  GC_state s = pthread_getspecific (gcstate_key);
  size_t cpoll = 0;

  if (amLeft) {
    mine = (Bool *)left;
    other = (Bool *)right;
  }
  else {
    mine = (Bool *)right;
    other = (Bool *)left;
  }
  leftsTurn = (Bool *)leftsTurn_;

  //__sync_synchronize ();
  //*mine = 1;
  ////__sync_synchronize ();
  //if (__sync_lock_test_and_set (mine, 1)) {
  if (!__sync_bool_compare_and_swap (mine, 0, 1)) {
    fprintf (stderr, "failed lock!\n");
  }
  while (*other) {
    if (GC_MightCheckForTerminationRequest(s, &cpoll))
      GC_TerminateThread(s);

    //__sync_synchronize ();
    if (amLeft != *leftsTurn) {
      //__sync_synchronize ();
      //*mine = 0;
      ////__sync_synchronize ();
      //__sync_lock_release (mine);
      __sync_bool_compare_and_swap (mine, 1, 0);
      while (amLeft != *leftsTurn) {
        //__sync_synchronize ();
        if (Proc_threadInSection ()) {
          Pointer mine_ = (Pointer)mine,
            other_ = (Pointer)other;
          leftsTurn_ = (Pointer)leftsTurn;
          ENTER3 (s, mine_, other_, leftsTurn_);
          LEAVE3 (s, mine_, other_, leftsTurn_);
          mine = (Bool *)mine_;
          other = (Bool *)other_;
          leftsTurn = (Bool *)leftsTurn_;
        }
      }
      //*mine = 1;
      //if (__sync_lock_test_and_set (mine, 1)) {
      if (!__sync_bool_compare_and_swap (mine, 0, 1)) {
        fprintf (stderr, "failed lock!\n");
      }
    }
    //__sync_synchronize ();
  }
}

void Parallel_dekkerRelease (Bool amLeft, Pointer left, Pointer right, Pointer leftsTurn_)
{
  Bool *mine, *leftsTurn;
  if (amLeft) {
    mine = (Bool *)left;
  }
  else {
    mine = (Bool *)right;
  }
  leftsTurn = (Bool *)leftsTurn_;

  //__sync_synchronize ();
  *leftsTurn = amLeft ? 0 : 1;
  //__sync_synchronize ();
  //*mine = 0;
  ////__sync_synchronize ();
  //__sync_lock_release (mine);
  __sync_bool_compare_and_swap (mine, 1, 0);
}

Word32 Parallel_processorNumber (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return Proc_processorNumber (s);
}

Word32 Parallel_numberOfProcessors (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->numberOfProcs;
}


Word64 Parallel_maxBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (uint64_t)s->cumulativeStatistics->maxBytesLiveSinceReset;
}

void Parallel_resetBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->cumulativeStatistics->maxBytesLiveSinceReset = 0;
}

uint64_t Parallel_getTimeInGC (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  uint64_t gcTime = rusageTime (&s->cumulativeStatistics->ru_gc);
  return gcTime;
}

// fetchAndAdd implementations

Int8 Parallel_fetchAndAdd8 (pointer p, Int8 v) {
  return __sync_fetch_and_add ((Int8 *)p, v);
}

Int16 Parallel_fetchAndAdd16 (pointer p, Int16 v) {
  return __sync_fetch_and_add ((Int16 *)p, v);
}

Int32 Parallel_fetchAndAdd32 (pointer p, Int32 v) {
  return __sync_fetch_and_add ((Int32 *)p, v);
}

Int64 Parallel_fetchAndAdd64 (pointer p, Int64 v) {
  return __sync_fetch_and_add ((Int64 *)p, v);
}

// arrayFetchAndAdd implementations

Int8 Parallel_arrayFetchAndAdd8 (Pointer p, GC_arrayLength i, Int8 v) {
  return __sync_fetch_and_add (((Int8*)p)+i, v);
}

Int16 Parallel_arrayFetchAndAdd16 (Pointer p, GC_arrayLength i, Int16 v) {
  return __sync_fetch_and_add (((Int16*)p)+i, v);
}

Int32 Parallel_arrayFetchAndAdd32 (Pointer p, GC_arrayLength i, Int32 v) {
  return __sync_fetch_and_add (((Int32*)p)+i, v);
}

Int64 Parallel_arrayFetchAndAdd64 (Pointer p, GC_arrayLength i, Int64 v) {
  return __sync_fetch_and_add (((Int64*)p)+i, v);
}

// compareAndSwap implementations

Int8 Parallel_compareAndSwap8 (pointer p, Int8 old, Int8 new) {
  return __sync_val_compare_and_swap ((Int8 *)p, old, new);
}

Int16 Parallel_compareAndSwap16 (pointer p, Int16 old, Int16 new) {
  return __sync_val_compare_and_swap ((Int16 *)p, old, new);
}

Int32 Parallel_compareAndSwap32 (pointer p, Int32 old, Int32 new) {
  return __sync_val_compare_and_swap ((Int32 *)p, old, new);
}

Int64 Parallel_compareAndSwap64 (pointer p, Int64 old, Int64 new) {
  return __sync_val_compare_and_swap ((Int64 *)p, old, new);
}

// arrayCompareAndSwap implementations

Int8 Parallel_arrayCompareAndSwap8 (Pointer p, GC_arrayLength i, Int8 old, Int8 new) {
  return __sync_val_compare_and_swap (((Int8*)p)+i, old, new);
}

Int16 Parallel_arrayCompareAndSwap16 (Pointer p, GC_arrayLength i, Int16 old, Int16 new) {
  return __sync_val_compare_and_swap (((Int16*)p)+i, old, new);
}

Int32 Parallel_arrayCompareAndSwap32 (Pointer p, GC_arrayLength i, Int32 old, Int32 new) {
  return __sync_val_compare_and_swap (((Int32*)p)+i, old, new);
}

Int64 Parallel_arrayCompareAndSwap64 (Pointer p, GC_arrayLength i, Int64 old, Int64 new) {
  return __sync_val_compare_and_swap (((Int64*)p)+i, old, new);
}

// YIFAN: sched_yield() is linux system function.
void Parallel_myYield (void) {
  sched_yield();
}

// YIFAN: pthread_yield() is nonstandard, but present on several other systems besides linux
// On linux, this function is implemented with sched_yield()
// I believe this is the same as sched_yield(), or maybe a little slower because of the
// calling of sched_yield() in linux.
void Parallel_myYield2 (void) {
  waitCnt++;
  pthread_yield();
}

void Parallel_myUsleep (Int64 usec) {
  usleep(usec);
}


// YIFAN: these functions utilized the pthread priority control functions
// But unfortunately thread priority is meaningful only when we use non-
// preempt schedule strategy, so they are useless most of the time.
static void
display_sched_attr(int policy, struct sched_param *param) {
  printf("    policy=%s, priority=%d\n",
         (policy == SCHED_FIFO) ? "SCHED_FIFO" : 
         (policy == SCHED_RR) ? "SCHED_RR" : 
         (policy == SCHED_OTHER) ? "SCHED_OTHER" :
          "???",
         param->sched_priority);
}

void Parallel_myPrioDown (void) {
  GC_state s = pthread_getspecific(gcstate_key);
  int policy;
  struct sched_param param;
  pthread_getschedparam(s->self, &policy, &param);
  
  // display_sched_attr(policy, &param);
  
  param.sched_priority -= 1;
  pthread_setschedprio(s->self, param.sched_priority);
}

void Parallel_myPrioUp (void) {
  GC_state s = pthread_getspecific(gcstate_key);
  int policy;
  struct sched_param param;
  pthread_getschedparam(s->self, &policy, &param);

  // display_sched_attr(policy, &param);

  param.sched_priority += 1;
  pthread_setschedprio(s->self, param.sched_priority);
}

void Parallel_myMutexLock(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  pthread_mutex_lock(&(thd_state->mailMutex));
}

void Parallel_myMutexUnlock(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  pthread_mutex_unlock(&(thd_state->mailMutex));
}

void Parallel_myCondWait(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  waitCnt++; // stats the waiting for response times
  thd_state->mailSuspending = true;
  pthread_cond_wait(&(thd_state->mailCond), &(thd_state->mailMutex));
  thd_state->mailSuspending = false;
}

void Parallel_myCondSignal(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  pthread_cond_signal(&(thd_state->mailCond));
}

void Parallel_myLLLock(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);
  pthread_mutex_lock(&(thd_state->llMutex));
  thd_state->llFlag = -1;
}
void Parallel_myLLUnlock(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);
  thd_state->llFlag = -2;
  pthread_mutex_unlock(&(thd_state->llMutex));
}

Int64 Parallel_myLLWait(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &(s->procStates[thd]);

  pthread_cond_wait(&(thd_state->llCond), &(thd_state->llMutex));
  return thd_state->llFlag;
}

Int64 Parallel_myLLSleep(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &(s->procStates[thd]);
  pthread_mutex_lock(&(thd_state->llMutex));
  thd_state->llFlag = -1;
  while (thd_state->llFlag == -1) {
    pthread_cond_wait(&(thd_state->llCond), &(thd_state->llMutex));
  }
  pthread_mutex_unlock(&(thd_state->llMutex));
  return thd_state->llFlag;
}

void Parallel_myLLWake(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);
  pthread_mutex_lock(&(thd_state->llMutex));
  thd_state->llFlag = -2;
  pthread_mutex_unlock(&(thd_state->llMutex));
  pthread_cond_signal(&(thd_state->llCond));
}

void Parallel_mySemPost (Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  sem_post(&(thd_state->mailSem));
}

void Parallel_mySemWait(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  waitCnt++;
  thd_state->mailSuspending = true;
  sem_wait(&(thd_state->mailSem));
  thd_state->mailSuspending = false;
}

void Parallel_myLLSignal(Int64 thd) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_state thd_state = &((s->procStates)[thd]);

  int numProcs = s->numberOfProcs;
  int h = ceil(sqrt(numProcs));

  for (int i = 0; i < 4; i++) {
    int chd;
    switch (i) {
    case 0:
      chd = thd - 1;
      break;
    case 1:
      chd = thd + 1;
      break;
    case 2:
      chd = thd - h;
      break;
    case 3:
    default:
      chd = thd + h;
      break;
    }
    chd = (chd >= numProcs) ? chd - numProcs : (chd < 0 ? chd + numProcs : chd);
    GC_state chd_state = &(s->procStates[chd]);
    pthread_mutex_lock(&(chd_state->llMutex));
    if (chd_state->llFlag == -1) {
      chd_state->llFlag = thd;
      pthread_mutex_unlock(&(chd_state->llMutex));
      pthread_cond_signal(&(chd_state->llCond));
      break;
    } else {
      pthread_mutex_unlock(&(chd_state->llMutex));
    }
  }
}

void Parallel_myGetWaitCnt() {
  printf("total wait cnt = %d\n", waitCnt);
}

void Parallel_myProbe() {
  probe++;
}

void Parallel_myGetProbe() {
  printf("total steal cnt = %d\n", probe);
}