#include <pthread.h>
#include <time.h>
#include "platform.h"
#include <signal.h>

/* num of holding thread or -1 if no one*/
volatile int32_t *Parallel_mutexes;

void Parallel_init (void) {
  GC_state s = pthread_getspecific (gcstate_key);

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

union doubleBin {
  Real64 d;
  Int64 i;
} doubleBin;

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

Real64 Parallel_compareAndSwapR64 (pointer p, Real64 old, Real64 new) {
  union doubleBin o, n, r;
  o.d = old;
  n.d = new;
  r.i = __sync_val_compare_and_swap (((Int64*)p), o.i, n.i);
  return r.d;
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

Real64 Parallel_arrayCompareAndSwapR64 (Pointer p, GC_arrayLength i, Real64 old, Real64 new) {
  union doubleBin o, n, r;
  o.d = old;
  n.d = new;
  r.i = __sync_val_compare_and_swap (((Int64*)p)+i, o.i, n.i);
  return r.d;
}

void Parallel_block_sig (int sig) {
  sigset_t s;
  sigemptyset(&s);
  sigaddset(&s, sig);
  pthread_sigmask (SIG_BLOCK, &s, NULL);
}
void Parallel_unblock_sig (int sig) {
  sigset_t s;
  sigemptyset(&s);
  sigaddset(&s, sig);
  pthread_sigmask (SIG_UNBLOCK, &s, NULL);
}

Int32 Parallel_check_blocked (int sig) {
  sigset_t s;
  sigemptyset(&s);
  pthread_sigmask (SIG_SETMASK, NULL, &s);
  return sigismember(&s, sig);
}

int refToInt (int *p) {
  return ((int) p);
}
