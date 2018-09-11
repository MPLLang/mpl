
#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void Parallel_init (void);

PRIVATE void Parallel_yield (void);
PRIVATE void Parallel_lockInit (Pointer);
PRIVATE void Parallel_lockTake (Pointer);
PRIVATE void Parallel_lockRelease (Pointer);
PRIVATE bool Parallel_alreadyLockedByMe (Pointer);
PRIVATE void Parallel_dekkerTake (Bool, Pointer, Pointer, Pointer);
PRIVATE void Parallel_dekkerRelease (Bool, Pointer, Pointer, Pointer);

PRIVATE Word32 Parallel_processorNumber (void);
PRIVATE Word32 Parallel_numberOfProcessors (void);
PRIVATE Word64 Parallel_maxBytesLive (void);
PRIVATE void Parallel_resetBytesLive (void);
PRIVATE Word64 Parallel_getTimeInGC (void);

PRIVATE Int8 Parallel_fetchAndAdd8 (pointer p, Int8 v);
PRIVATE Int16 Parallel_fetchAndAdd16 (pointer p, Int16 v);
PRIVATE Int32 Parallel_fetchAndAdd32 (pointer p, Int32 v);
PRIVATE Int64 Parallel_fetchAndAdd64 (pointer p, Int64 v);

PRIVATE Int8 Parallel_arrayFetchAndAdd8 (pointer p, GC_arrayLength i, Int8 v);
PRIVATE Int16 Parallel_arrayFetchAndAdd16 (pointer p, GC_arrayLength i, Int16 v);
PRIVATE Int32 Parallel_arrayFetchAndAdd32 (pointer p, GC_arrayLength i, Int32 v);
PRIVATE Int64 Parallel_arrayFetchAndAdd64 (pointer p, GC_arrayLength i, Int64 v);

PRIVATE Int8 Parallel_compareAndSwap8 (pointer p, Int8 old, Int8 new);
PRIVATE Int16 Parallel_compareAndSwap16 (pointer p, Int16 old, Int16 new);
PRIVATE Int32 Parallel_compareAndSwap32 (pointer p, Int32 old, Int32 new);
PRIVATE Int64 Parallel_compareAndSwap64 (pointer p, Int64 old, Int64 new);

PRIVATE Int8 Parallel_arrayCompareAndSwap8 (Pointer, GC_arrayLength, Int8, Int8);
PRIVATE Int16 Parallel_arrayCompareAndSwap16 (Pointer, GC_arrayLength, Int16, Int16);
PRIVATE Int32 Parallel_arrayCompareAndSwap32 (Pointer, GC_arrayLength, Int32, Int32);
PRIVATE Int64 Parallel_arrayCompareAndSwap64 (Pointer, GC_arrayLength, Int64, Int64);

// YIFAN: yield_all related implementations
PRIVATE void Parallel_myYield (void);
PRIVATE void Parallel_myYield2 (void);
PRIVATE void Parallel_myUsleep (Int64 usec);

// YIFAN: priority control related
PRIVATE void Parallel_myPrioDown (void);
PRIVATE void Parallel_myPrioUp   (void);

// YIFAN: mail related functions. Reserved for 'yield_to'
PRIVATE void Parallel_myMutexLock  (Int64 thd);
PRIVATE void Parallel_myMutexUnlock(Int64 thd);
PRIVATE void Parallel_myCondWait   (Int64 thd);
PRIVATE void Parallel_myCondSignal (Int64 thd);
PRIVATE void Parallel_myMailLoop   (Int64* flag, Int64 waitCnt);

// YIFAN: Lifeline dynamic threads sleep control
PRIVATE void  Parallel_myLLLock    (Int64 thd);
PRIVATE void  Parallel_myLLUnlock  (Int64 thd);
PRIVATE Int64 Parallel_myLLWait    (Int64 thd);
PRIVATE Int64 Parallel_myLLSleep   (Int64 thd);
PRIVATE Int64 Parallel_myLLTimedSleep (Int64 thd, Int64 msec);
PRIVATE void  Parallel_myLLWake    (Int64 thd);
PRIVATE void  Parallel_myLLSignal  (Int64 thd);

PRIVATE Int64 Parallel_myLLFindChild (Int64 thd);
PRIVATE void  Parallel_myLLSignalSpec(Int64 thd);

// YIFAN: probe related
PRIVATE void Parallel_myGetWaitCnt (void);
PRIVATE void Parallel_myProbe (void);
PRIVATE void Parallel_myGetProbe(void);
#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
