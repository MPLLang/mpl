 
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
PRIVATE Real64 Parallel_compareAndSwapR64 (pointer p, double old, double new);

PRIVATE Int8 Parallel_arrayCompareAndSwap8 (Pointer, GC_arrayLength, Int8, Int8);
PRIVATE Int16 Parallel_arrayCompareAndSwap16 (Pointer, GC_arrayLength, Int16, Int16);
PRIVATE Int32 Parallel_arrayCompareAndSwap32 (Pointer, GC_arrayLength, Int32, Int32);
PRIVATE Int64 Parallel_arrayCompareAndSwap64 (Pointer, GC_arrayLength, Int64, Int64);
PRIVATE Real64 Parallel_arrayCompareAndSwapR64 (Pointer, GC_arrayLength, double, Real64);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
