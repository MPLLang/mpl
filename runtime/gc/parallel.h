
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

PRIVATE Int32 Parallel_fetchAndAdd (pointer p, Int32 v);
PRIVATE bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
