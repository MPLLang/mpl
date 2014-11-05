
#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void Parallel_init (void);

PRIVATE void Parallel_yield (void);
PRIVATE Pointer Parallel_lockInit (Pointer);
PRIVATE void Parallel_lockTake (Pointer);
PRIVATE void Parallel_lockRelease (Pointer);
PRIVATE void Parallel_dekkerTake (Bool, Pointer, Pointer, Pointer);
PRIVATE void Parallel_dekkerRelease (Bool, Pointer, Pointer, Pointer);

#warning Remove when sure I am done
#if 0
void Parallel_lock (Int32);
void Parallel_unlock (Int32);
#endif

PRIVATE Int32 Parallel_processorNumber (void);
PRIVATE Int32 Parallel_numberOfProcessors (void);
PRIVATE Word64 Parallel_maxBytesLive (void);
PRIVATE void Parallel_resetBytesLive (void);
PRIVATE Word64 Parallel_getTimeInGC (void);

PRIVATE Int32 Parallel_fetchAndAdd (pointer p, Int32 v);
PRIVATE bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
