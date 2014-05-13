
#if (defined (MLTON_GC_INTERNAL_BASIS))

void Parallel_init (void);

void Parallel_yield (void);
Pointer Parallel_lockInit (Pointer);
void Parallel_lockTake (Pointer);
void Parallel_lockRelease (Pointer);
void Parallel_dekkerTake (Bool, Pointer, Pointer, Pointer);
void Parallel_dekkerRelease (Bool, Pointer, Pointer, Pointer);

void Parallel_lock (Int32);
void Parallel_unlock (Int32);

Int32 Parallel_processorNumber (void);
Int32 Parallel_numberOfProcessors (void);
Word64 Parallel_maxBytesLive (void);
void Parallel_resetBytesLive (void);
Word64 Parallel_getTimeInGC (void);

Int32 Parallel_fetchAndAdd (pointer p, Int32 v);
bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
