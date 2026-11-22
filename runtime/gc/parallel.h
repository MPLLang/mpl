
#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void Parallel_init (GC_state s);

PRIVATE void Parallel_lockInit (Pointer);
PRIVATE void Parallel_lockTake (GC_state, Pointer);
PRIVATE void Parallel_lockRelease (Pointer);
PRIVATE bool Parallel_alreadyLockedByMe (GC_state, Pointer);
PRIVATE void Parallel_dekkerTake (GC_state, Bool, Pointer, Pointer, Pointer);
PRIVATE void Parallel_dekkerRelease (Bool, Pointer, Pointer, Pointer);

PRIVATE Word32 Parallel_processorNumber (GC_state s);
PRIVATE Word32 Parallel_numberOfProcessors (GC_state s);
PRIVATE Word64 Parallel_maxBytesLive (void);
PRIVATE void Parallel_resetBytesLive (void);
PRIVATE Word64 Parallel_getTimeInGC (GC_state s);

PRIVATE Int8 Parallel_fetchAndAdd8 (pointer p, Int8 v);
PRIVATE Int16 Parallel_fetchAndAdd16 (pointer p, Int16 v);
PRIVATE Int32 Parallel_fetchAndAdd32 (pointer p, Int32 v);
PRIVATE Int64 Parallel_fetchAndAdd64 (pointer p, Int64 v);

PRIVATE Int8 Parallel_arrayFetchAndAdd8 (pointer p, GC_sequenceLength i, Int8 v);
PRIVATE Int16 Parallel_arrayFetchAndAdd16 (pointer p, GC_sequenceLength i, Int16 v);
PRIVATE Int32 Parallel_arrayFetchAndAdd32 (pointer p, GC_sequenceLength i, Int32 v);
PRIVATE Int64 Parallel_arrayFetchAndAdd64 (pointer p, GC_sequenceLength i, Int64 v);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
