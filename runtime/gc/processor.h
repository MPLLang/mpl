#if (defined (MLTON_GC_INTERNAL_FUNCS))

/* Unique number for this thread */
int32_t Proc_processorNumber (GC_state s);

/* Used to make sure all threads are properly initialized */
void Proc_waitForInitialization (GC_state s);
void Proc_signalInitialization (GC_state s);
bool Proc_isInitialized (GC_state s);

/* Synchronize all processors */
void Proc_beginCriticalSection (GC_state s);
void Proc_endCriticalSection (GC_state s);
bool Proc_threadInSection (void);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
