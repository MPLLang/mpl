#ifndef GDTOA_MULTIPLE_THREADS_DEFS_
#define GDTOA_MULTIPLE_THREADS_DEFS_

extern void set_max_gdtoa_threads(unsigned int n);
void ACQUIRE_DTOA_LOCK(int n);
void FREE_DTOA_LOCK(int n);
unsigned int dtoa_get_threadno(void);

#endif
