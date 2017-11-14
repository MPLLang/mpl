#include "platform.h"

C_Int_t Time_getTimeOfDay (Ref(C_Time_t) sec, Ref(C_SUSeconds_t) usec) {
  struct timeval timeval;
  int res;
  res = gettimeofday (&timeval, (struct timezone*)NULL);
  if (! res) {
    *((C_Time_t*)sec) = timeval.tv_sec;
    *((C_SUSeconds_t*)usec) = timeval.tv_usec;
  }
  return res;
}

Word64 Time_rdtsc() {
  unsigned int lo, hi;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));                        
  return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );  
}
