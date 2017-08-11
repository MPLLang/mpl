#include "platform.h"

C_Time_t MLton_Rusage_self_utime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_self.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_self_utime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_self.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_self_stime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_self.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_self_stime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_self.ru_stime.tv_usec;
}

C_Time_t MLton_Rusage_children_utime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_children.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_children_utime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_children.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_children_stime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_children.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_children_stime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_children.ru_stime.tv_usec;
}

C_Time_t MLton_Rusage_gc_utime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_gc.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_gc_utime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_gc.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_gc_stime_sec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_gc.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_gc_stime_usec (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  return tlsObjects->MLton_Rusage_gc.ru_stime.tv_usec;
}

void MLton_Rusage_getrusage (void) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  GC_getGCRusageOfProc(-1, &(tlsObjects->MLton_Rusage_gc));
  getrusage (RUSAGE_SELF, &(tlsObjects->MLton_Rusage_self));
  getrusage (RUSAGE_CHILDREN, &(tlsObjects->MLton_Rusage_children));
}

void MLton_Rusage_getGCRusageOfProc (Int32_t p) {
  struct TLSObjects* tlsObjects = GC_getTLSObjects();

  GC_getGCRusageOfProc(p, &(tlsObjects->MLton_Rusage_gc));
}
