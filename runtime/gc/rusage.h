/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void rusageZero (struct rusage *ru);
static inline void rusagePlusMax (struct rusage *ru1,
                                  struct rusage *ru2,
                                  struct rusage *ru);
static inline void rusageMinusMax (struct rusage *ru1,
                                   struct rusage *ru2,
                                   struct rusage *ru);
static inline void rusageMultiply (struct rusage *ru1,
                                   size_t factor,
                                   struct rusage *ru);
static inline uintmax_t rusageTime (struct rusage *ru);
static inline void startTiming (int who, struct rusage *ru_start);
static uintmax_t stopTiming (int who, struct rusage *ru_start, struct rusage *ru_gc);

void timespec_now(struct timespec *x);
/* compute dst = dst - x. requires that dst >= x. */
void timespec_sub(struct timespec *dst, struct timespec *x);
/* compute dst = dst + x */
void timespec_add(struct timespec *dst, struct timespec *x);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
