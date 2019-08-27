/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void rusageZero (struct rusage *ru) {
  memset (ru, 0, sizeof (*ru));
}

void rusagePlusMax (struct rusage *ru1,
                    struct rusage *ru2,
                    struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec;
  suseconds_t     usec;

  sec = ru1->ru_utime.tv_sec + ru2->ru_utime.tv_sec;
  usec = ru1->ru_utime.tv_usec + ru2->ru_utime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_utime.tv_sec = sec;
  ru->ru_utime.tv_usec = usec;

  sec = ru1->ru_stime.tv_sec + ru2->ru_stime.tv_sec;
  usec = ru1->ru_stime.tv_usec + ru2->ru_stime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_stime.tv_sec = sec;
  ru->ru_stime.tv_usec = usec;
}

void rusageMinusMax (struct rusage *ru1,
                     struct rusage *ru2,
                     struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec;
  suseconds_t     usec;

  sec = (ru1->ru_utime.tv_sec - ru2->ru_utime.tv_sec) - 1;
  usec = ru1->ru_utime.tv_usec + million - ru2->ru_utime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_utime.tv_sec = sec;
  ru->ru_utime.tv_usec = usec;

  sec = (ru1->ru_stime.tv_sec - ru2->ru_stime.tv_sec) - 1;
  usec = ru1->ru_stime.tv_usec + million - ru2->ru_stime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_stime.tv_sec = sec;
  ru->ru_stime.tv_usec = usec;
}

static inline void rusageMultiply (struct rusage *ru1,
                                   size_t factor,
                                   struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec;
  suseconds_t     usec;

  sec = ru1->ru_utime.tv_sec * factor;
  usec = ru1->ru_utime.tv_usec * factor;
  sec += (usec / million);
  usec %= million;
  ru->ru_utime.tv_sec = sec;
  ru->ru_utime.tv_usec = usec;

  sec = ru1->ru_stime.tv_sec * factor;
  usec = ru1->ru_stime.tv_usec * factor;
  sec += (usec / million);
  usec %= million;
  ru->ru_stime.tv_sec = sec;
  ru->ru_stime.tv_usec = usec;
}

uintmax_t rusageTime (struct rusage *ru) {
  uintmax_t result;

  result = 0;
  result += 1000 * (uintmax_t)ru->ru_utime.tv_sec;
  result += 1000 * (uintmax_t)ru->ru_stime.tv_sec;
  result += (uintmax_t)ru->ru_utime.tv_usec / 1000;
  result += (uintmax_t)ru->ru_stime.tv_usec / 1000;
  return result;
}

void startTiming (int who, struct rusage *ru_start) {
  getrusage (who, ru_start);
}

/* Accumulate and return time as number of milliseconds. */
uintmax_t stopTiming (int who, struct rusage *ru_start, struct rusage *ru_acc) {
  struct rusage ru_finish, ru_total;

  getrusage (who, &ru_finish);
  rusageMinusMax (&ru_finish, ru_start, &ru_total);
  rusagePlusMax (ru_acc, &ru_total, ru_acc);
  return rusageTime (&ru_total);
}

void timespec_now(struct timespec *x) {
#if defined(CLOCK_MONOTONIC_RAW)
  clock_gettime(CLOCK_MONOTONIC_RAW, x);
#else
  clock_gettime(CLOCK_MONOTONIC, x);
#endif
}

void timespec_sub(struct timespec *x, struct timespec *y) {
  assert(x->tv_sec >= y->tv_sec);

  if (x->tv_nsec < y->tv_nsec) {
    assert(x->tv_sec >= y->tv_sec + 1);
    x->tv_sec -= 1;
    x->tv_nsec += 1000000000L;
  }

  assert(x->tv_sec >= y->tv_sec);
  assert(x->tv_nsec >= y->tv_nsec);

  x->tv_sec -= y->tv_sec;
  x->tv_nsec -= y->tv_nsec;
}

void timespec_add(struct timespec *x, struct timespec *y) {
  x->tv_sec += y->tv_sec;
  x->tv_nsec += y->tv_nsec;

  if (x->tv_nsec >= 1000000000L) {
    x->tv_sec += 1;
    x->tv_nsec -= 1000000000L;
  }
}
