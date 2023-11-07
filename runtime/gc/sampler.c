/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */


void initSampler(
  __attribute__((unused)) GC_state s,
  Sampler samp,
  SamplerClosure func,
  struct timespec *desiredInterval)
{
  samp->func = *func;
  samp->desiredInterval = *desiredInterval;
  samp->currentEpoch = 0;
  timespec_now(&(samp->absoluteStart));
}


static void timespec_mul(struct timespec *dst, size_t multiplier) {
  size_t sec = dst->tv_sec;
  size_t nsec = dst->tv_nsec;

  size_t nps = 1000L * 1000 * 1000;

  size_t new_nsec = (nsec * multiplier) % nps;
  size_t add_sec = (nsec * multiplier) / nps;

  dst->tv_sec = (sec * multiplier) + add_sec;
  dst->tv_nsec = new_nsec;
}


static inline double timespec_to_seconds(struct timespec *tm) {
  return (double)tm->tv_sec + ((double)tm->tv_nsec * 0.000000001);
}


void maybeSample(GC_state s, Sampler samp) {
  size_t oldEpoch = samp->currentEpoch;

  // compute the time of the last successful sample (relative to start)
  struct timespec lastSample;
  lastSample = samp->desiredInterval;
  timespec_mul(&lastSample, oldEpoch);

  // compare against current time by computing epoch diff
  struct timespec now;
  timespec_now(&now);
  timespec_sub(&now, &(samp->absoluteStart));
  double diff = timespec_to_seconds(&now) - timespec_to_seconds(&lastSample);
  long epochDiff = (long)(diff / timespec_to_seconds(&samp->desiredInterval));

  if (epochDiff < 1)
    return;

  size_t newEpoch = oldEpoch + epochDiff;

  if (__sync_bool_compare_and_swap(&samp->currentEpoch, oldEpoch, newEpoch)) {
    samp->func.fun(s, &now, samp->func.env);
  }
}