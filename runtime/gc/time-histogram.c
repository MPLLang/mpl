/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))


TimeHistogram TimeHistogram_new(
  size_t numBuckets,
  struct timespec * bucketWidth)
{
  numBuckets = max(1, numBuckets);
  TimeHistogram h = (TimeHistogram)malloc(sizeof(struct TimeHistogram));
  h->buckets = malloc(numBuckets * sizeof(size_t));
  h->numBuckets = numBuckets;
  h->bucketWidth = *bucketWidth;

  for (size_t i = 0; i < numBuckets; i++) {
    h->buckets[i] = 0;
  }

  return h;
}


void TimeHistogram_free(TimeHistogram h) {
  free(h->buckets);
  free(h);
}


size_t TimeHistogram_numBuckets(TimeHistogram h) {
  return h->numBuckets;
}


void TimeHistogram_insert(TimeHistogram h, struct timespec *elem) {
  struct timespec remainder = *elem;

  for (size_t i = 0; i < h->numBuckets; i++) {
    if (i == h->numBuckets-1 || timespec_geq(&(h->bucketWidth), &remainder)) {
      h->buckets[i]++;
      break;
    }
    timespec_sub(&remainder, &(h->bucketWidth));
  }
}


size_t TimeHistogram_reportDistribution(TimeHistogram h, double *output) {
  size_t total = 0;
  for (size_t i = 0; i < h->numBuckets; i++) {
    total += h->buckets[i];
  }
  
  if (total > 0) {
    for (size_t i = 0; i < h->numBuckets; i++) {
      output[i] = (double)(h->buckets[i]) / (double)total;
    }
  }
  else {
    for (size_t i = 0; i < h->numBuckets; i++) {
      output[i] = (double)0.0;
    }
  }

  return total;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */
