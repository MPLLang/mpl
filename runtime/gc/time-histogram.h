/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef TIME_HISTOGRAM_H_
#define TIME_HISTOGRAM_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct TimeHistogram {

  size_t *buckets;
  size_t numBuckets;
  struct timespec bucketWidth;

} * TimeHistogram;

#else

struct TimeHistogram;
typedef struct TimeHistogram * TimeHistogram;

#endif /* MLTON_GC_INTERNAL_TYPES */



#if (defined (MLTON_GC_INTERNAL_FUNCS))

// numBuckets has to be at least 1
// ith bucket (except for last) has the range [i*bucketWidth, (i+1)*bucketWidth)
// last bucket is for all remaining range
TimeHistogram TimeHistogram_new(size_t numBuckets, struct timespec * bucketWidth);

void TimeHistogram_free(TimeHistogram th);

size_t TimeHistogram_numBuckets(TimeHistogram h);

// write bucketed distribution into output (values in range [0,1])
// requires len(output) >= numBuckets(h)
size_t TimeHistogram_reportDistribution(TimeHistogram h, double *output);

void TimeHistogram_insert(TimeHistogram th, struct timespec *elem);

#endif

#endif /* TIME_HISTOGRAM_H_ */
