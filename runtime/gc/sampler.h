/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef SAMPLER_H_
#define SAMPLER_H_

#if (defined (MLTON_GC_INTERNAL_FUNCS))

typedef void (*SamplerFun) (GC_state s, struct timespec *tm, void *env);

typedef struct SamplerClosure {
  SamplerFun fun;
  void *env;
} *SamplerClosure;

typedef struct Sampler {
  struct SamplerClosure func;
  struct timespec desiredInterval;
  struct timespec absoluteStart;
  size_t currentEpoch;
} * Sampler;


void initSampler(GC_state s, Sampler samp, SamplerClosure func, struct timespec *desiredInterval);

void maybeSample(GC_state s, Sampler samp);

#endif /* MLTON_GC_INTERNAL_FUNCS */


#endif /* SAMPLER_H_ */
