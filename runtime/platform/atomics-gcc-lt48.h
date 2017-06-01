/* Copyright (C) 2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file atomics-gcc-gte48.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Definition of the atomics for GCC >= 4.8
 */

#ifndef ATOMICS_H_
#define ATOMICS_H_

#include <stdint.h>

/* RAM_NOTE: These atomics are only for x86/x86_64, and possibly overkill! */

#define ATOMIC_STORE(suffix, type)                                      \
  static inline void atomicStore##suffix(volatile type* destination, type value) { \
    __sync_synchronize();                                               \
    *destination = value;                                               \
    __sync_synchronize();                                               \
  }

#define ATOMIC_LOAD(suffix, type)                                       \
  static inline type atomicLoad##suffix(volatile type* destination) {   \
    __sync_synchronize();                                               \
    type value = *destination;                                          \
    __sync_synchronize();                                               \
    return value;                                                       \
  }

#define DEFINE_ATOMICS(suffix, type)                     \
  ATOMIC_STORE(suffix, type)                             \
  ATOMIC_LOAD(suffix, type)

DEFINE_ATOMICS(U8, uint8_t)
DEFINE_ATOMICS(U16, uint16_t)
DEFINE_ATOMICS(U32, uint32_t)
DEFINE_ATOMICS(U64, uint64_t)

DEFINE_ATOMICS(S8, int8_t)
DEFINE_ATOMICS(S16, int16_t)
DEFINE_ATOMICS(S32, int32_t)
DEFINE_ATOMICS(S64, int64_t)

#undef DEFINE_ATOMICS

#endif /* ATOMICS_H_ */
