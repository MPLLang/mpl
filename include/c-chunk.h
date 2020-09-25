/* Copyright (C) 2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

/* `memcpy` is used by coercion `<ty>_castTo<ty>` functions (`basis/coerce.h`)
 * and by misaligned `<ty>_fetch`, `<ty>_store`, and `<ty>_move` functions
 * (`basis/Real/Real-ops.h` and `basis/Word/Word-ops.h`)
 */
#include <string.h>
/* Math functions used by `Real<n>_f` functions (`basis/Real/Real-ops.h`).
 */
#include <math.h>

#include "ml-types.h"
#include "c-types.h"
#include "c-common.h"

#define Expect(x,c) __builtin_expect(x, c)
#define UNUSED __attribute__ ((unused))
#define Unreachable() __builtin_unreachable()

/* ------------------------------------------------- */
/*  Operands                                         */
/* ------------------------------------------------- */

#define G(ty, i) (global##ty [i])
#define H(ty, k, o) ((ty)(&staticHeap##k + (o)))
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define S(ty, i) (*(ty*)(StackTop + (i)))
#define T(ty, i) T ## ty ## _ ## i
#define X(ty, b, i, s, o) (*(ty*)((b) + ((i) * (s)) + (o)))

/* ------------------------------------------------- */
/* Primitives                                        */
/* ------------------------------------------------- */

#ifndef INLINE
#define INLINE __attribute__((always_inline)) inline
#endif
#include "basis/coerce.h"
#include "basis/cpointer.h"
#include "basis/Real/Real-ops.h"
#include "basis/Word/Word-ops.h"

/* ------------------------------------------------- */
/*                 References                        */
/* ------------------------------------------------- */

static inline
Real64 ArrayR64_cas(Real64* a, Word64 i, Real64 x, Real64 y) {
  Word64 result =
    __sync_val_compare_and_swap(((Word64*)a) + i, *((Word64*)&x), *((Word64*)&y));
  return *((Real64*)&result);
}

#define RefW8_cas(r, x, y) __sync_val_compare_and_swap((Word8*)(r), (x), (y))
#define RefW16_cas(r, x, y) __sync_val_compare_and_swap((Word16*)(r), (x), (y))
#define RefW32_cas(r, x, y) __sync_val_compare_and_swap((Word32*)(r), (x), (y))
#define RefW64_cas(r, x, y) __sync_val_compare_and_swap((Word64*)(r), (x), (y))

#define RefR32_cas(r, x, y) __sync_val_compare_and_swap((Real32*)(r), (x), (y))
#define RefR64_cas(r, x, y) __sync_val_compare_and_swap((Real64*)(r), (x), (y))

#define RefP_cas(r, x, y) __sync_val_compare_and_swap((Objptr*)(r), (x), (y))
#define RefQ_cas(r, x, y) __sync_val_compare_and_swap((CPointer*)(r), (x), (y))

#define ArrayW8_cas(a, i, x, y) __sync_val_compare_and_swap(((Word8*)(a)) + (i), (x), (y))
#define ArrayW16_cas(a, i, x, y) __sync_val_compare_and_swap(((Word16*)(a)) + (i), (x), (y))
#define ArrayW32_cas(a, i, x, y) __sync_val_compare_and_swap(((Word32*)(a)) + (i), (x), (y))
#define ArrayW64_cas(a, i, x, y) __sync_val_compare_and_swap(((Word64*)(a)) + (i), (x), (y))

#define ArrayR32_cas(a, i, x, y) __sync_val_compare_and_swap(((Real32*)(a)) + (i), (x), (y))
// #define ArrayR64_cas(a, i, x, y) __sync_val_compare_and_swap(((Real64*)(a)) + (i), (x), (y))

#define ArrayP_cas(a, i, x, y) __sync_val_compare_and_swap(((Objptr*)(a)) + (i), (x), (y))
#define ArrayQ_cas(a, i, x, y) __sync_val_compare_and_swap(((CPointer*)(a)) + (i), (x), (y))

extern void Assignable_writeBarrier(CPointer, Objptr, Objptr*, Objptr);

static inline void GC_writeBarrier(CPointer s, Objptr obj, CPointer dst, Objptr src) {
  Assignable_writeBarrier(s, obj, dst, src);
}

#endif /* #ifndef _C_CHUNK_H_ */
