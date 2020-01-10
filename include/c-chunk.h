/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

#include <stdio.h>
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

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

#define UNUSED __attribute__ ((unused))
#define NORETURN __attribute__ ((noreturn))
#define Unreachable() __builtin_unreachable()

/* ------------------------------------------------- */
/* Chunk                                             */
/* ------------------------------------------------- */

#define Chunk(n)                                \
        DeclareChunk(n) {                       \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: Chunk%d(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, n, (int)nextBlock);

#define ChunkSwitch(n)                          \
                goto doSwitchNextBlock;         \
                doSwitchNextBlock:              \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: ChunkSwitch%d(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, n, (int)nextBlock); \
                switch (nextBlock) {

#define EndChunkSwitch                          \
                default:                        \
                        goto doLeaveChunk;      \
                } /* end switch (nextBlock) */

#define EndChunk(n, tail)                       \
                /* interchunk return */         \
                doLeaveChunk:                   \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: EndChunk%d(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, n, (int)nextBlock); \
                if (tail) {                     \
                        return (*(nextChunks[nextBlock]))(gcState, stackTop, frontier, nextBlock); \
                } else {                        \
                        FlushFrontier();        \
                        FlushStackTop();        \
                        return nextBlock;       \
                }                               \
        } /* end chunk */


/* ------------------------------------------------- */
/*  Operands                                         */
/* ------------------------------------------------- */

#define C(ty, x) (*(ty*)(x))
#define G(ty, i) (global##ty [i])
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define X(ty, b, i, s, o) (*(ty*)((b) + ((i) * (s)) + (o)))
#define S(ty, i) (*(ty*)(StackTop + (i)))

#define GCState gcState
#define Frontier frontier
#define StackTop stackTop

#define ExnStack *(size_t*)(GCState + ExnStackOffset)
#define FrontierMem *(Pointer*)(GCState + FrontierOffset)
#define StackBottom *(Pointer*)(GCState + StackBottomOffset)
#define StackTopMem *(Pointer*)(GCState + StackTopOffset)

/* ------------------------------------------------- */
/* Cache and Flush                                   */
/* ------------------------------------------------- */

#define CacheFrontier()                         \
        do {                                    \
                Frontier = FrontierMem;         \
        } while (0)

#define CacheStackTop()                         \
        do {                                    \
                StackTop = StackTopMem;         \
        } while (0)

#define FlushFrontier()                         \
        do {                                    \
                FrontierMem = Frontier;         \
        } while (0)

#define FlushStackTop()                         \
        do {                                    \
                StackTopMem = StackTop;         \
        } while (0)

/* ------------------------------------------------- */
/* Stack                                             */
/* ------------------------------------------------- */

#define Push(bytes)                                                     \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Push (%d)\n",          \
                                        __FILE__, __LINE__, bytes);     \
                StackTop += (bytes);                                    \
        } while (0)

/* ------------------------------------------------- */
/* Transfers                                         */
/* ------------------------------------------------- */

#define BNZ(x, lnz, lz)                                                 \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: BNZ(%llu, %s, %s)\n",  \
                                        __FILE__, __LINE__, ((unsigned long long)x), #lnz, #lz); \
                if (x) goto lnz; else goto lz;                          \
        } while (0)

#define NearCall(l)                             \
        goto l

#define FarCall(n, l, tail)                     \
        do {                                    \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: FarCall(%d, %s)\n", \
                                        __FILE__, __LINE__, (int)n, #l); \
                if (tail) {                     \
                        return ChunkName(n)(gcState, stackTop, frontier, l); \
                } else {                        \
                        FlushFrontier();        \
                        FlushStackTop();        \
                        return l;               \
                }                               \
        } while (0)

#define Return()                                                                \
        do {                                                                    \
                nextBlock = *(uintptr_t*)(StackTop - sizeof(uintptr_t));        \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Return()  nextBlock = %d\n",   \
                                        __FILE__, __LINE__, (int)nextBlock);    \
                goto doSwitchNextBlock;                                         \
        } while (0)

#define Raise()                                                                 \
        do {                                                                    \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Raise()\n",                    \
                                        __FILE__, __LINE__);                    \
                StackTop = StackBottom + ExnStack;                              \
                Return();                                                       \
        } while (0)                                                             \


/* ------------------------------------------------- */
/* Calling SML from C                                */
/* ------------------------------------------------- */

#define Thread_returnToC()                                              \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Thread_returnToC()\n", \
                                        __FILE__, __LINE__);            \
                return (uintptr_t)-1;                                   \
        } while (0)

/* ------------------------------------------------- */
/* Primitives                                        */
/* ------------------------------------------------- */

#ifndef MLTON_CODEGEN_STATIC_INLINE
#define MLTON_CODEGEN_STATIC_INLINE static inline
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
