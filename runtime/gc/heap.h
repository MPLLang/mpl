/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define GC_HEAP_LIMIT_SLOP 512

/* The most that the mutator may allocate after one limit check of a
 * statically known size, GC_HEAP_LIMIT_SLOP included. The compiler splits
 * the allocations of a basic block so that no such check asks for more than
 * maxBytesAlloc = 512*7 - limitSlop bytes (LimitCheck.transform in
 * mlton/backend/insert-checks.fun), and GC_collect adds the slop to every
 * request: 512 * 7 bytes in all, chosen there so that such a request, with
 * the chunk descriptor, fits in the smallest block (4K).
 *
 * This must match insert-checks.fun: if the 512*7 there changes, change it
 * here too. The runtime relies on it where a check also asks for a size
 * known only at run time (the bound on an IntInf result): the request is
 * then at most that size plus GC_LIMIT_CHECK_MAX_BYTES, and
 * GC_LIMIT_CHECK_MAX_BYTES is the room the code after such a call may use
 * before its next limit check (see leaveOversizedChunk in int-inf.c).
 */
#define GC_LIMIT_CHECK_MAX_BYTES (7 * GC_HEAP_LIMIT_SLOP)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */


#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerInRootHeap(GC_state s, pointer p);
static inline bool isObjptrInRootHeap(GC_state s, objptr p);

#if ASSERT
static inline bool threadAndHeapOkay(GC_state s);
#endif

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
