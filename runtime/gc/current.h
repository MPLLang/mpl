/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline objptr getThreadCurrentObjptr (GC_state s);
static inline GC_thread getThreadCurrent (GC_state s);
static inline objptr getStackCurrentObjptr (GC_state s);
static inline GC_stack getStackCurrent (GC_state s);

/**
 * Returns the objptr of the current struct HM_HierarchicalHeap in use as per
 * the passed-in GC_state
 *
 * @param s The GC_state to refer to
 *
 * @return the objptr of the current struct HM_HierarchicalHeap
 */
static inline objptr getHierarchicalHeapCurrentObjptr (GC_state s);

/**
 * Returns the pointer to the current struct HM_HierarchicalHeap in use as per
 * the passed-in GC_state
 *
 * @param s the GC_state to refer to
 *
 * @return the pointer to the accessible current struct HM_HierarchicalHeap or
 * NULL if not set
 */
static inline
struct HM_HierarchicalHeap* getHierarchicalHeapCurrent (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
