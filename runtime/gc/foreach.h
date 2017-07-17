/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

struct GlobalHeapHole {
  pointer start;
  pointer end;
};

typedef bool (*ObjptrPredicateFunction) (GC_state s, pointer p, void* args);
typedef void (*ForeachObjptrFunction) (GC_state s, objptr *opp, void* args);

static inline void callIfIsObjptr (GC_state s,
                                   ForeachObjptrFunction f,
                                   objptr *opp,
                                   void* fArgs);
/* foreachGlobalObjptr (s, f)
 *
 * Apply f to each global object pointer into the heap.
 */
static inline void foreachGlobalObjptr (GC_state s,
                                        ForeachObjptrFunction f,
                                        void* fArgs);

/* foreachObjptrInObject (s, p, skipWeaks, f)
 *
 * Applies f to each object pointer in the object pointed to by p.
 * Returns pointer to the end of object, i.e. just past object.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */
static inline pointer foreachObjptrInObject (GC_state s,
                                             pointer p,
                                             bool skipWeaks,
                                             ObjptrPredicateFunction predicate,
                                             void* pArgs,
                                             ForeachObjptrFunction f,
                                             void* fArgs);

/* foreachObjptrInRange (s, front, back, f, skipWeaks)
 *
 * Apply f to each pointer between front and *back, which should be a
 * contiguous sequence of objects, where front points at the beginning
 * of the first object and *back points just past the end of the last
 * object.  f may increase *back (for example, this is done by
 * forward).  foreachObjptrInRange returns a pointer to the end of
 * the last object it visits.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */
static inline pointer foreachObjptrInRange (GC_state s,
                                            pointer front,
                                            pointer *back,
                                            bool skipWeaks,
                                            struct GlobalHeapHole* holes,
                                            ObjptrPredicateFunction predicate,
                                            void* pArgs,
                                            ForeachObjptrFunction f,
                                            void* fArgs);

typedef void (*GC_foreachStackFrameFun) (GC_state s, GC_frameIndex i);

/* foreachStackFrame (s, f);
 *
 * Apply f to the frame index of each frame in the current stack.
 */
static inline void foreachStackFrame (GC_state s, GC_foreachStackFrameFun f);

/**
 * ObjptrPredicateFunction that always returns true
 *
 * @return TRUE
 */
bool trueObjptrPredicate(GC_state s, pointer p, void* args);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
