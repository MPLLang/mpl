/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void enter (GC_state s);
static inline void leave (GC_state s);

/* RAM_NOTE: Condense out of macros? Also, some versions not used */

#define ENTER0(s) do { enter (s); } while(0)
#define ENTER1(s, p) do { objptr roots[1]; \
                          roots[0] = pointerToObjptr (p, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 1; \
                          enter (s); \
                          p = objptrToPointer (roots[0], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)
#define ENTER2(s, p1, p2) do { objptr roots[2];                           \
                          roots[0] = pointerToObjptr (p1, s->heap->start); \
                          roots[1] = pointerToObjptr (p2, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 2; \
                          enter (s); \
                          p1 = objptrToPointer (roots[0], s->heap->start); \
                          p2 = objptrToPointer (roots[1], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)
#define ENTER3(s, p1, p2, p3) do { objptr roots[3];                        \
                          roots[0] = pointerToObjptr (p1, s->heap->start); \
                          roots[1] = pointerToObjptr (p2, s->heap->start); \
                          roots[2] = pointerToObjptr (p3, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 3; \
                          enter (s); \
                          p1 = objptrToPointer (roots[0], s->heap->start); \
                          p2 = objptrToPointer (roots[1], s->heap->start); \
                          p3 = objptrToPointer (roots[2], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)

#define LEAVE0(s) do { leave (s); } while(0)
#define LEAVE1(s, p) do { objptr roots[1]; \
                          roots[0] = pointerToObjptr (p, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 1; \
                          leave (s); \
                          p = objptrToPointer (roots[0], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)
#define LEAVE2(s, p1, p2) do { objptr roots[2]; \
                          roots[0] = pointerToObjptr (p1, s->heap->start); \
                          roots[1] = pointerToObjptr (p2, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 2; \
                          leave (s); \
                          p1 = objptrToPointer (roots[0], s->heap->start); \
                          p2 = objptrToPointer (roots[1], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)
#define LEAVE3(s, p1, p2, p3) do { objptr roots[3];                        \
                          roots[0] = pointerToObjptr (p1, s->heap->start); \
                          roots[1] = pointerToObjptr (p2, s->heap->start); \
                          roots[2] = pointerToObjptr (p3, s->heap->start); \
                          s->roots = roots; \
                          s->rootsLength = 3; \
                          leave (s); \
                          p1 = objptrToPointer (roots[0], s->heap->start); \
                          p2 = objptrToPointer (roots[1], s->heap->start); \
                          p3 = objptrToPointer (roots[2], s->heap->start); \
                          s->roots = NULL; \
                          s->rootsLength = 0; \
                        } while(0)

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
