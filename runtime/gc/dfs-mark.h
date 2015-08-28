/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef enum {
  MARK_MODE,
  UNMARK_MODE,
} GC_markMode;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

typedef bool (*dfsDescendHookFun)(GC_state s, void* rawArgs, objptr object);
typedef void (*dfsAscendHookFun)(GC_state s,
                                 void* rawArgs,
                                 objptr* objectObjptr);

static inline bool isPointerMarked (pointer p);
static inline bool isPointerMarkedByMode (pointer p, GC_markMode m);
static size_t dfsMarkByModeCustom (GC_state s, pointer root,
                                   GC_markMode mode,
                                   bool shouldHashCons,
                                   bool shouldLinkWeaks,
                                   dfsDescendHookFun descendHook,
                                   void* descendHookArgs,
                                   dfsAscendHookFun ascendHook,
                                   void* ascendHookArgs);
static size_t dfsMarkByMode (GC_state s, pointer root,
                             GC_markMode mode,
                             bool shouldHashCons,
                             bool shouldLinkWeaks);
static inline void dfsMarkWithHashConsWithLinkWeaks (GC_state s,
                                                     objptr *opp,
                                                     void* ignored);
static inline void dfsMarkWithoutHashConsWithLinkWeaks (GC_state s,
                                                        objptr *opp,
                                                        void* ignored);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
