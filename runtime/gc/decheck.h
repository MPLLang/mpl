/* Copyright (C) 2020 Lawrence Wang, Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _DECHECK_H_
#define _DECHECK_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_thread;
typedef struct GC_thread *GC_thread;

typedef union {
    struct {
        uint32_t path;
        uint32_t depth;
    } internal;
    uint64_t bits;
} decheck_tid_t;

struct ManageEntangledArgs
{
  decheck_tid_t reader;
  decheck_tid_t root;
  uint32_t unpinDepth;
  bool firstCall;
};

#define DECHECK_BOGUS_BITS ((uint64_t)0)
#define DECHECK_BOGUS_TID ((decheck_tid_t){ .bits = DECHECK_BOGUS_BITS })

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_HH_decheckFork(GC_state s, uint64_t *left, uint64_t *right);
PRIVATE void GC_HH_decheckSetTid(GC_state s, uint64_t tid);
PRIVATE uint64_t GC_HH_decheckGetTid(GC_state s, objptr thread);
PRIVATE void GC_HH_decheckJoin(GC_state s, uint64_t t1, uint64_t t2);
PRIVATE void GC_HH_copySyncDepthsFromThread(GC_state s, objptr fromThread, objptr toThread, uint32_t stealDepth);

PRIVATE bool GC_HH_decheckMaxDepth(objptr resultRef);

#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void decheckInit(GC_state s);
bool decheck(GC_state s, objptr ptr);
bool decheckIsOrdered(GC_thread thread, decheck_tid_t t1);
int lcaHeapDepth(decheck_tid_t t1, decheck_tid_t t2);
objptr manage_entangled(GC_state s, objptr ptr, decheck_tid_t reader);
void traverseAndCheck(GC_state s, objptr *opp ,objptr op, void *rawArgs);
#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* _DECHECK_H_ */
