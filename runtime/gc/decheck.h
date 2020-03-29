/**
 * @file decheck.h
 * @brief Disentanglement checking
 * @author Lawrence Wang (lawrenc2)
 * @bug No known bugs
 */

#ifndef _DECHECK_H_
#define _DECHECK_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct {
    uint32_t path;
    uint32_t depth;
} decheck_tid_t;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_HH_decheckInit(GC_state s);
void GC_HH_decheckFork(GC_state s, decheck_tid_t *left, decheck_tid_t *right);
void GC_HH_decheckSetTid(GC_state s, decheck_tid_t tid);
void GC_HH_decheckJoin(GC_state s, decheck_tid_t t1, decheck_tid_t t2);

#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void decheckRead(GC_state s, objptr *ptr);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* _DECHECK_H_ */
