/**
 * @file decheck.h
 * @brief Disentanglement checking
 * @author Lawrence Wang (lawrenc2)
 * @bug No known bugs
 */

#ifndef _DECHECK_H_
#define _DECHECK_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef union {
    struct {
        uint32_t path;
        uint32_t depth;
    } internal;
    uint64_t bits;
} decheck_tid_t;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_HH_decheckInit(GC_state s);
void GC_HH_decheckFork(GC_state s, uint64_t *left, uint64_t *right);
void GC_HH_decheckSetTid(GC_state s, uint64_t tid);
void GC_HH_decheckJoin(GC_state s, uint64_t t1, uint64_t t2);

#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void decheckRead(GC_state s, objptr *ptr);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* _DECHECK_H_ */
