#include "gc_state.h"

#ifndef PROCESSOR_H_
#define PROCESSOR_H_

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/*********/
/* Types */
/*********/
/**
 * Enumerates the states of the BSP round run in Proc_BSP().
 */
enum BSPState {
  DONE, /**< The BSP is "done" and ready to start a new round */
  WAITING, /**< The BSP round has started and is waiting for participants to
              join */
  IN_PROGRESS /**< The BSP round is in-progress */
};

/**
 * The bspFunction is a function used in each stage of a BSP round in
 * Proc_BSP().
 *
 * @param arg The argument to the function, as a generic void*
 *
 * @return TRUE if the caller should continue participating in the BSP round,
 * FALSE otherwise.
 */
typedef bool (*bspFunction)(void* arg);

/*************/
/* Interface */
/*************/
/* Unique number for this thread */
int32_t Proc_processorNumber (GC_state s);

/* Used to make sure all threads are properly initialized */
void Proc_waitForInitialization (GC_state s);
void Proc_signalInitialization (GC_state s);
bool Proc_isInitialized (GC_state s);

/* Synchronize all processors */
void Proc_beginCriticalSection (GC_state s);
void Proc_endCriticalSection (GC_state s);

/**
 * This function starts or joins a BSP round
 *
 * @attention
 * The initiator of the round must set 'functions' to non-NULL, 'numFunctions'
 * to non-zero, and 'args' to non-NULL. Participants must set 'functions' to
 * NULL, 'numFunctions' to zero, and 'args' to NULL.
 *
 * @param s The GC_state to use in this round
 * @param functions The array of functions to go through in the BSP
 * @param numFunctions The number of functions in 'functions'
 * @param args The array of arguments for each function in 'functions'
 *
 * @return FALSE if (a) the caller attempts to join a BSP round in the
 * 'IN_PROGRESS' state, (b) an initiator joins a BSP round in the 'WAITING'
 * state, or (c) a participant joins a BSP round in the 'DONE' state; TRUE
 * otherwise.
 */
bool Proc_BSP(GC_state s,
              bspFunction* functions,
              size_t numFunctions,
              void** args);

/**
 * @return Current BSP state.
 */
enum BSPState Proc_BSPState(void);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* PROCESSOR_H_ */
