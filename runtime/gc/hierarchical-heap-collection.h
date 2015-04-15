/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap-collection.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Definition of the HierarchicalHeap collection interface
 */

#ifndef HIERARCHICAL_HEAP_COLLECTION_H_
#define HIERARCHICAL_HEAP_COLLECTION_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))
struct HHObjptrFunctionArgs {
  void** destinationLevelList;
  struct HM_HierarchicalHeap* hh;
  size_t minLevel;
  size_t maxLevel;
};

typedef void (*HHObjptrFunction) (GC_state s,
                                  struct HHObjptrFunctionArgs* args,
                                  objptr* opp);
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_BASIS))
/**
 * This function registers 'queuePointer' as the work stealing queue associated
 * with processor 'processor'
 *
 * @note
 * A second call will overwrite the previously saved registration
 *
 * @attention
 * 'queuePointer' <em>must</em> point to an object on the global heap.
 *
 * @param processor The processor to register for
 * @param queuePointer pointer to an object in the global heap that is the queue
 * to register
 */
PRIVATE void HM_HHC_registerQueue(int processor, pointer queuePointer);

/**
 * This function registers 'queueLockPointer' as the work stealing queue lock
 * associated with processor 'processor'
 *
 * @note
 * A second call will overwrite the previously saved registration
 *
 * @attention
 * 'queueLockPointer' <em>must</em> point to an object on the global heap.
 *
 * @param processor The processor to register for
 * @param queueLockPointer pointer to an int ref in the global heap that is the
 * queue lock to register
 */
PRIVATE void HM_HHC_registerQueueLock(int processor, pointer queueLockPointer);
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/**
 * This function performs a local collection on the current hierarchical heap
 */
void HM_HHC_collectLocal(void);

/**
 * Call 'f' for each hierarchical heap objptr in the object 'p'
 *
 * @param s The GC_state to use
 * @param p The object to iterate over
 * @param f The function to call
 * @param destinationLevelList The destination level list passed to 'f'
 * @param hh The struct HM_HierarchicalHeap passed to 'f'
 * @param minLevel The minLevel passed to 'f'
 * @param maxLevel The maxLevel passed to 'f'
 *
 * @return The pointer after the object
 */
static pointer HM_HHC_foreachHHObjptrInObject(GC_state s,
                                              pointer p,
                                              bool traceObject,
                                              HHObjptrFunction f,
                                              struct HHObjptrFunctionArgs* fArgs);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */
