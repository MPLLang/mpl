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
typedef void (*HHObjptrFunction) (GC_state s,
                                  void** destinationLevelList,
                                  struct HM_HierarchicalHeap* hh,
                                  size_t minLevel,
                                  size_t maxLevel,
                                  objptr* opp);
#endif /* MLTON_GC_INTERNAL_TYPES */

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
static void* HM_HHC_foreachHHObjptrInObject(GC_state s,
                                            pointer p,
                                            HHObjptrFunction f,
                                            void** destinationLevelList,
                                            struct HM_HierarchicalHeap* hh,
                                            size_t minLevel,
                                            size_t maxLevel);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */
