/* Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Definition of the HierarchicalHeap object and management interface. This
 * module belongs in the HeapManagement::HierarchicalHeap (HM_HH) namespace
 */

#ifndef HIERARCHICAL_HEAP_H_
#define HIERARCHICAL_HEAP_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))
/* RAM_NOTE: May need to be rearranged for cache efficiency */
/* RAM_NOTE: Needs to be renamed to HM_HH_HierarchicalHeap */
/**
 * @brief
 * Represents a "node" of the hierarchical heap and contains various data
 * about its contents and structure
 *
 * HierarchicalHeap objects are normal objects with the following layout:
 *
 * header ::
 * padding ::
 * lock (Int32) ::
 * level (Word32) ::
 * stealLevel (Word32) ::
 * id (Word32) ::
 * chunkList (void*) ::
 * parentHH (objptr) ::
 * nextChildHH (objptr) ::
 * childHHList (objptr)
 *
 * There may be zero or more bytes of padding for alignment purposes.
 */
struct HM_HierarchicalHeap {
  void* lastAllocatedChunk; /**< The last allocated chunk */

  Int32 lock; /**< The spinlock for exclusive access to the childHHList */

  /* RAM_NOTE: can be lesser width if I add another field */
  Word32 level; /**< The current level of the hierarchy which new chunks should
                 * belong to. */

  Word32 stealLevel; /**< The parent's level that I stole from */

  Word32 id; /**< the ID of this HierarchicalHeap object, for visualization
              * purposes */

  void* levelList; /**< The list of level lists. See HM_ChunkInfo for more
                    * information */

  void* newLevelList; /**< The new list of level lists generated during a local
                       * garbage collection. See HM_ChunkInfo for more
                       information */

  objptr parentHH; /**< The heap this object branched off of or BOGUS_OBJPTR
                    * if it is the first heap. */

  objptr nextChildHH; /**< The next heap in the 'derivedHHList' of
                       * the 'parentHH'. This variable is the 'next'
                       * pointer for the intrusive linked list. */

  objptr childHHList; /**< The list of heaps that are derived from this
                       * heap. All heaps in this list have their 'parentHH' set
                       * to this object. In addition, it is in descending order
                       * of 'stealLevel' */
} __attribute__((packed));

COMPILE_TIME_ASSERT(HM_HierarchicalHeap__packed,
                    sizeof(struct HM_HierarchicalHeap) ==
                    sizeof(void*) +
                    sizeof(Int32) +
                    sizeof(Word32) +
                    sizeof(Word32) +
                    sizeof(Word32) +
                    sizeof(void*) +
                    sizeof(void*) +
                    sizeof(objptr) +
                    sizeof(objptr) +
                    sizeof(objptr));

/**
 * This value is an "invalid" level and used for HM_HierarchicalHeap
 * initialization
 */
#define HM_HH_INVALID_LEVEL CHUNK_INVALID_LEVEL

/**
 * This is the value of HM_HierarchicalHeap::lock when locked
 */
#define HM_HH_LOCK_LOCKED ((Int32)(1))

/**
 * This is the value of HM_HierarchicalHeap::lock when unlocked
 */
#define HM_HH_LOCK_UNLOCKED ((Int32)(-1))

/**
 * This is the initial value of HM_HierarchicalHeap::lock
 */
#define HM_HH_LOCK_INITIALIZER HM_HH_LOCK_UNLOCKED
#else
struct HM_HierarchicalHeap;
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_BASIS))
/**
 * Appends the derived hierarchical heap to the childHHList of the source
 * hierarchical heap and sets relationships appropriately.
 *
 * @attention
 * 'childHH' should be a newly created "orphan" HierarchicalHeap.
 *
 * @param parentHH The source struct HM_HierarchicalHeap
 * @param childHH The struct HM_HierarchicalHeap set to be derived off of
 * 'parentHH'.
 * @param stealLevel The level at the parent which the child has stolen off of.
 */
PRIVATE void HM_HH_appendChild(pointer parentHHPointer,
                               pointer childHHPointer,
                               Word32 stealLevel);

/**
 * Gets the current level from a struct HM_HierarchicalHeap
 *
 * @param hhPointer The pointer to the struct HM_HierarchicalHeap to use
 *
 * @return The level of the HM_HierarchicalHeap
 */
PRIVATE size_t HM_HH_getLevel(pointer hhPointer);

/**
 * Merges the specified hierarchical heap back into its source hierarchical
 * heap.
 *
 * @attention
 * The specified heap must already be fully merged (i.e. its childHHList should
 * be empty).
 *
 * @attention
 * Once this function completes, the passed-in heap should be considered used
 * and all references to it dropped.
 *
 * @param hh The struct HM_HierarchicalHeap* to merge back into its source.
 */
PRIVATE void HM_HH_mergeIntoParent(pointer hhPointer);

/**
 * Promotes the chunks at the last level to the parent's level
 *
 * @param hhPointer The HM_HierarchicalHeap to modify
 */
PRIVATE void HM_HH_promoteChunks(pointer hhPointer);

/**
 * Sets the current level in a struct HM_HierarchicalHeap
 *
 * @param hhPointer The pointer to the struct HM_HierarchicalHeap to use
 * @param level The level to set
 */
PRIVATE void HM_HH_setLevel(pointer hhPointer, size_t level);
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/* RAM_NOTE: should take GC_state argument once I get that back in */

/**
 * Pretty-prints the hierarchical heap structure
 *
 * @param hh The struct HM_HierarchicalHeap to print
 * @param stream The stream to print to
 */
void HM_HH_display(const struct HM_HierarchicalHeap* hh, FILE* stream);

/**
 * This function ensures that the heap is not empty and has enough space to
 * satisfy allocation requests
 *
 * @param hh The struct HM_HierarchicalHeap to modify
 */
void HM_HH_ensureNotEmpty(struct HM_HierarchicalHeap* hh);

/**
 * This function extends the hierarchical heap with at least bytesRequested free
 * space.
 *
 * @attention
 * On successful completion, the frontier in hh->lastAllocatedChunk is updated
 * to the frontier of the extension and HM_getHierarchicalHeapLimit(hh) will
 * return the limit of the extension.
 *
 * @param hh The hierarchical heap to extend
 * @param bytesRequested The minimum size of the extension
 *
 * @return TRUE if extension succeeded, FALSE otherwise
 */
bool HM_HH_extend(struct HM_HierarchicalHeap* hh, size_t bytesRequested);

/**
 * Gets the containing hierarchical heap for the given objptr
 *
 * @attention
 * object <em>must</em> be within the hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap for
 *
 * @return The struct HM_HierarchicalHeap that 'object' belongs to
 */
struct HM_HierarchicalHeap* HM_HH_getContaining(GC_state s, objptr object);

/**
 * Returns the current hierarchical heap in use
 *
 * @param s The GC_state to use
 *
 * @return hh The struct HM_HierarchicalHeap in use
 */
struct HM_HierarchicalHeap* HM_HH_getCurrent(GC_state s);

/**
 * Returns the highest stolen level
 *
 * @param s The GC_state to use
 * @param hh The hierarchical heap to inspect
 *
 * @return HM_HH_INVALID_LEVEL if nothing was stolen, the highest stolen level
 * otherwise
 */
Word32 HM_HH_getHighestStolenLevel(GC_state s,
                                   const struct HM_HierarchicalHeap* hh);

/**
 * Gets the heap limit from a struct HM_HierarchicalHeap
 *
 * @param hh The struct HM_HierarchicalHeap to use
 *
 * @return the heap limit
 */
void* HM_HH_getLimit(const struct HM_HierarchicalHeap* hh);

/**
 * Gets the level of an objptr in the hierarchical heap
 *
 * @attention
 * objptr must be a valid, allocated HierarchicalHeap objptr!
 *
 * @param s The GC_state to use
 * @param object The objptr
 *
 * @return the level of the objptr
 */
Word32 HM_HH_getObjptrLevel(GC_state s, objptr object);

/**
 * Gets the frontier from a struct HM_HierarchicalHeap
 *
 * @param hh The struct HM_HierarchicalHeap to use
 *
 * @return the frontier of the currently active chunk.
 */
void* HM_HH_getFrontier(const struct HM_HierarchicalHeap* hh);

/**
 * Checks if 'candidateObjptr' belongs to the hierarchical heap space.
 *
 * @param s The GC_state to use
 * @param candidateObjptr The objptr to test
 *
 * @return TRUE if 'candidateObjptr' belongs to the hierarchical heap space,
 * FALSE otherwise
 */
bool HM_HH_objptrInHierarchicalHeap(GC_state s, objptr candidateObjptr);

/**
 * Returns offset into object to get at the struct HM_HierarchicalHeap
 *
 * @note
 * Objects are passed to runtime functions immediately <em>after</em> the
 * header, so we only need to pass the padding to get to the struct.
 *
 * @param s The GC_state to get alignment off of for HM_sizeofHierarchicalHeap()
 *
 * @return The offset into the object to get to the struct HM_HierarchicalHeap
 */
size_t HM_HH_offsetof(GC_state s);

/**
 * Updates the values in 'hh' to reflect mutator
 *
 * @param hh The struct HM_HierarchicalHeap to update
 * @param frontier The new frontier
 */
void HM_HH_updateValues(struct HM_HierarchicalHeap* hh,
                        void* frontier);

/**
 * Returns the sizeof the the struct HM_HierarchicalHeap in the heap including
 * header and padding
 *
 * @param s The GC_state to take alignment off of
 *
 * @return total size of the struct HM_HierarchicalHeap object
 */
size_t HM_HH_sizeof(GC_state s);

/**
 * Update pointers in the level list of the hierarchical heap passed in. This
 * should be called upon moving a hierarchical heap to ensure that all pointers
 * are forwarded.
 *
 * @param hhObjptr The objptr of the hierarchical heap to update
 */
void HM_HH_updateLevelListPointers(objptr hhObjptr);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */
