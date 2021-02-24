/* Copyright (C) 2018-2021 Sam Westrick
 * Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef HIERARCHICAL_HEAP_H_
#define HIERARCHICAL_HEAP_H_

#include "chunk.h"
#include "concurrent-collection.h"

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct HM_UnionFindNode {
  struct HM_UnionFindNode *representative;
  struct HM_HierarchicalHeap *payload;

  /* In the union-find tree of HH records, any node that is not a representative
   * is a "dependant". These nodes only serve to redirect queries towards the
   * representative, and eventually become garbage, as they are "spliced out"
   * due to path compression.
   *
   * To make sure we don't leak a node, all dependants of each representative
   * are linked into a tree. Representatives are allowed to have at most one
   * dependant link, whereas dependants are allows up to two. This makes it
   * always possible to perform a union:
   *
   *                          r1
   *      r1    r2            |
   *      |     |     ===>    r2
   *      D1    D2            | \
   *                          D1 D2
   *
   * The advantage of using a tree (instead of e.g. a cycle or a flat linked
   * list) is parallelism. After a union:
   *   height(r1) <- 1 + max(height(r1), height(r2))
   * So, the final tree should be approximately balanced. We can then enumerate
   * all dependants efficiently in parallel. (This is important for parallel
   * GC, although we haven't implemented this yet.)
   */
  struct HM_UnionFindNode *dependant1;
  struct HM_UnionFindNode *dependant2;

} *HM_UnionFindNode;


typedef struct HM_HierarchicalHeap {
  struct HM_UnionFindNode *ufNode;
  uint32_t depth;

  struct HM_chunkList chunkList;

  /** This is a bit of a hack. For root (fully concurrent) collections,
    * we need to separate collected space from the space where new allocations
    * are permitted. Really, all we need is just a separate chunklist (and
    * this is what Jatin originally implemented). But for tracking HH objects
    * and their union-find dependants, having a completely separate HH object
    * is nice because then we can free dependants during root CC.
    */
  struct HM_HierarchicalHeap *subHeapForRootCC;

  struct HM_chunkList rememberedSet;
  struct ConcurrentPackage concurrentPack;

  /* The next non-empty ancestor heap. This may skip over "unused" levels.
   * Also, all threads have their own leaf-to-root path (essentially, path
   * copying) which is merged only at join points of the program. */
  struct HM_HierarchicalHeap *nextAncestor;

  size_t numDependants;
  size_t heightDependants;

} *HM_HierarchicalHeap;

#define HM_HH_INVALID_DEPTH CHUNK_INVALID_DEPTH

#else

struct HM_UnionFindNode;
typedef struct HM_UnionFindNode *HM_UnionFindNode;
struct HM_HierarchicalHeap;
typedef struct HM_HierarchicalHeap *HM_HierarchicalHeap;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline HM_UnionFindNode HM_HH_getUFNode(HM_HierarchicalHeap hh)
{
  return hh->ufNode;
}

static inline ConcurrentPackage HM_HH_getConcurrentPack(HM_HierarchicalHeap hh)
{
  return &(hh->concurrentPack);
}

static inline HM_chunkList HM_HH_getChunkList(HM_HierarchicalHeap hh)
{
  return &(hh->chunkList);
}

static inline HM_chunkList HM_HH_getRemSet(HM_HierarchicalHeap hh)
{
  return &(hh->rememberedSet);
}

HM_HierarchicalHeap HM_HH_new(GC_state s, uint32_t depth);

uint32_t HM_HH_getDepth(HM_HierarchicalHeap hh);

bool HM_HH_isLevelHead(HM_HierarchicalHeap hh);

bool HM_HH_isCCollecting(HM_HierarchicalHeap hh);
void HM_HH_addRootForCollector(HM_HierarchicalHeap hh, pointer p);

void HM_HH_merge(GC_state s, GC_thread parent, GC_thread child);
void HM_HH_promoteChunks(GC_state s, GC_thread thread);
void HM_HH_ensureNotEmpty(GC_state s, GC_thread thread);

bool HM_HH_extend(GC_state s, GC_thread thread, size_t bytesRequested);

/* zip-up hh1 and hh2, returning the new deepest leaf
 * (will be one of hh1 or hh2) */
HM_HierarchicalHeap HM_HH_zip(
  GC_state s,
  HM_HierarchicalHeap hh1,
  HM_HierarchicalHeap hh2);

/* Find the HH at the indicated depth (create one if doesn't exist) */
HM_HierarchicalHeap HM_HH_getHeapAtDepth(GC_state s, GC_thread thread, uint32_t depth);

HM_HierarchicalHeap HM_HH_getCurrent(GC_state s);
pointer HM_HH_getFrontier(GC_thread thread);
pointer HM_HH_getLimit(GC_thread thread);
void HM_HH_updateValues(GC_thread thread, pointer frontier);

size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize);
size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes);

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread);

void HM_HH_forceLeftHeap(uint32_t processor, pointer threadp);
pointer HM_HH_getRoot(pointer threadp);
void HM_HH_registerCont(pointer kl, pointer kr, pointer k, pointer threadp);
void HM_HH_resetList(pointer threadp);


/** Very fancy (constant-space) loop that frees each dependant union-find
  * node of hh. Specifically, calls this on each dependant ufnode:
  *
  *   freeFixedSize(getUFAllocator(s), ufnode)
  *
  * Or, if specified to retire instead:
  *
  *   HH_EBR_retire(s, ufnode);
  *
  * Note that this does NOT free hh, or its corresponding ufnode.
  */
void HM_HH_freeAllDependants(
  GC_state s,
  HM_HierarchicalHeap hh,
  bool retireInsteadOfFree);


#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */
