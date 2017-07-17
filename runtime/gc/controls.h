/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_ratios {
  /* Minimum live ratio to use copying GC. */
  float copy;
  /* Only use generational GC with copying collection if the ratio of
   * heap size to live data size is below copyGenerational.
   */
  float copyGenerational;
  float grow;
  float hashCons;
  /* Desired ratio of heap size to live data. */
  float live;
  /* Minimum live ratio to use mark-compact GC. */
  float markCompact;
  /* Only use generational GC with mark-compact collection if the
   * ratio of heap size to live data size is below
   * markCompactGenerational.
   */
  float markCompactGenerational;
  /* As long as the ratio of bytes live to nursery size is greater
   * than nurseryRatio, use minor GCs.
   */
  float nursery;
  float ramSlop;
  float stackCurrentGrow;
  float stackCurrentMaxReserved;
  float stackCurrentPermitReserved;
  float stackCurrentShrink;
  float stackMaxReserved;
  float stackShrink;
  /* Limit available memory as a function of _max_live_. */
  float available;
};

/**
 * Ratios for Hierarchical Heap operations
 */
struct HM_HierarchicalHeapConfig {
  double allocatedRatio; /**< the ratio of PoolSize:AllocatedBytes under which a
                          * collection is triggered */

  double liveLCRatio; /**< minimum LCHS:LCS ratio to maintain */

  size_t initialLCHS; /**< initial LCHS, in bytes */

  size_t maxLCHS; /**< maximum LCHS, in bytes. */

};

enum HHCollectionLevel {
  ALL,
  LOCAL,
  SUPERLOCAL,
  NONE
};

enum SummaryFormat {
  HUMAN,
  JSON
};

struct GC_controls {
  size_t fixedHeap; /* If 0, then no fixed heap. */
  size_t maxHeap; /* if zero, then unlimited, else limit total heap */
  bool mayLoadWorld;
  bool mayPageHeap; /* Permit paging heap to disk during GC */
  bool mayProcessAtMLton;
  bool messages; /* Print a message at the start and end of each gc. */
  bool HMMessages; /* print messages regarding heap management */
  size_t oldGenArraySize; /* Arrays larger are allocated in old gen, if possible. */
  size_t allocChunkSize; /* Minimum size reserved for any allocation request. */
  int32_t affinityBase; /* First processor to use when setting affinity */
  int32_t affinityStride; /* Number of processors between first and second */
  bool restrictAvailableSize; /* Use smaller heaps to improve space profiling accuracy */
  struct GC_ratios ratios;
  struct HM_HierarchicalHeapConfig hhConfig;
  struct ChunkPool_config chunkPoolConfig;
  bool rusageMeasureGC;
  bool summary; /* Print a summary of gc info when program exits. */
  enum SummaryFormat summaryFormat;
  FILE* summaryFile;
  enum HHCollectionLevel hhCollectionLevel;
  /* Size of the trace buffer */
  size_t traceBufferSize;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool detailedGCTime (GC_state s);
static inline bool needGCTime (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
