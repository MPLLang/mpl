/* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_ratios {
  float ramSlop;
  float stackCurrentGrow;
  float stackCurrentMaxReserved;
  float stackCurrentPermitReserved;
  float stackCurrentShrink;
  float stackMaxReserved;
  float stackShrink;
};

#define MAX_LCHS_INFINITE ((size_t)-1)

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
  bool mayLoadWorld;
  bool mayProcessAtMLton;
  bool messages; /* Print a message at the start and end of each gc. */
  bool HMMessages; /* print messages regarding heap management */
  size_t allocChunkSize;
  size_t minChunkSize;
  bool deferredPromotion;
  bool freeListCoalesce;
  bool mayUseAncestorChunk;
  bool oldHHGCPolicy;
  bool setAffinity; /* whether or not to set processor affinity */
  int32_t affinityBase; /* First processor to use when setting affinity */
  int32_t affinityStride; /* Number of processors between first and second */
  struct GC_ratios ratios;
  struct HM_HierarchicalHeapConfig hhConfig;
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
