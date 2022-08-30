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

/**
 * Ratios for Hierarchical Heap operations
 */
struct HM_HierarchicalHeapConfig {
  /* when (bytesAllocatedSinceLastCollection / bytesSurvivedLastCollection)
   * crosses this threshold, a local collection may be triggered. */
  double collectionThresholdRatio;

  /* the smallest amount of allocated data that can be collected in a
   * local collection */
  size_t minCollectionSize;

  /* smallest amount for a CC */
  size_t minCCSize;

  size_t maxCCChainLength;
  double ccThresholdRatio;
  uint32_t maxCCDepth;

  /* the shallowest depth that will be claimed for a local
   * collection. */
  uint32_t minLocalDepth;
};

enum GC_CollectionType {
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
  size_t allocChunkSize;
  size_t blockSize;
  size_t allocBlocksMinSize;
  size_t superblockThreshold; // upper bound on size-class of a superblock
  size_t megablockThreshold; // upper bound on size-class of a megablock (unmap above this threshold)
  float emptinessFraction;
  bool debugKeepFreeBlocks;
  bool manageEntanglement;
  bool freeListCoalesce;  /* disabled for now */
  bool setAffinity; /* whether or not to set processor affinity */
  int32_t affinityBase; /* First processor to use when setting affinity */
  int32_t affinityStride; /* Number of processors between first and second */
  struct GC_ratios ratios;
  struct HM_HierarchicalHeapConfig hhConfig;
  bool rusageMeasureGC;
  bool summary; /* Print a summary of gc info when program exits. */
  enum SummaryFormat summaryFormat;
  FILE* summaryFile;
  enum GC_CollectionType collectionType;
  /* Size of the trace buffer */
  size_t traceBufferSize;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool detailedGCTime (GC_state s);
static inline bool needGCTime (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
