/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk-pool.c
 *
 * @author Ram Raghunathan
 *
 * @brief
 * This is a naive implementation of the Chunk Pool interface with minimal
 * optimizations. It requires chunk sizes to be powers of 2 and multiples of
 * <tt>ChunkPool_MINIMUMCHUNKSIZE</tt>
 *
 * This implementation of the Chunk Pool interface found in chunk-pool.h is a
 * simple version that has serialized access to <tt>ChunkPool_allocate()</tt>
 * and <tt>ChunkPool_free</tt> along with a RW-lock on
 * <tt>ChunkPool_find()</tt>. It does not do any other multithreaded access or
 * locality optimizations. It is backed by a <tt>mmap()</tt>'d memory region.
 *
 * Finding a chunk is done by checking candidate chunk beginnings and verifying
 * them against a guarantor.
 The method used requires chunk sizes to be powers
 * of 2 and integral multiples of the minimum chunk size.
 */

/***********/
/* Structs */
/***********/
/**
 * @brief
 * This struct contains the metadata for each chunk
 *
 * <tt>previous</tt> and <tt>next</tt> are used in the free list to link to
 * other chunks. When in the free list, this struct contains information about
 * the span the the chunk starts, specifically the number of chunks in the
 * span (<tt>numChunksInSpan</tt>). When not in a list, this struct contains
 * a pointer to the first chunk of the span it belongs to
 * (<tt>firstChunkInSpan</tt>). The address of the struct is converted to an
 * index and specifies which chunk this struct describes. This design allows for
 * easy chunk to metadata and metadata to chunk transformations using simple
 * pointer arithmetic.
 */
struct ChunkPool_chunkMetadata {
  union {
    size_t numChunksInSpan;
    struct ChunkPool_chunkMetadata* firstChunkInSpan;
  };
  struct ChunkPool_chunkMetadata* previous;
  struct ChunkPool_chunkMetadata* next;
};

/******************************/
/* Static Function Prototypes */
/******************************/
static unsigned int ChunkPool_findFreeListIndexForSize (size_t size);
static void ChunkPool_insertIntoFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata* list);
static void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata* list);

/*************/
/* Constants */
/*************/
/**
 * This specifies the compiled-in minimum chunk size. All allocation requests
 * will be satisfied with chunks that are at least this size
 */
 /* 12 bits */
#define ChunkPool_CHUNKADDRESSMASK (0xfffULL)
/* 2^12 bytes == 4KiB */
#define ChunkPool_MINIMUMCHUNKSIZE (1ULL << ChunkPool_CHUNKADDRESSMASK)
#define ChunkPool_NUMFREELISTS (256)
#define ChunkPool_NONFIRSTCHUNK ((struct ChunkPool_chunkMetadata*)(-1LL))

/*************/
/* Variables */
/*************/
static void* ChunkPool_poolStart = NULL;
static void* ChunkPool_poolEnd = NULL;
static struct ChunkPool_chunkMetadata* ChunkPool_chunkMetadatas = NULL;
static struct ChunkPool_chunkMetadata*
ChunkPool_freeLists[ChunkPool_NUMFREELISTS] = {0};


/*************/
/* Functions */
/*************/
/**
 * This function <em>must</em> be called before calling any other functions. It
 * is not thread-safe. The actual maximum pool size will be the maximum multiple
 * of <tt>ChunkPool_MINIMUMCHUNKSIZE</tt> that is less than or equal to
 * <tt>poolSize</tt>.
 */
void ChunkPool_initialize (size_t poolSize) {
  /* get adjusted pool size */
  size_t adjustedPoolSize = poolSize & ~ChunkPool_CHUNKADDRESSMASK;

  /* map the chunks */
  void* region = mmap (NULL,
                       adjustedPoolSize,
                       PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS,
                       -1,
                       0);
  if (MAP_FAILED == region) {
    diee(__FILE__ ":%d: mmap failed with errno %d", __LINE__, errno);
  }

  /* setup pool variables */
  ChunkPool_poolStart = region;
  ChunkPool_poolEnd = region + adjustedPoolSize;

  /* setup metadatas variable */
  unsigned int numChunks = adjustedPoolSize / ChunkPool_MINIMUMCHUNKSIZE;
  ChunkPool_chunkMetadatas = malloc_safe (numChunks *
                                          sizeof(*ChunkPool_chunkMetadatas));

  /* Coalesce entire region into one gigantic chunk */
  ChunkPool_chunkMetadatas[0].numChunksInSpan = numChunks;
  ChunkPool_chunkMetadatas[0].previous = NULL;
  ChunkPool_chunkMetadatas[0].next = NULL;
  for (unsigned int i = 1; i < numChunks; i++) {
    ChunkPool_chunkMetadatas[i].firstChunkInSpan = &(ChunkPool_chunkMetadatas[0]);
    ChunkPool_chunkMetadatas[i].previous = ChunkPool_NONFIRSTCHUNK;
    ChunkPool_chunkMetadatas[i].next = ChunkPool_NONFIRSTCHUNK;
  }

  /* Add the giant chunk into the free list */
  unsigned int freeListIndex =
      ChunkPool_findFreeListIndexForSize (adjustedPoolSize);
  ChunkPool_insertIntoFreeList (&(ChunkPool_chunkMetadatas[0]),
                                ChunkPool_freeLists[freeListIndex]);
}

/**
 * This function is implemented and serializes against all other functions.
 */
PRIVATE void* ChunkPool_allocate (size_t* bytesRequested) {
#error Unimplemented!
}

/**
 * This function is implemented and serializes against all other functions.
 */
PRIVATE bool ChunkPool_free (void* chunk) {
#error Unimplemented!
}

/**
 * This function serializes against <tt>ChunkPool_allocate()</tt> and
 * <tt>ChunkPool_free()</tt> but is reentrant against itself. It works by
 * consecutively checking candidate chunk beginnings and verifying them against
 * a guarantor array. This continues until the chunk is found or the function
 * runs out of candidates to test. For fast performance, this method requires
 * chunk sizes to be powers of 2 and integral multiples of the minimum chunk
 * size.
 */
PRIVATE void* ChunkPool_find (void* object) {
#error Unimplemented!
}

unsigned int ChunkPool_findFreeListIndexForSize (size_t size) {
  for (unsigned int multiplier = 1;
       multiplier < ChunkPool_NUMFREELISTS;
       multiplier++) {
    if (size <= (multiplier * ChunkPool_MINIMUMCHUNKSIZE)) {
      return (multiplier - 1);
    }
  }

  /*
   * If execution reaches here,
   * size >= (ChunkPool_NUMFREELISTS * ChunkPool_MINIMUMCHUNKSIZE)
   */
  return (ChunkPool_NUMFREELISTS - 1);
}

void ChunkPool_insertIntoFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata* list) {
  #error Unimplemented!
}

void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata* list) {
  #error Unimplemented!
}
