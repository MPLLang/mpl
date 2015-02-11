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
 * them against a guarantor. The method used requires chunk sizes to be integral
 * multiples of the minimum chunk size.
 */

#include "chunk-pool.h"

#include <pthread.h>

/***********/
/* Structs */
/***********/
/**
 * @brief
 * This struct contains the metadata for each chunk
 *
 * <tt>previous</tt> and <tt>next</tt> are used in the free list to link to
 * other chunks. When in the free list, this struct contains information about
 * the span the the chunk starts, specifically the number of chunks in the span
 * (<tt>numChunksInSpan</tt>). When not in a list, this struct contains a
 * pointer to the first chunk of the span it belongs to
 * (<tt>spanStart</tt>). The address of the struct is converted
 * to an index and specifies which chunk this struct describes. This design
 * allows for easy chunk to metadata and metadata to chunk transformations using
 * simple pointer arithmetic.
 */
struct ChunkPool_chunkMetadata {
  union {
    size_t numChunksInSpan;
    struct ChunkPool_chunkMetadata* spanStart;
  } spanInfo;
  struct ChunkPool_chunkMetadata* previous;
  struct ChunkPool_chunkMetadata* next;
};

/******************************/
/* Static Function Prototypes */
/******************************/
/**
 * Finds the index of the free list that contains spans of size
 * <tt>numChunks</tt>
 *
 * @param numChunks number of chunks in a span to find the free list of
 *
 * @return index of the free list which contains spans of size
 * <tt>numChunks</tt>
 */
static unsigned int ChunkPool_findFreeListIndexForNumChunks (size_t numChunks);

/**
 * Inserts the chunkMetadata object into the proper free list
 *
 * @param chunkMetadata the object to insert
 */
static void ChunkPool_insertIntoFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata);

/**
 * Removes the chunkMetadata object from its list
 *
 * @param chunkMetadata the object to remove
 */
static void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata);

/**
 * Initialize a span
 *
 * @param spanStart the first chunk in the span, which contains
 * the <em>initialized</em> metadata for the span
 */
static void ChunkPool_initializeSpan (
    struct ChunkPool_chunkMetadata* spanStart);

/**
 * Splits a given span if possible.
 *
 * @param newSpanStart Variable to populate with pointer to chunk
 * metadata of the first chunk in the new span.
 * @param chunkMetadata The span to split.
 * @param numChunks The number of chunks to split to
 *
 * @return If split, <tt>newSpanStart<tt> is populated with the
 * pointer to the chunk metadata of the first chunk in the new span created by
 * splitting. In addition, the span referred to by <tt>chunkMetadata</tt> is
 * shrunk to <tt>numChunks</tt> in size. If no split is possible,
 * <tt>firstChunkMretadataInNewSpan</tt> is set to <tt>NULL</tt> and
 * <tt>chunkMetadata</tt> is unchanged.
 */
static void ChunkPool_maybeSplitSpan (
    struct ChunkPool_chunkMetadata** newSpanStart,
    struct ChunkPool_chunkMetadata* chunkMetadata,
    size_t numChunks);

/**
 * Map chunkMetadata object to the chunk pointer
 *
 * @param chunkMetdata the object to map
 *
 * @return The corresponding chunk
 */
static void* ChunkPool_chunkMetadataToChunk (
    const struct ChunkPool_chunkMetadata* chunkMetadata);

/**
 * Map chunk pointer to the chunkMetadata object
 *
 * @param chunk the chunk to map
 *
 * @return The corresponding chunkMetadata
 */
static struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    void* const chunk);

/*************/
/* Constants */
/*************/
/**
 * This specifies the compiled-in minimum chunk size. All allocation requests
 * will be satisfied with chunks that are at least this size
 */
#define ChunkPool_CHUNKADDRESSMASK (ChunkPool_MINIMUMCHUNKSIZE - 1)
/* 2^12 bytes == 4KiB */
#define ChunkPool_MINIMUMCHUNKSIZE (1ULL << 12)
#define ChunkPool_NUMFREELISTS (256)
#define ChunkPool_NONFIRSTCHUNK ((struct ChunkPool_chunkMetadata*)(-1LL))
#define ChunkPool_ALLOCATED ((struct ChunkPool_chunkMetadata*)(-2LL))

/*************/
/* Variables */
/*************/
static bool ChunkPool_initialized = false;

static pthread_rwlock_t ChunkPool_lock = PTHREAD_RWLOCK_INITIALIZER;

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
  /* I die if I hit an error anyways, so just set this early */
  ChunkPool_initialized = true;

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
    diee(__FILE__ ":%d: mmap() failed with errno %d", __LINE__, errno);
  }

  /* setup pool variables */
  ChunkPool_poolStart = region;
  ChunkPool_poolEnd = ((uint8_t*)(region)) + adjustedPoolSize;

  /* setup metadatas variable */
  unsigned int numChunks = adjustedPoolSize / ChunkPool_MINIMUMCHUNKSIZE;
  ChunkPool_chunkMetadatas = malloc_safe (numChunks *
                                          sizeof(*ChunkPool_chunkMetadatas));
  ChunkPool_chunkMetadatas->spanInfo.numChunksInSpan = numChunks;

  /* Add the giant chunk into the free list */
  ChunkPool_insertIntoFreeList (ChunkPool_chunkMetadatas);
}

/**
 * This function is implemented and serializes against all other functions.
 */
void* ChunkPool_allocate (size_t* bytesRequested) {
  assert (ChunkPool_initialized);

  assert (*bytesRequested > 0);

  /* adjust requested to multiple of minimum chunk size */
  *bytesRequested = (*bytesRequested + ChunkPool_MINIMUMCHUNKSIZE - 1) &
                    ~ChunkPool_CHUNKADDRESSMASK;
  size_t chunksRequested = *bytesRequested / ChunkPool_MINIMUMCHUNKSIZE;

  pthread_rwlock_wrlock_safe(&ChunkPool_lock);
  /* Search for chunk to satisfy */
  for (int freeListIndex =
           ChunkPool_findFreeListIndexForNumChunks (chunksRequested);
       freeListIndex < ChunkPool_NUMFREELISTS;
       freeListIndex++) {
    for (struct ChunkPool_chunkMetadata* cursor =
             ChunkPool_freeLists[freeListIndex];
         NULL != cursor;
         cursor = cursor->next) {
      if (chunksRequested <= cursor->spanInfo.numChunksInSpan) {
        /*
         * split the span if possible and return the chunk
         */
        /* first remove the span from the list */
        ChunkPool_removeFromFreeList (cursor);

        /* now split */
        struct ChunkPool_chunkMetadata* newSpanStart;
        ChunkPool_maybeSplitSpan (&newSpanStart, cursor, chunksRequested);

        if (NULL != newSpanStart) {
          /* insert residual back into freelist */
          ChunkPool_insertIntoFreeList (newSpanStart);
        }

        /* done with free list modifications at this point, so unlock */
        pthread_rwlock_unlock_safe(&ChunkPool_lock);

        /* initialize span and return */
        ChunkPool_initializeSpan (cursor);
        cursor->previous = ChunkPool_ALLOCATED;
        cursor->next = ChunkPool_ALLOCATED;
        return ChunkPool_chunkMetadataToChunk (cursor);
      }
    }
  }

  /*
   * If execution reaches here, then I do not have a span to satisfy the request
   */
  pthread_rwlock_unlock_safe(&ChunkPool_lock);
  return NULL;
}

/**
 * This function is implemented and serializes against all other functions.
 */
bool ChunkPool_free (void* chunk) {
  assert (ChunkPool_initialized);

  struct ChunkPool_chunkMetadata* spanStart =
      ChunkPool_chunkToChunkMetadata (chunk);

  assert (ChunkPool_ALLOCATED == spanStart->previous);
  assert (ChunkPool_ALLOCATED == spanStart->next);

  pthread_rwlock_wrlock_safe(&ChunkPool_lock);

  /* Get the previous and next spans for coalescing */
  struct ChunkPool_chunkMetadata* previousSpanStart = spanStart - 1;
  if (ChunkPool_NONFIRSTCHUNK == previousSpanStart->previous) {
    assert (ChunkPool_NONFIRSTCHUNK == previousSpanStart->next);

    previousSpanStart = previousSpanStart->spanInfo.spanStart;

    assert (ChunkPool_NONFIRSTCHUNK != previousSpanStart->previous);
    assert (ChunkPool_NONFIRSTCHUNK != previousSpanStart->next);
  }

  struct ChunkPool_chunkMetadata* nextSpanStart =
      spanStart + (spanStart->spanInfo.numChunksInSpan);
  assert (ChunkPool_NONFIRSTCHUNK != previousSpanStart->previous);
  assert (ChunkPool_NONFIRSTCHUNK != previousSpanStart->next);

  if (ChunkPool_ALLOCATED != previousSpanStart->previous) {
    /* coalesceable! remove from list and join */
    ChunkPool_removeFromFreeList (previousSpanStart);
    previousSpanStart->spanInfo.numChunksInSpan += spanStart->spanInfo.numChunksInSpan;

    /*
     * set spanStart to previousSpanStart so that I don't have to branch on
     * for coalescing nextSpanStart
     */
    spanStart = previousSpanStart;
  }

  if (ChunkPool_ALLOCATED != nextSpanStart->previous) {
    /* coalesceable! remove from list and join */
    ChunkPool_removeFromFreeList (nextSpanStart);
    spanStart->spanInfo.numChunksInSpan += nextSpanStart->spanInfo.numChunksInSpan;
  }

  /*
   * At this point, spanStart points to the possible coalesced span. Reinsert it
   * into the free list!
   */
  ChunkPool_insertIntoFreeList (spanStart);

  pthread_rwlock_unlock_safe(&ChunkPool_lock);

  return true;
}

/**
 * This function serializes against <tt>ChunkPool_allocate()</tt> and
 * <tt>ChunkPool_free()</tt> but is reentrant against itself. It works by
 * finding the containing minimum size chunk of <tt>object</tt> and then finding
 * the containing span of that chunk via <tt>ChunkPool_chunkMetadatas</tt>. From
 * this, the containing chunk that was returned by <tt>ChunkPool_allocate</tt>
 * is found.
 */
void* ChunkPool_find (void* object) {
  assert (ChunkPool_initialized);

  assert (object >= ChunkPool_poolStart);
  assert (object < ChunkPool_poolEnd);

  void* containingChunk = ((void*)(((size_t)(object)) &
                                   ~ChunkPool_CHUNKADDRESSMASK));
  struct ChunkPool_chunkMetadata* chunkMetadata =
      ChunkPool_chunkToChunkMetadata (containingChunk);
  void* chunk = NULL;

  /*
   * RAM_NOTE: May not need to lock if I can guarantee that these chunks will
   * not enter free list or be re-initialized
   */
  pthread_rwlock_rdlock_safe(&ChunkPool_lock);
  if (ChunkPool_NONFIRSTCHUNK == chunkMetadata->previous) {
    assert (ChunkPool_NONFIRSTCHUNK == chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata->spanInfo.spanStart);
  } else {
    assert  (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata);
  }
  pthread_rwlock_unlock_safe(&ChunkPool_lock);

  return chunk;
}

bool ChunkPool_pointerInChunkPool (void* candidate) {
  assert (ChunkPool_initialized);

  return ((candidate >= ChunkPool_poolStart) &&
          (candidate < ChunkPool_poolEnd));
}

unsigned int ChunkPool_findFreeListIndexForNumChunks (size_t numChunks) {
  assert (ChunkPool_initialized);

  assert (numChunks > 0);

  if (numChunks < ChunkPool_NUMFREELISTS) {
    return (numChunks - 1);
  } else {
    return (ChunkPool_NUMFREELISTS - 1);
  }

  /* return statement here to satisfy gcc */
  return (ChunkPool_NUMFREELISTS - 1);
}

void ChunkPool_insertIntoFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  struct ChunkPool_chunkMetadata** list =
      &(ChunkPool_freeLists[ChunkPool_findFreeListIndexForNumChunks (
          chunkMetadata->spanInfo.numChunksInSpan)]);

  chunkMetadata->previous = NULL;
  chunkMetadata->next = *list;
  if (NULL != *list) {
    (*list)->previous = chunkMetadata;
  }
  *list = chunkMetadata;
}

void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->previous);
  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);

  struct ChunkPool_chunkMetadata** list =
      &(ChunkPool_freeLists[ChunkPool_findFreeListIndexForNumChunks (
          chunkMetadata->spanInfo.numChunksInSpan)]);

  if (NULL != chunkMetadata->previous) {
    chunkMetadata->previous->next = chunkMetadata->next;
  }

  if (NULL != chunkMetadata->next) {
    chunkMetadata->next->previous = chunkMetadata->previous;
  }

  if (*list == chunkMetadata) {
    assert (NULL == chunkMetadata->previous);
    *list = chunkMetadata->next;
  }
}

void ChunkPool_initializeSpan (struct ChunkPool_chunkMetadata* spanStart) {
  assert (ChunkPool_initialized);

  /* setup the forwarding pointers */
  for (unsigned int chunkOffset = 1;
       chunkOffset < spanStart->spanInfo.numChunksInSpan;
       chunkOffset++) {
    struct ChunkPool_chunkMetadata* chunkMetadata =
        spanStart + chunkOffset;
    chunkMetadata->spanInfo.spanStart = spanStart;
    chunkMetadata->previous = ChunkPool_NONFIRSTCHUNK;
    chunkMetadata->next = ChunkPool_NONFIRSTCHUNK;
  }
}

void ChunkPool_maybeSplitSpan (
    struct ChunkPool_chunkMetadata** newSpanStart,
    struct ChunkPool_chunkMetadata* chunkMetadata,
    size_t splitChunks) {
  assert (ChunkPool_initialized);

  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->previous);
  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);
  assert (chunkMetadata->spanInfo.numChunksInSpan >= splitChunks);

  if (chunkMetadata->spanInfo.numChunksInSpan == splitChunks) {
    /* nothing to do! */
    *newSpanStart = NULL;
  } else {
    /* can split, so should split */
    *newSpanStart = chunkMetadata + splitChunks;
    (*newSpanStart)->spanInfo.numChunksInSpan =
        chunkMetadata->spanInfo.numChunksInSpan - splitChunks;
    chunkMetadata->spanInfo.numChunksInSpan = splitChunks;
  }
}

void* ChunkPool_chunkMetadataToChunk (
    const struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  unsigned int chunkIndex = chunkMetadata - ChunkPool_chunkMetadatas;

  assert (((void*)(((uint8_t*)(ChunkPool_poolStart)) +
                   (chunkIndex * ChunkPool_MINIMUMCHUNKSIZE))) <
          ChunkPool_poolEnd);
  return (((uint8_t*)(ChunkPool_poolStart)) +
          (chunkIndex * ChunkPool_MINIMUMCHUNKSIZE));
}

struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    void* const chunk) {
  assert (ChunkPool_initialized);

  assert (0 == (((size_t)(chunk)) & ChunkPool_CHUNKADDRESSMASK));
  assert (chunk >= ChunkPool_poolStart);
  assert (chunk < ChunkPool_poolEnd);

  unsigned int chunkIndex =
      (((uint8_t*)(chunk)) - ((uint8_t*)(ChunkPool_poolStart))) / ChunkPool_MINIMUMCHUNKSIZE;
  return (ChunkPool_chunkMetadatas + chunkIndex);
}
