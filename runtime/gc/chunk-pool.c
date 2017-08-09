
/* Copyright (C) 2014-2017 Ram Raghunathan.
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
 * and <tt>ChunkPool_free</tt> along with a unlocked access to metadata
 * functions like <tt>ChunkPool_find()</tt>. It does not do any other
 * multithreaded access or locality optimizations. It is backed by a
 * <tt>mmap()</tt>'d memory region.
 *
 * Finding a chunk is done by checking candidate chunk beginnings and verifying
 * them against a guarantor. The method used requires chunk sizes to be integral
 * multiples of the minimum chunk size.
 */

#include "chunk-pool.h"
#include "util.h"

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
 * Performs the actual chunk free operation.
 *
 * @attention
 * Does not lock! The lock <em>must</em> be held before calling this function.
 *
 * @param chunk The chunk to free
 *
 * @return TRUE on success, FALSE otherwise.
 */
bool ChunkPool_performFree(void* chunk);

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
    const volatile struct ChunkPool_chunkMetadata* chunkMetadata);

/**
 * Map chunk pointer to the chunkMetadata object
 *
 * @param chunk the chunk to map
 *
 * @return The corresponding chunkMetadata
 */
static struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    const void* chunk);

/*************/
/* Constants */
/*************/
/**
 * This specifies the compiled-in minimum chunk size. All allocation requests
 * will be satisfied with chunks that are at least this size
 */
#define ChunkPool_CHUNKADDRESSMASK (ChunkPool_MINIMUMCHUNKSIZE - 1)
/* 4KiB */
#define ChunkPool_MINIMUMCHUNKSIZE ((size_t)(512ULL * 1024))
#define ChunkPool_NUMFREELISTS ((size_t)(256ULL))
#define ChunkPool_NONFIRSTCHUNK ((struct ChunkPool_chunkMetadata*)(-1LL))
#define ChunkPool_ALLOCATED ((struct ChunkPool_chunkMetadata*)(-2LL))

/*************/
/* Variables */
/*************/
static bool ChunkPool_initialized = FALSE;

static pthread_mutex_t ChunkPool_lock = PTHREAD_MUTEX_INITIALIZER;

static struct ChunkPool_config ChunkPool_config;
static void* ChunkPool_poolStart = NULL;
static void* ChunkPool_poolEnd = NULL;
static size_t ChunkPool_bytesAllocated = 0ULL;
static size_t ChunkPool_maxBytesAllocated = 0ULL;
static size_t ChunkPool_currentPoolSize = 0ULL;

static struct ChunkPool_chunkMetadata* ChunkPool_chunkMetadatas = NULL;
static struct ChunkPool_chunkMetadata* ChunkPool_chunkMetadatasEnd = NULL;
static struct ChunkPool_chunkMetadata*
ChunkPool_freeLists[ChunkPool_NUMFREELISTS] = {NULL};


/*************/
/* Functions */
/*************/
/**
 * This function is not thread-safe.
 */
void ChunkPool_initialize (struct ChunkPool_config* config) {
  assert(!ChunkPool_initialized);

  ChunkPool_config = *config;

  /* adjust and assert inputs */
  /* round down maxSize */
  ChunkPool_config.maxSize =
      ChunkPool_config.maxSize & ~ChunkPool_CHUNKADDRESSMASK;

  /* round down initialSize to a minimum of the page size */
  ChunkPool_config.initialSize =
      ChunkPool_config.initialSize & ~ChunkPool_CHUNKADDRESSMASK;
  ChunkPool_config.initialSize =
      (0 == ChunkPool_config.initialSize) ?
      (ChunkPool_MINIMUMCHUNKSIZE) : (ChunkPool_config.initialSize);

  if (ChunkPool_config.liveRatio < 1.0) {
    DIE("ChunkPool live ratio %f < 1.0 is illegal.",
        ChunkPool_config.liveRatio);
  }

  /* at this point, the inputs are adjusted and ready to use */

  /* map the chunks */
  void* region = mmap (NULL,
                       ChunkPool_config.maxSize + ChunkPool_MINIMUMCHUNKSIZE,
                       PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE,
                       -1,
                       0);
  if (MAP_FAILED == region) {
    DIE("mmap() failed with errno %d.", errno);
  }

  /* setup pool variables */
  ChunkPool_poolStart =
      ((void*)((((size_t)(region)) + ChunkPool_MINIMUMCHUNKSIZE - 1) &
               ~ChunkPool_CHUNKADDRESSMASK));
  ChunkPool_poolEnd = ((uint8_t*)(ChunkPool_poolStart)) +
                      ChunkPool_config.maxSize;
  ChunkPool_currentPoolSize = ChunkPool_config.initialSize;

  LOG(LM_CHUNK_POOL, LL_INFO,
      "Created chunk pool of size %zu bytes at %p",
      ChunkPool_config.maxSize,
      ChunkPool_poolStart);

  VALGRIND_CREATE_MEMPOOL(&ChunkPool_poolStart, 0, 0);
  VALGRIND_MAKE_MEM_NOACCESS(ChunkPool_poolStart, ChunkPool_config.maxSize);

  /* setup metadatas variable */
  size_t numChunks = ChunkPool_config.maxSize / ChunkPool_MINIMUMCHUNKSIZE;
  ChunkPool_chunkMetadatas = malloc_safe (numChunks *
                                          sizeof(*ChunkPool_chunkMetadatas));
  ChunkPool_chunkMetadatasEnd = ChunkPool_chunkMetadatas + numChunks;
  ChunkPool_chunkMetadatas->spanInfo.numChunksInSpan = numChunks;

  LOG(LM_CHUNK_POOL, LL_INFO,
      "Created chunk pool metadata of size %zu bytes at %p",
      numChunks * sizeof(*ChunkPool_chunkMetadatas),
      ((void*)(ChunkPool_chunkMetadatas)));

  /* we are now initialized */
  ChunkPool_initialized = TRUE;

  /* Add the giant chunk into the free list */
  ChunkPool_insertIntoFreeList (ChunkPool_chunkMetadatas);
}

/**
 * Currently, this function fakes the resize by changing the reported pool
 * size. This call is *not* serialized.
 */
void ChunkPool_maybeResize(void) {
  size_t oldPoolSize = ChunkPool_currentPoolSize;

  double ratio = ((double)(ChunkPool_currentPoolSize)) /
                 ((double)(ChunkPool_bytesAllocated));

  if (ratio < (2 * (ChunkPool_config.liveRatio + 1))) {
    size_t preferredNewPoolSize =
        (2 * (ChunkPool_config.liveRatio + 1)) * ChunkPool_bytesAllocated;

    ChunkPool_currentPoolSize =
        (preferredNewPoolSize > ChunkPool_config.maxSize) ?
        ChunkPool_config.maxSize : preferredNewPoolSize;
  }

  if (oldPoolSize != ChunkPool_currentPoolSize) {
    LOG(LM_CHUNK_POOL, LL_DEBUG,
        "Live Ratio %.2f < %.2f, so resized Chunk Pool from %zu bytes to %zu "
        "bytes",
        ratio,
        2 * (ChunkPool_config.liveRatio + 1),
        oldPoolSize,
        ChunkPool_currentPoolSize);
  }
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

  pthread_mutex_lock_safe(&ChunkPool_lock);

  ChunkPool_bytesAllocated += *bytesRequested;
  assert(ChunkPool_bytesAllocated <= ChunkPool_config.maxSize);
  assert((ChunkPool_bytesAllocated % ChunkPool_MINIMUMCHUNKSIZE) == 0);

  /* Search for chunk to satisfy */
  for (size_t freeListIndex =
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
        assert(cursor->spanInfo.numChunksInSpan >= chunksRequested);

        if (NULL != newSpanStart) {
          /* insert residual back into freelist */
          ChunkPool_insertIntoFreeList(newSpanStart);
        }

        /* initialize span and tidy up*/
        ChunkPool_initializeSpan (cursor);
        cursor->previous = ChunkPool_ALLOCATED;
        cursor->next = ChunkPool_ALLOCATED;

        /* done with free list modifications at this point, so unlock */
        pthread_mutex_unlock_safe(&ChunkPool_lock);
        void* chunk = ChunkPool_chunkMetadataToChunk (cursor);
        LOG(LM_CHUNK_POOL, LL_DEBUG, "Allocating chunk %p", chunk);
        VALGRIND_MEMPOOL_ALLOC(&ChunkPool_poolStart, chunk, *bytesRequested);

        /* update maxBytesAllocated */
        if (ChunkPool_bytesAllocated > ChunkPool_maxBytesAllocated) {
          ChunkPool_maxBytesAllocated = ChunkPool_bytesAllocated;
        }

        return chunk;
      }
    }
  }

  /*
   * If execution reaches here, then I do not have a span to satisfy the request
   */
  ChunkPool_bytesAllocated -= *bytesRequested;
  assert(ChunkPool_bytesAllocated <= ChunkPool_config.maxSize);
  assert((ChunkPool_bytesAllocated % ChunkPool_MINIMUMCHUNKSIZE) == 0);

  pthread_mutex_unlock_safe(&ChunkPool_lock);

  return NULL;
}

/**
 * This function is implemented and serializes against all other functions in a
 * batch.
 */
bool ChunkPool_iteratedFree (ChunkPool_BatchFreeFunction f, void* fArgs) {
  assert (ChunkPool_initialized);

  pthread_mutex_lock_safe(&ChunkPool_lock);
  for (void* chunk = f(fArgs);
       NULL != chunk;
       chunk = f(fArgs)) {
    if (!ChunkPool_performFree(chunk)) {
      pthread_mutex_unlock_safe(&ChunkPool_lock);
      return FALSE;
    }
  }
  pthread_mutex_unlock_safe(&ChunkPool_lock);

  return TRUE;
}

/**
 * This function is implemented and serializes against all other functions.
 */
bool ChunkPool_free (void* chunk) {
  assert (ChunkPool_initialized);

  pthread_mutex_lock_safe(&ChunkPool_lock);
  bool result = ChunkPool_performFree(chunk);
  pthread_mutex_unlock_safe(&ChunkPool_lock);

  return result;
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

  const void* containingChunk = ((void*)(((size_t)(object)) &
                                         ~ChunkPool_CHUNKADDRESSMASK));
  const volatile struct ChunkPool_chunkMetadata* chunkMetadata =
      ChunkPool_chunkToChunkMetadata (containingChunk);
  void* chunk = NULL;

  if (ChunkPool_NONFIRSTCHUNK == chunkMetadata->previous) {
    assert (ChunkPool_NONFIRSTCHUNK == chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata->spanInfo.spanStart);
  } else {
    assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata);
  }

#if ASSERT
  chunkMetadata = ChunkPool_chunkToChunkMetadata(chunk);
  assert(ChunkPool_ALLOCATED == chunkMetadata->previous);
  assert(ChunkPool_ALLOCATED == chunkMetadata->next);
#endif

  LOG(LM_CHUNK_POOL, LL_DEBUGMORE, "pointer %p in chunk %p", object, chunk);

  return chunk;
}

size_t ChunkPool_allocated(void) {
  return ChunkPool_bytesAllocated;
}

size_t ChunkPool_maxAllocated(void) {
  return ChunkPool_maxBytesAllocated;
}

size_t ChunkPool_size(void) {
  return ChunkPool_config.maxSize;
}

/**
 * This function serializes against nothing
 */
size_t ChunkPool_chunkSize(void* chunk) {
  assert (chunk >= ChunkPool_poolStart);
  assert (chunk < ChunkPool_poolEnd);
  assert(((void*)(((size_t)(chunk)) & ~ChunkPool_CHUNKADDRESSMASK)) == chunk);

  const volatile struct ChunkPool_chunkMetadata* chunkMetadata =
      ChunkPool_chunkToChunkMetadata(chunk);


  assert(ChunkPool_ALLOCATED == chunkMetadata->previous);
  assert(ChunkPool_ALLOCATED == chunkMetadata->next);

  return (chunkMetadata->spanInfo.numChunksInSpan * ChunkPool_MINIMUMCHUNKSIZE);
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

bool ChunkPool_performFree(void* chunk) {
  LOG(LM_CHUNK_POOL, LL_DEBUG, "Freeing chunk %p", chunk);

  struct ChunkPool_chunkMetadata* spanStart =
      ChunkPool_chunkToChunkMetadata (chunk);

  /* should be the first chunk in a span! */
  assert (ChunkPool_ALLOCATED == spanStart->previous);
  assert (ChunkPool_ALLOCATED == spanStart->next);

  ChunkPool_bytesAllocated -= spanStart->spanInfo.numChunksInSpan *
                              ChunkPool_MINIMUMCHUNKSIZE;
  assert(ChunkPool_bytesAllocated <= ChunkPool_config.maxSize);
  assert((ChunkPool_bytesAllocated % ChunkPool_MINIMUMCHUNKSIZE) == 0);

  /*
   * RAM_NOTE: Implement coalescing here! Can be done by initializing first and
   * last metadatas for every chunk
   */
#if 0
  /* Get the previous and next spans for coalescing */
  bool coalesced = FALSE;
  struct ChunkPool_chunkMetadata* previousSpanStart = spanStart - 1;
  previousSpanStart = (previousSpanStart >= ChunkPool_chunkMetadatas) ?
                      (previousSpanStart) : (NULL);

  struct ChunkPool_chunkMetadata* nextSpanStart =
      spanStart + (spanStart->spanInfo.numChunksInSpan);
  nextSpanStart = (nextSpanStart < ChunkPool_chunkMetadatasEnd) ?
                  (nextSpanStart) : (NULL);

  if ((NULL != previousSpanStart) &&
      (ChunkPool_NONFIRSTCHUNK == previousSpanStart->previous)) {
    /* go to first chunk in previous span if necessary */
    assert (ChunkPool_NONFIRSTCHUNK == previousSpanStart->next);

    previousSpanStart = previousSpanStart->spanInfo.spanStart;
  }

  if ((NULL != previousSpanStart) &&
      (ChunkPool_ALLOCATED != previousSpanStart->previous)) {
    assert(ChunkPool_ALLOCATED != previousSpanStart->next);
    assert(ChunkPool_NONFIRSTCHUNK != previousSpanStart->previous);
    assert(ChunkPool_NONFIRSTCHUNK != previousSpanStart->next);

    /* coalesceable! remove from list and join */
    ChunkPool_removeFromFreeList (previousSpanStart);
    previousSpanStart->spanInfo.numChunksInSpan +=
        spanStart->spanInfo.numChunksInSpan;

    /*
     * set spanStart to previousSpanStart so that I don't have to branch on
     * for coalescing nextSpanStart
     */
    spanStart = previousSpanStart;

    coalesced = TRUE;
  }

  if ((NULL != nextSpanStart) &&
      (ChunkPool_ALLOCATED != nextSpanStart->previous)) {
    assert(ChunkPool_ALLOCATED != nextSpanStart->next);
    assert(ChunkPool_NONFIRSTCHUNK != nextSpanStart->previous);
    assert(ChunkPool_NONFIRSTCHUNK != nextSpanStart->next);

    /* coalesceable! remove from list and join */
    ChunkPool_removeFromFreeList (nextSpanStart);
    spanStart->spanInfo.numChunksInSpan += nextSpanStart->spanInfo.numChunksInSpan;

    coalesced = TRUE;
  }

  /*
   * At this point, spanStart points to the possible coalesced span. Reinsert it
   * into the free list!
   */
  if (coalesced) {
    /* I coalesced, so I need to reinitialize the span */
    ChunkPool_initializeSpan(spanStart);
  }
#endif
  ChunkPool_insertIntoFreeList(spanStart);

  VALGRIND_MEMPOOL_FREE(&ChunkPool_poolStart, chunk);

  return TRUE;
}

void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->previous);
  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);

  struct ChunkPool_chunkMetadata** list =
      &(ChunkPool_freeLists[ChunkPool_findFreeListIndexForNumChunks (
          chunkMetadata->spanInfo.numChunksInSpan)]);

#if ASSERT
  {
    /* make sure chunkMetadata is in the free list! */
    bool found = FALSE;
    for (const struct ChunkPool_chunkMetadata* cursor = *list;
         NULL != cursor;
         cursor = cursor->next) {
      if (chunkMetadata == cursor) {
        found = TRUE;
      }
    }
    assert(found);
  }
#endif

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

#if ASSERT
  {
    /* make sure chunkMetadata is NOT in the free list! */
    bool found = FALSE;
    for (const struct ChunkPool_chunkMetadata* cursor = *list;
         NULL != cursor;
         cursor = cursor->next) {
      if (chunkMetadata == cursor) {
        found = TRUE;
      }
    }
    assert(!found);
  }
#endif
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
    const volatile struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  size_t chunkIndex = chunkMetadata - ChunkPool_chunkMetadatas;
  assert(chunkIndex <= SIZE_MAX / ChunkPool_MINIMUMCHUNKSIZE);
  assert(chunkIndex < ((((size_t)(ChunkPool_chunkMetadatasEnd)) -
                        ((size_t)(ChunkPool_chunkMetadatas))) /
                       sizeof(struct ChunkPool_chunkMetadata)));

  void* chunk = ((void*)(((size_t)(ChunkPool_poolStart)) +
                         (chunkIndex * ChunkPool_MINIMUMCHUNKSIZE)));
  assert (0 == (((size_t)(chunk)) & ChunkPool_CHUNKADDRESSMASK));
  assert(chunk >= ChunkPool_poolStart);
  assert(chunk < ChunkPool_poolEnd);

  return chunk;
}

struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    const void* chunk) {
  assert (ChunkPool_initialized);

  assert (0 == (((size_t)(chunk)) & ChunkPool_CHUNKADDRESSMASK));
  assert (chunk >= ChunkPool_poolStart);
  assert (chunk < ChunkPool_poolEnd);

  size_t chunkIndex =
      (((const size_t)(chunk)) - ((const size_t)(ChunkPool_poolStart))) /
      ChunkPool_MINIMUMCHUNKSIZE;
  assert(chunkIndex < ((((size_t)(ChunkPool_chunkMetadatasEnd)) -
                        ((size_t)(ChunkPool_chunkMetadatas))) /
                       sizeof(struct ChunkPool_chunkMetadata)));

  return (ChunkPool_chunkMetadatas + chunkIndex);
}
