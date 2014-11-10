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
 * the span the the chunk starts, specifically the number of chunks in the span
 * (<tt>numChunksInSpan</tt>). When not in a list, this struct contains a
 * pointer to the first chunk of the span it belongs to
 * (<tt>firstChunkMetadataInSpan</tt>). The address of the struct is converted
 * to an index and specifies which chunk this struct describes. This design
 * allows for easy chunk to metadata and metadata to chunk transformations using
 * simple pointer arithmetic.
 */
struct ChunkPool_chunkMetadata {
  union {
    size_t numChunksInSpan;
    struct ChunkPool_chunkMetadata* firstChunkMetadataInSpan;
  };
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
 * Inserts the chunkMetadata object into the list
 *
 * @param chunkMetadata the object to insert
 * @param list the list to insert into
 */
static void ChunkPool_insertIntoFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** list);

/**
 * Removes the chunkMetadata object from the list
 *
 * @param chunkMetadata the object to remove
 * @param list the list to remove from
 */
static void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** list);

/**
 * Initialize a span
 *
 * @param firstChunkMetadataInSpan the first chunk in the span, which contains
 * the metadata for the span
 * @param numChunks Number of chunks in the span.
 */
static void ChunkPool_initializeSpan (
    struct ChunkPool_chunkMetadata* firstChunkMetadataInSpan, size_t numChunks);

/**
 * Splits a given span if possible, inserts the residual span into the list, and
 * returns the first chunk in the split span.
 *
 * @param chunkMetadata The span to split
 * @param containingFreeList The list containing the span
 * @param numChunks The number of chunks to split to
 *
 * @return The span that is shrunk to <tt>numChunks</tt>
 */
static struct ChunkPool_chunkMetadata*
ChunkPool_maybeSplitSpanAndReturnAllocatedChunkMetadata (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** containingFreeList,
    size_t numChunks);

/**
 * Map chunkMetadata object to the chunk pointer
 *
 * @param chunkMetdata the object to map
 *
 * @return The corresponding chunk
 */
static void* ChunkPool_chunkMetadataToChunk (
    struct ChunkPool_chunkMetadata* chunkMetadata);

/**
 * Map chunk pointer to the chunkMetadata object
 *
 * @param chunk the chunk to map
 *
 * @return The corresponding chunkMetadata
 */
static struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    void* chunk);

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
static bool ChunkPool_initialized = false;

static pthread_rwlock_t ChunkPool_lock;

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
    diee(__FILE__ ":%d: mmap() failed with errno %d", __LINE__, errno);
  }

  /* setup pool variables */
  ChunkPool_poolStart = region;
  ChunkPool_poolEnd = region + adjustedPoolSize;

  /* setup metadatas variable */
  unsigned int numChunks = adjustedPoolSize / ChunkPool_MINIMUMCHUNKSIZE;
  ChunkPool_chunkMetadatas = malloc_safe (numChunks *
                                          sizeof(*ChunkPool_chunkMetadatas));
  ChunkPool_initializeSpan (ChunkPool_chunkMetadatas, numChunks);

  /* Add the giant chunk into the free list */
  unsigned int freeListIndex =
      ChunkPool_findFreeListIndexForNumChunks (numChunks);
  ChunkPool_insertIntoFreeList (ChunkPool_chunkMetadatas,
                                &(ChunkPool_freeLists[freeListIndex]));

  /* Initialize the lock */
  int initRetVal;
  if (0 != (initRetVal = pthread_rwlock_init(&ChunkPool_lock, NULL))) {
    errno = initRetVal;
    diee(__FILE__ ":%d: pthread_rwlock_init() failed with errno %d", __LINE__, errno);
  }

  ChunkPool_initialized = true;
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
      if (chunksRequested <= cursor->numChunksInSpan) {
        /* split the span if possible and return the chunk */
        void* chunk = ChunkPool_chunkMetadataToChunk (
            ChunkPool_maybeSplitSpanAndReturnAllocatedChunkMetadata (
                cursor,
                &(ChunkPool_freeLists[freeListIndex]),
                chunksRequested));
        pthread_rwlock_unlock_safe(&ChunkPool_lock);
        return chunk;
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
  pthread_rwlock_rdlock_safe(&ChunkPool_lock);
  struct ChunkPool_chunkMetadata* chunkMetadata =
      ChunkPool_chunkToChunkMetadata (containingChunk);
  void* chunk = NULL;
  if (ChunkPool_NONFIRSTCHUNK == chunkMetadata->previous) {
    assert (ChunkPool_NONFIRSTCHUNK == chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata->firstChunkMetadataInSpan);
  } else {
    assert  (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);
    chunk = ChunkPool_chunkMetadataToChunk(chunkMetadata);
  }

  pthread_rwlock_unlock_safe(&ChunkPool_lock);
  return chunk;
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
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** list) {
  assert (ChunkPool_initialized);

  chunkMetadata->previous = NULL;
  chunkMetadata->next = *list;
  (*list)->previous = chunkMetadata;
  *list = chunkMetadata;
}

void ChunkPool_removeFromFreeList (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** list) {
  assert (ChunkPool_initialized);

  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->previous);
  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);

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

void ChunkPool_initializeSpan (
    struct ChunkPool_chunkMetadata* firstChunkMetadataInSpan,
    size_t numChunks) {
  assert (ChunkPool_initialized);

  firstChunkMetadataInSpan->numChunksInSpan = numChunks;
  firstChunkMetadataInSpan->previous = NULL;
  firstChunkMetadataInSpan->next = NULL;
  for (unsigned int chunkOffset = 1;
       chunkOffset < numChunks;
       chunkOffset++) {
    struct ChunkPool_chunkMetadata* chunkMetadata =
        firstChunkMetadataInSpan + chunkOffset;
    chunkMetadata->firstChunkMetadataInSpan = firstChunkMetadataInSpan;
    chunkMetadata->previous = ChunkPool_NONFIRSTCHUNK;
    chunkMetadata->next = ChunkPool_NONFIRSTCHUNK;
  }
}

struct ChunkPool_chunkMetadata*
ChunkPool_maybeSplitSpanAndReturnAllocatedChunkMetadata (
    struct ChunkPool_chunkMetadata* chunkMetadata,
    struct ChunkPool_chunkMetadata** containingFreeList,
    size_t splitChunks) {
  assert (ChunkPool_initialized);

  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->previous);
  assert (ChunkPool_NONFIRSTCHUNK != chunkMetadata->next);

  /* first remove the span from the list */
  ChunkPool_removeFromFreeList (chunkMetadata, containingFreeList);

  assert (chunkMetadata->numChunksInSpan >= splitChunks);
  if (chunkMetadata->numChunksInSpan == splitChunks) {
    /* no need to split, all done! */
  } else {
    /* can split, so should split */
    struct ChunkPool_chunkMetadata* firstChunkInNewSpan =
        chunkMetadata + splitChunks;
    ChunkPool_initializeSpan (firstChunkInNewSpan,
                              chunkMetadata->numChunksInSpan - splitChunks);
    unsigned int freeListIndex = ChunkPool_findFreeListIndexForNumChunks (
        firstChunkInNewSpan->numChunksInSpan);
    ChunkPool_insertIntoFreeList (firstChunkInNewSpan,
                                  &(ChunkPool_freeLists[freeListIndex]));

    chunkMetadata->numChunksInSpan = splitChunks;
  }

  /* return chunkMetadata, which is now split */
  return chunkMetadata;
}

void* ChunkPool_chunkMetadataToChunk (
    struct ChunkPool_chunkMetadata* chunkMetadata) {
  assert (ChunkPool_initialized);

  unsigned int chunkIndex =
      (chunkMetadata - ChunkPool_chunkMetadatas) / sizeof(*chunkMetadata);

  assert (ChunkPool_poolStart + (chunkIndex * ChunkPool_MINIMUMCHUNKSIZE) <
          ChunkPool_poolEnd);
  return (ChunkPool_poolStart + (chunkIndex * ChunkPool_MINIMUMCHUNKSIZE));
}

struct ChunkPool_chunkMetadata* ChunkPool_chunkToChunkMetadata (
    void* chunk) {
  assert (ChunkPool_initialized);

  assert (0 == (((size_t)(chunk)) & ChunkPool_CHUNKADDRESSMASK));
  assert (chunk >= ChunkPool_poolStart);
  assert (chunk < ChunkPool_poolEnd);

  unsigned int chunkIndex =
      (chunk - ChunkPool_poolStart) / ChunkPool_MINIMUMCHUNKSIZE;
  return (ChunkPool_chunkMetadatas + chunkIndex);
}
