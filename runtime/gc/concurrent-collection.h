/**
 * @file hierarchical-heap-collection.h
 *
 * @author Jatin Arora
 *
 * @brief
 * Definition of the Concurrent collection interface
 */
#include "chunk.h"

#ifndef CONCURRENT_COLLECTION_H_
#define CONCURRENT_COLLECTION_H_

#if (defined (MLTON_GC_INTERNAL_FUNCS))


// Assume complete access in this function
// This function constructs a HM_chunkList of reachable chunks without copying them 
// Then it adds the remaining chunks to the free list. 
// The GC here proceeds here at the chunk level of granularity. i.e if one obj
// in the chunk is live then the whole chunk is.
void collectWithRoots(INIT ROOT SET);


// Struct to pass around args. repList is the new chunklist. 
typedef struct ConcurrentCollectArgs {
	HM_chunklist origList;
	HM_chunklist repList;
	// Can add this for faster isCandidateChunk test by checking for equality
	// Similar to concurrent-collection.c:62
	// HM_chunk cacheChunk; 
} ConcurrentCollectArgs;


#endif

#endif