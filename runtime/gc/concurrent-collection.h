/**
 * @file hierarchical-heap-collection.h
 *
 * @author Jatin Arora
 *
 * @brief
 * Definition of the concurrent collection interface
 */

#ifndef CONCURRENT_COLLECTION_H_
#define CONCURRENT_COLLECTION_H_

#include "concurrent-stack.h"
#include "hierarchical-heap.h"
#include "objptr.h"
// #include "logger.h"


#if (defined (MLTON_GC_INTERNAL_FUNCS))
#define LL_Log LL_FORCE
// Struct to pass around args. repList is the new chunklist.
typedef struct ConcurrentCollectArgs {
	HM_chunkList origList;
	HM_chunkList repList;
	void* toHead;
	void* fromHead;
} ConcurrentCollectArgs;

typedef struct ConcurrentPackage {
//  It is possible that the collection turned off and the stack isn't empty
//	This is a result of the non-atomicity in the write barrier implementation
//	from checking of isCollecting to addition into the stack
	CC_stack* rootList;
	//children roots
	objptr snapLeft;
	objptr snapRight;
	bool isCollecting;
	bool shouldCollect;
	objptr stack;
	size_t bytesAllocatedSinceLastCollection;
	size_t bytesSurvivedLastCollection;
	struct HM_chunkList remSet;
} * ConcurrentPackage;

// Assume complete access in this function
// This function constructs a HM_chunkList of reachable chunks without copying them
// Then it adds the remaining chunks to the free list.
// The collections here proceeds here at the chunk level of granularity. i.e if one obj
// in the chunk is live then the whole chunk is. However, tracing is at the granularity of objects.
// Objects in chunks that are preserved may point to chunks that are not. But such objects aren't
// reachable.
void CC_collectWithRoots(GC_state s, struct HM_HierarchicalHeap * targetHH, GC_thread thread);

void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth);
void CC_addToStack(ConcurrentPackage cp, pointer p);

bool CC_isPointerMarked (pointer p);
void printObjPtrFunction(GC_state s, objptr* opp, void* rawArgs);
#endif

#endif