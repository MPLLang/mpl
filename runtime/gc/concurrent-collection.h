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

#include "forgotten-set.h"
#include "hierarchical-heap.h"
#include "objptr.h"
#include "deferred-promote.h"
// #include "logger.h"


#if (defined (MLTON_GC_INTERNAL_TYPES))

// Struct to pass around args. repList is the new chunklist.
typedef struct ConcurrentCollectArgs {
  HM_chunkList origList;
  HM_chunkList repList;
  void* toHead;
  void* fromHead;
  size_t bytesSaved;
} ConcurrentCollectArgs;


enum CCState{
  CC_UNREG,
  CC_REG,
  CC_COLLECTING
};

typedef struct ConcurrentPackage {
//  It is possible that the collection turned off and the stack isn't empty
//  This is a result of the non-atomicity in the write barrier implementation
//  from checking of isCollecting to addition into the stack
  CC_forgotten* fSet;
  //children roots
  objptr snapLeft;
  objptr snapRight;
  objptr snapTemp;
  // bool isCollecting;
  bool shouldCollect;
  enum CCState ccstate;
  objptr stack;

  /** For deciding when to collect. Could be cleaned up.
    */
  size_t bytesAllocatedSinceLastCollection;
  size_t bytesSurvivedLastCollection;

  /** To avoid races with other processor adding to the remset (writebarrier or
    * promotions).
    */
  // struct HM_chunkList remSet;

} * ConcurrentPackage;

#else

struct ConcurrentPackage;
typedef struct ConcurrentPackage *ConcurrentPackage;

#endif


#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** operations on the forgotten set */
void CC_addToFSet(ConcurrentPackage cp, pointer p);
void CC_initFSet(ConcurrentPackage cp);
void CC_clearFSet(ConcurrentPackage cp);
void CC_freeFSet(ConcurrentPackage cp);

/** These two functions perform a concurrent mark and sweep style collection.
  * The algorithm is approximate---it considers liveness at the level of chunks.
  * If even 1 object is live in a chunk, the whole chunk is preserved (without splitting).
  */

/** CC_collectAtRoot collects expects the heap to be collected as an argument*/
void CC_collectAtRoot(pointer threadp, pointer hhp);

/** CC_collectAtPublicLevel traverses the heap hierarchy
  *  and finds the appropriate heap to collect
  */
void CC_collectAtPublicLevel(GC_state s, GC_thread thread, uint32_t depth);

bool CC_isPointerMarked (pointer p);
void printObjPtrFunction(GC_state s, objptr* opp, void* rawArgs);
#endif

#endif
