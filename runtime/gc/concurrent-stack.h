#ifndef CC_STACK_H
#define CC_STACK_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_stack_data {
    struct HM_chunkList storage;
    bool isClosed;
    pthread_mutex_t mutex;
} CC_stack_data;

typedef struct CC_stack {
  bool allClosed;
  size_t numStacks;
  struct CC_stack_data *stacks;
} CC_stack;

#else

struct CC_stack_data;
typedef struct CC_stack_data CC_stack_data;

struct CC_stack;
typedef struct CC_stack CC_stack;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void CC_stack_init(GC_state s, CC_stack* stack);

bool CC_stack_push(GC_state s, CC_stack* stack, void* datum);

void CC_stack_free(GC_state s, CC_stack* stack);
void CC_stack_clear(GC_state s, CC_stack* stack);

/** Try to close it, to prevent further pushes. This only works if the bag is
  * empty. If non-empty, a batch of elements are removed and put into the
  * given list. Return value indicates whether or not the close was successful
  */
bool CC_stack_try_close(CC_stack* stack, HM_chunkList removed);

// Prevent further pushes
// void CC_stack_close(CC_stack* stack);

void forEachObjptrInCCStackBag(
  GC_state s,
  HM_chunkList storage,
  GC_foreachObjptrFun f,
  void* rawArgs);

// void forEachObjptrinStack(GC_state s,
//                           CC_stack* stack,
//                           GC_foreachObjptrFun f,
//                           void* rawArgs);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_STACK_H */
