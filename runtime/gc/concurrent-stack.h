#ifndef CC_STACK_H
#define CC_STACK_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_stack_data {
    size_t size;
    size_t capacity;
    // The actual array holding the data of the stack.
    void** storage;
    bool isClosed;
    pthread_mutex_t mutex;
} CC_stack_data;

typedef struct CC_stack {
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

void CC_stack_init(GC_state s, CC_stack* stack, size_t capacity);

bool CC_stack_push(GC_state s, CC_stack* stack, void* datum);

void CC_stack_free(CC_stack* stack);
void CC_stack_clear(CC_stack* stack);

// Prevent further pushes
void CC_stack_close(CC_stack* stack);

void forEachObjptrinStack(GC_state s,
                          CC_stack* stack,
                          GC_foreachObjptrFun f,
                          void* rawArgs);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_STACK_H */
