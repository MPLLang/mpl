#ifndef CC_STACK_H
#define CC_STACK_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_stack {
    size_t size;
    size_t capacity;
    // The actual array holding the data of the stack.
    void** storage;
    pthread_mutex_t mutex;
}
CC_stack;

#else

struct CC_stack;
typedef struct CC_stack CC_stack;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void CC_stack_init(CC_stack* stack, size_t capacity);

bool CC_stack_push(CC_stack* stack, void* datum);

void* CC_stack_top(CC_stack* stack);

void* CC_stack_pop(CC_stack* stack);

size_t CC_stack_size(CC_stack* stack);

size_t CC_stack_capacity(CC_stack* stack);

void CC_stack_free(CC_stack* stack);
void CC_stack_clear(CC_stack* stack);

void forEachObjptrinStack(GC_state s,
                          CC_stack* stack,
                          GC_foreachObjptrFun f,
                          void* rawArgs);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CC_STACK_H */
