#include "concurrent-stack.h"

#include <pthread.h>

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define MAX(A,B) (((A) > (B)) ? (A) : (B))

static const size_t MINIMUM_CAPACITY = 10;

void CC_stack_init(CC_stack* stack, size_t capacity){
    stack->size = 0;
    stack->capacity = MAX(capacity, MINIMUM_CAPACITY);
    stack->storage = malloc(sizeof(void*) * stack->capacity);
    pthread_mutex_init(&stack->mutex, NULL);
    // pthread_cond_init(&stack->empty_condition_variable, NULL);
    // pthread_cond_init(&stack->full_condition_variable, NULL);
}

// assumes that the mutex is held
bool increaseCapacity(CC_stack* stack, int factor){
    assert(stack->storage!=NULL);
    void** new_store = (void**) realloc(stack->storage,
                                    sizeof(void*) * (factor*stack->capacity));

    if(new_store!=NULL){
        stack->storage = new_store;
        stack->capacity*=factor;
        return true;
    }
    else
        return false;
}

// return false if the push failed. true if push succeeds
bool CC_stack_push(CC_stack* stack, void* datum){
    pthread_mutex_lock(&stack->mutex);

    if (stack->size == stack->capacity){
        bool capIncrease = increaseCapacity(stack, 2);
        if(!capIncrease){
            assert(0);
        }
    }

    stack->storage[stack->size++] = datum;
    pthread_mutex_unlock(&stack->mutex);
    return true;
}

// two threads can't pop the same stack
void* CC_stack_pop(CC_stack* stack){
    void* ret;
    pthread_mutex_lock(&stack->mutex);

    if (stack->size == 0){
        LOG(LM_HH_COLLECTION, LL_FORCE, "assertion that two different threads don't pop failed");
        assert(0);
    }
    ret = stack->storage[stack->size - 1];
    stack->size--;

    pthread_mutex_unlock(&stack->mutex);

    return ret;
}

void* CC_stack_top(CC_stack* stack){
    void* ret;
    pthread_mutex_lock(&stack->mutex);

    if (stack->size == 0){
        LOG(LM_HH_COLLECTION, LL_FORCE, "top query demands a size query");
        assert(0);
    }

    ret = stack->storage[stack->size - 1];

    // pthread_cond_signal(&stack->full_condition_variable);
    pthread_mutex_unlock(&stack->mutex);

    return ret;
}

size_t CC_stack_size(CC_stack* stack){
    size_t size;

    pthread_mutex_lock(&stack->mutex);
    size = stack->size;
    pthread_mutex_unlock(&stack->mutex);

    return size;
}

size_t CC_stack_capacity(CC_stack* stack){
    return stack->capacity;
}

void CC_stack_free(CC_stack* stack){
    free(stack->storage);
    pthread_mutex_destroy(&stack->mutex);
}

void CC_stack_clear(CC_stack* stack){
    pthread_mutex_lock(&stack->mutex);
    stack->size = 0;
    pthread_mutex_unlock(&stack->mutex);
}


void forEachObjptrinStack(GC_state s,
                          CC_stack* stack,
                          ForeachObjptrFunction f,
                          void* rawArgs){
    if(stack==NULL)
        return;
    // size can only increase while iterating.
    // CC_stack_size function requires mutex, therefore it is more efficient to have two loops
    // the inner loop does not query size and once it's done, we can re-check size
    size_t i = 0, size = CC_stack_size(stack);
    while(i!=size){

        while(i!=size){
            callIfIsObjptr(s, f, (objptr*) stack->storage[i++], rawArgs);
        }

        #if ASSERT
            size_t size_new = CC_stack_size(stack);
            assert(size_new>=size);
            size = size_new;
        #else
            size = CC_stack_size(stack);
        #endif
    }
}

#endif

