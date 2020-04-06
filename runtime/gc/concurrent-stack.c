#include "concurrent-stack.h"
#include <pthread.h>

#define MAX(A,B) (((A) > (B)) ? (A) : (B))

static const size_t MINIMUM_CAPACITY = 10;

void concurrent_stack_init(concurrent_stack* stack, size_t capacity)
{
    stack->size = 0;
    stack->capacity = MAX(capacity, MINIMUM_CAPACITY);
    stack->storage = malloc(sizeof(void*) * stack->capacity);
    pthread_mutex_init(&stack->mutex, NULL);
    pthread_cond_init(&stack->empty_condition_variable, NULL);
    pthread_cond_init(&stack->full_condition_variable, NULL);
}

void concurrent_stack_push(concurrent_stack* stack, void* datum)
{
    pthread_mutex_lock(&stack->mutex);

    while (stack->size == stack->capacity)
    {
        pthread_cond_wait(&stack->full_condition_variable, &stack->mutex);
    }

    stack->storage[stack->size++] = datum;

    pthread_cond_signal(&stack->empty_condition_variable);
    pthread_mutex_unlock(&stack->mutex);
}

void* concurrent_stack_pop(concurrent_stack* stack)
{
    void* ret;
    pthread_mutex_lock(&stack->mutex);

    while (stack->size == 0)
    {
        pthread_cond_wait(&stack->empty_condition_variable, &stack->mutex);
    }
    ret = stack->storage[stack->size - 1];
    stack->size--;

    pthread_cond_signal(&stack->full_condition_variable);
    pthread_mutex_unlock(&stack->mutex);

    return ret;
}

void* concurrent_stack_top(concurrent_stack* stack)
{
    void* ret;
    pthread_mutex_lock(&stack->mutex);

    while (stack->size == 0)
    {
        pthread_cond_wait(&stack->empty_condition_variable, &stack->mutex);
    }

    ret = stack->storage[stack->size - 1];

    // pthread_cond_signal(&stack->full_condition_variable);
    pthread_mutex_unlock(&stack->mutex);

    return ret;
}

size_t concurrent_stack_size(concurrent_stack* stack)
{
    size_t size;

    pthread_mutex_lock(&stack->mutex);
    size = stack->size;
    pthread_mutex_unlock(&stack->mutex);

    return size;
}

size_t concurrent_stack_capacity(concurrent_stack* stack)
{
    return stack->capacity;
}

void concurrent_stack_free(concurrent_stack* stack)
{
    free(stack->storage);
    pthread_mutex_destroy(&stack->mutex);
    pthread_cond_destroy(&stack->empty_condition_variable);
    pthread_cond_destroy(&stack->full_condition_variable);
}

void concurrent_stack_clear(concurrent_stack* stack) {
    pthread_mutex_lock(&stack->mutex);
    stack->size = 0;
    pthread_mutex_unlock(&stack->mutex);
}
