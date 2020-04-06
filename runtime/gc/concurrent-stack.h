#ifndef CONCURRENT_STACK_H
#define CONCURRENT_STACK_H


typedef struct concurrent_stack
{

    /*******************************************
    The number of elements stored in this stack.
    *******************************************/
    size_t size;

    /**************************************************
    The maximum number of elements this stack can hold.
    **************************************************/
    size_t capacity;

    /**********************************************
    The actual array holding the data of the stack.
    **********************************************/
    void** storage;

    /************************************************
    The mutual exclusion lock for updating the stack.
    ************************************************/
    pthread_mutex_t mutex;

    /*****************************
    Guards against an empty stack.
    *****************************/
    pthread_cond_t  empty_condition_variable;

    /***************************
    Guards against a full stack.
    ***************************/
    pthread_cond_t  full_condition_variable;
}
concurrent_stack;

/*****************************************
Initializes a new, empty concurrent stack.
*****************************************/
void concurrent_stack_init(concurrent_stack* stack, size_t capacity);

/****************************************
Pushes a datum onto the top of the stack.
****************************************/
void concurrent_stack_push(concurrent_stack* stack, void* datum);

/******************************************
Returns, but does not remove the top datum.
******************************************/
void* concurrent_stack_top(concurrent_stack* stack);

/****************************************
Removes the topmost datum from the stack.
****************************************/
void* concurrent_stack_pop(concurrent_stack* stack);

/*******************************************
Returns the number of elements in the stack.
*******************************************/
size_t concurrent_stack_size(concurrent_stack* stack);

/*********************************
Returns the capacity of the stack.
*********************************/
size_t concurrent_stack_capacity(concurrent_stack* stack);

/***********************************
Releases all resources of the stack.
***********************************/
void concurrent_stack_free(concurrent_stack* stack);
void concurrent_stack_clear(concurrent_stack* stack);

#endif /* CONCURRENT_STACK_H */
