#include "forgotten-set.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define MAX(A,B) (((A) > (B)) ? (A) : (B))

static const size_t MINIMUM_CAPACITY = 10;

void CC_forgotten_init(CC_forgotten* fSet, size_t capacity){
    fSet->size = 0;
    fSet->capacity = MAX(capacity, MINIMUM_CAPACITY);
    fSet->elements = malloc(sizeof(void*) * fSet->capacity);
    pthread_mutex_init(&fSet->mutex, NULL);
}

/** assumes that the lock is held */
static bool increaseCapacity(CC_forgotten* fSet, int factor){
    assert(fSet->elements!=NULL);
    void** new_store = (void**) realloc(fSet->elements,
                                    sizeof(void*) * (factor*fSet->capacity));
    if(new_store!=NULL){
        fSet->elements = new_store;
        fSet->capacity*=factor;
        return true;
    }
    else {
        return false;
    }
}

bool CC_forgotten_push(CC_forgotten* fSet, void* datum){
    pthread_mutex_lock(&fSet->mutex);

    if (fSet->size == fSet->capacity){
        bool capIncrease = increaseCapacity(fSet, 2);
        if(!capIncrease){
            DIE("Ran out of space for CC_forgotten!\n");
            assert(0);
        }
    }

    fSet->elements[fSet->size++] = datum;
    pthread_mutex_unlock(&fSet->mutex);
    return true;
}

size_t CC_forgotten_size(CC_forgotten* fSet){
    size_t size;
    // Not sure if we need mutex here
    pthread_mutex_lock(&fSet->mutex);
    size = fSet->size;
    pthread_mutex_unlock(&fSet->mutex);

    return size;
}

size_t CC_forgotten_capacity(CC_forgotten* fSet){
    return fSet->capacity;
}

void CC_forgotten_free(CC_forgotten* fSet){
    free(fSet->elements);
    pthread_mutex_destroy(&fSet->mutex);
}

void CC_forgotten_clear(CC_forgotten* fSet){
    pthread_mutex_lock(&fSet->mutex);
    fSet->size = 0;
    pthread_mutex_unlock(&fSet->mutex);
}

/** The function assumes that the fSet's size can only increase.
  * Of course we can clear the fSet, but not while this code is operating
  * on it.
  */
void forEachForgottenPointer(GC_state s,
                          CC_forgotten* fSet,
                          GC_foreachObjptrFun f,
                          void* rawArgs){
    if(fSet==NULL)
        return;

    struct GC_foreachObjptrClosure fObjptrClosure = {.fun = f, .env = rawArgs};

    size_t i = 0, size = CC_forgotten_size(fSet);
    while(i!=size){
        while(i!=size){
            objptr* opp = (objptr*)(&(fSet->elements[i]));
            callIfIsObjptr(s, &fObjptrClosure, opp);
            i++;
        }
        #if ASSERT
            size_t size_new = CC_forgotten_size(fSet);
            assert(size_new>=size);
            size = size_new;
        #else
            size = CC_forgotten_size(fSet);
        #endif
    }
}

#endif

