#include "forgotten-set.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define MAX(A,B) (((A) > (B)) ? (A) : (B))

static const size_t MINIMUM_CAPACITY = 2;


void CC_local_init(CC_localSet* lSet, const size_t capacity){
    lSet->size = 0;
    lSet->capacity = MAX(capacity, MINIMUM_CAPACITY);

    lSet->elements = malloc(sizeof(void*) * capacity);

    pthread_mutex_init(&lSet->mutex, NULL);
}


void CC_forgotten_init(CC_forgotten* fSet, const size_t capacity) {
  GC_state s = pthread_getspecific(gcstate_key);

  uint32_t numProcs = s->numberOfProcs;
  fSet->localSets = (CC_localSet*) malloc(sizeof(struct CC_localSet) * s->numberOfProcs);

  for(uint32_t p = 0; p < numProcs; p++) {
    CC_local_init(&(fSet->localSets[p]), capacity);
  }
}

/** assumes that the lock is held */
static bool increaseCapacity(CC_localSet* lSet, int factor){
    assert(lSet->elements!=NULL);
    void** new_store = (void**) realloc(lSet->elements,
                                    sizeof(void*) * (factor*lSet->capacity));
    if (new_store == NULL) return false;

    lSet->elements = new_store;
    lSet->capacity*=factor;
    return true;
}

static bool CC_local_push(CC_localSet* lSet, void* datum){

    pthread_mutex_lock(&lSet->mutex);

    if (lSet->size == lSet->capacity){
        bool capIncrease = increaseCapacity(lSet, 2);
        if(!capIncrease){
            DIE("Ran out of space for CC_forgotten!\n");
            assert(0);
        }
    }

    lSet->elements[lSet->size++] = datum;
    pthread_mutex_unlock(&lSet->mutex);
    return true;
}

bool CC_forgotten_push(CC_forgotten* fSet, void* datum){
    GC_state s = pthread_getspecific(gcstate_key);
    return CC_local_push(&fSet->localSets[s->procNumber], datum);
}

static size_t CC_local_size(CC_localSet* lSet){
    size_t size;
    pthread_mutex_lock(&lSet->mutex);
    size = lSet->size;
    pthread_mutex_unlock(&lSet->mutex);

    return size;
}

static void CC_local_free(CC_localSet* lSet){
    free(lSet->elements);
    pthread_mutex_destroy(&lSet->mutex);
}

void CC_forgotten_free(CC_forgotten* fSet){
  GC_state s = pthread_getspecific(gcstate_key);
  uint32_t numProcs = s->numberOfProcs;

  for(uint32_t p = 0; p < numProcs; p++) {
    CC_local_free(&(fSet->localSets[p]));
  }
}

static void CC_local_clear(CC_localSet* lSet){
    pthread_mutex_lock(&lSet->mutex);
    lSet->size = 0;
    pthread_mutex_unlock(&lSet->mutex);
}

void CC_forgotten_clear(CC_forgotten* fSet){
  GC_state s = pthread_getspecific(gcstate_key);
  uint32_t numProcs = s->numberOfProcs;

  for(uint32_t p = 0; p < numProcs; p++) {
    CC_local_clear(&(fSet->localSets[p]));
  }
}

static void copyElements(CC_localSet* lSet, void** buffer, size_t i, size_t numCopy) {
    pthread_mutex_lock(&lSet->mutex);
    assert(i + numCopy <= lSet->size);
    memcpy(buffer, lSet->elements + i, numCopy * sizeof(void*));

    #if ASSERT
    for(size_t j = 0; j<numCopy; j++) {
        assert(buffer[j] == lSet->elements[i+j]);
    }
    #endif
    pthread_mutex_unlock(&lSet->mutex);
}

/** The function assumes that the fSet's size can only increase.
  * Of course we can clear the fSet, but not while this code is operating
  * on it.
  */
static inline int forwardFromIdx(GC_state s,
                          CC_localSet* lSet,
                          size_t idx,
                          GC_foreachObjptrFun f,
                          void* rawArgs){
    if(lSet==NULL)
        return 0;

    const size_t BUFFER_SIZE = 100;
    void* localBuffer[BUFFER_SIZE];

    struct GC_foreachObjptrClosure fObjptrClosure = {.fun = f, .env = rawArgs};

    size_t i = idx, size = CC_local_size(lSet);

    while(i!=size){

        size_t numCopy = min(size-i, BUFFER_SIZE);

        copyElements(lSet, localBuffer, i, numCopy);

        for(size_t j=0; j<numCopy; j++) {
            objptr* opp = (objptr*)(&(localBuffer[j]));
            callIfIsObjptr(s, &fObjptrClosure, opp);
        }

        i+=numCopy;

        #if ASSERT
            size_t size_new = CC_local_size(lSet);
            assert(size_new>=size);
            size = size_new;
        #else
            size = CC_local_size(lSet);
        #endif
    }

    return size;
}


void forEachForgottenPointer(GC_state s,
                          CC_forgotten* fSet,
                          GC_foreachObjptrFun f,
                          void* rawArgs){

  size_t sizes[s->numberOfProcs];
  for(uint32_t p =0; p < s->numberOfProcs; p++) sizes[p] = 0;

  bool clean = true;
  do {
    for(uint32_t p = 0; p<s->numberOfProcs; p++) {
      size_t newSize = forwardFromIdx(s, &(fSet->localSets[p]), sizes[p], f, rawArgs);

      assert(newSize >= sizes[p]);
      if(newSize > sizes[p]) clean = false;
      sizes[p] = newSize;
    }
  }while(!clean);
}

#endif

