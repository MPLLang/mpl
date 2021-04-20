#ifndef FORGOTTEN_SET_H
#define FORGOTTEN_SET_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct CC_localSet {
    size_t size;
    size_t capacity;
    void** elements;
    pthread_mutex_t mutex;
}
CC_localSet;

typedef struct CC_forgotten {
  CC_localSet* localSets;
}
CC_forgotten;



#else

struct CC_forgotten;
typedef struct CC_forgotten CC_forgotten;

#endif

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void CC_forgotten_init(CC_forgotten* fSet, size_t capacity);

bool CC_forgotten_push(CC_forgotten* fSet, void* datum);

void CC_forgotten_free(CC_forgotten* fSet);
void CC_forgotten_clear(CC_forgotten* fSet);

void forEachForgottenPointer(GC_state s,
                          CC_forgotten* fSet,
                          GC_foreachObjptrFun f,
                          void* rawArgs);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* FORGOTTEN_SET_H */
