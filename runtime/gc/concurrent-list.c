/* A public list is read without its mutex: by writers (the last chunk and
 * its frontier; its limit is fixed before the chunk is published) and by
 * walkers (HM_foreachPublic: the first chunk, each chunk's nextChunk,
 * retireChunk and frontier). Those reads are atomic loads (acquire for the
 * pointers), and the stores they can see are atomic (release for the
 * pointers, which publish a zeroed chunk), also under the mutex. The entries
 * themselves: see CC_storeInConcListWithPurpose. */

void CC_initConcList(CC_concList concList) {
  concList->firstChunk = NULL;
  concList->lastChunk = NULL;
  pthread_mutex_init(&(concList->mutex), NULL);
}


void allocateChunkInConcList(
  GC_state s,
  CC_concList concList,
  size_t objSize,
  HM_chunk lastChunk,
  enum BlockPurpose purpose)
{
  // pthread_mutex_lock(&concList->mutex);
  if(__atomic_load_n(&concList->lastChunk, __ATOMIC_ACQUIRE) != lastChunk) {
    // pthread_mutex_unlock(&concList->mutex);
    return;
  }

  HM_chunk chunk = HM_getFreeChunkWithPurpose(s, objSize, purpose);

  if (NULL == chunk)
  {
    DIE("Out of memory. Unable to allocate chunk of size %zu.",
        objSize);
  }

  assert(chunk->frontier == HM_getChunkStart(chunk));
  assert(chunk->mightContainMultipleObjects);
  assert((size_t)(chunk->limit - chunk->frontier) >= objSize);
  assert(chunk != NULL);

  chunk->prevChunk = lastChunk;
  // if (lastChunk != NULL)
  // {
  //   lastChunk->nextChunk = chunk;
  // }
  // concList->lastChunk = chunk;

  memset((void *)HM_getChunkStart(chunk), '\0', HM_getChunkLimit(chunk) - HM_getChunkStart(chunk));

  /* The release stores below publish the zeroed chunk to the writers and
   * walkers that read these pointers without the mutex. */
  bool success = false;
  if(__atomic_load_n(&concList->lastChunk, __ATOMIC_ACQUIRE) == lastChunk) {
    pthread_mutex_lock(&concList->mutex);
    if (concList->lastChunk == lastChunk) {
      if (concList->firstChunk == NULL) {
        __atomic_store_n(&concList->firstChunk, chunk, __ATOMIC_RELEASE);
      }
      if(lastChunk != NULL) {
        __atomic_store_n(&lastChunk->nextChunk, chunk, __ATOMIC_RELEASE);
      }
      __atomic_store_n(&concList->lastChunk, chunk, __ATOMIC_RELEASE);
      success = true;
    }
    pthread_mutex_unlock(&concList->mutex);
  }
  if (!success) {
    HM_freeChunkWithInfo(s, chunk, NULL, purpose);
  }

  // if (!__sync_bool_compare_and_swap(&(concList->lastChunk), lastChunk, chunk)) {
  //   HM_freeChunk(s, chunk);
  // } else if (lastChunk != NULL) {
  //   lastChunk->nextChunk = chunk;
  // }
}


pointer CC_storeInConcListWithPurpose(GC_state s, CC_concList concList, void* p, size_t objSize, enum BlockPurpose purpose){
  assert(concList != NULL);
  // pthread_mutex_lock(&concList->mutex);
  while(TRUE) {
    HM_chunk chunk = __atomic_load_n(&concList->lastChunk, __ATOMIC_ACQUIRE);
    if (NULL == chunk) {
      allocateChunkInConcList(s, concList, objSize, chunk, purpose);
      continue;
    }
    else {
      pointer frontier = __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED);
      size_t sizePast = (size_t) (chunk->limit - frontier);
      if (sizePast < objSize) {
        allocateChunkInConcList(s, concList, objSize, chunk, purpose);
        continue;
      }

      pointer new_frontier = frontier + objSize;
      bool success = __sync_bool_compare_and_swap(&(chunk->frontier), frontier, new_frontier);
      if (success)
      {
        /* Walkers take an entry once its last word is non-zero (for a
         * remembered entry, its object) and read the rest after it: write
         * the rest first, then the last word with a release store. */
        assert(objSize >= sizeof(uint64_t) && objSize % sizeof(uint64_t) == 0);
        size_t head = objSize - sizeof(uint64_t);
        uint64_t last;
        memcpy(frontier, p, head);
        memcpy(&last, (char *)p + head, sizeof(uint64_t));
        __atomic_store_n((uint64_t *)(frontier + head), last, __ATOMIC_RELEASE);
        // pthread_mutex_unlock(&concList->mutex);
        return frontier;
      }
    }
  }
  // pthread_mutex_unlock(&concList->mutex);
  DIE("should never come here");
  return NULL;
}


// void CC_foreachObjInList(CC_concList concList, size_t objSize, HM_foreachObjClosure f) {

//   struct HM_chunkList _chunkList;
//   HM_chunkList chunkList =  &(_chunkList);
//   chunkList->firstChunk = concList->firstChunk;
//   pthread_mutex_lock(&concList->mutex);
//   chunkList->lastChunk = concList->lastChunk;
//   concList->firstChunk = NULL;
//   concList->lastChunk = NULL;
//   pthread_mutex_unlock(&concList->mutex);
//   HM_foreachObjInChunkList(chunkList, objSize, f);
// }

// void CC_foreachRemInConc(GC_state s, CC_concList concList, struct HM_foreachDownptrClosure* f) {
//   struct HM_chunkList _store;
//   HM_chunkList store = &(_store);
//   HM_initChunkList(store);

//   while (TRUE) {
//     HM_chunk firstChunk = concList->firstChunk;
//     if (firstChunk == NULL) {
//       break;
//     }

//     pthread_mutex_lock(&concList->mutex);
//     HM_chunk lastChunk = concList->lastChunk;
//     concList->firstChunk = NULL;
//     concList->lastChunk = NULL;
//     pthread_mutex_unlock(&concList->mutex);

//     assert(firstChunk != NULL);
//     assert(lastChunk != NULL);
//     HM_chunk chunk = firstChunk;
//     while (chunk != NULL) {
//       pointer p = HM_getChunkStart(chunk);
//       pointer frontier = HM_getChunkFrontier(chunk);
//       while (p < frontier)
//       {
//         f->fun(s, (HM_remembered)p, f->env);
//         p += sizeof(struct HM_remembered);
//       }
//       chunk = chunk->nextChunk;
//     }

//     if (store->firstChunk != NULL) {
//       store->lastChunk->nextChunk = firstChunk;
//       firstChunk->prevChunk = store->lastChunk;
//       store->lastChunk = lastChunk;
//     }
//     else {
//       store->firstChunk = firstChunk;
//       store->lastChunk = lastChunk;
//     }
//   }

//   /*add the chunks back to the list*/
//   pthread_mutex_lock(&concList->mutex);
//   if (concList->firstChunk != NULL) {
//     concList->firstChunk->prevChunk = store->lastChunk;
//     store->lastChunk->nextChunk = concList->firstChunk;
//     concList->firstChunk = store->firstChunk;
//   }
//   else {
//     concList->firstChunk = store->firstChunk;
//     concList->lastChunk = store->lastChunk;
//   }
//   pthread_mutex_unlock(&concList->mutex);

// }

void CC_popAsChunkList(CC_concList concList, HM_chunkList chunkList) {
  pthread_mutex_lock(&concList->mutex);
  chunkList->firstChunk = concList->firstChunk;
  __atomic_store_n(&concList->firstChunk, NULL, __ATOMIC_RELEASE);
  chunkList->lastChunk = concList->lastChunk;
  __atomic_store_n(&concList->lastChunk, NULL, __ATOMIC_RELEASE);
  pthread_mutex_unlock(&concList->mutex);
}

HM_chunk CC_getLastChunk (CC_concList concList) {
  HM_chunk c;
  pthread_mutex_lock(&concList->mutex);
  c = concList->lastChunk;
  pthread_mutex_unlock(&concList->mutex);
  return c;
}

void CC_appendConcList(CC_concList concList1, CC_concList concList2) {

  HM_chunk firstChunk, lastChunk;
  pthread_mutex_lock(&concList2->mutex);
  firstChunk = concList2->firstChunk;
  lastChunk = concList2->lastChunk;
  __atomic_store_n(&concList2->firstChunk, NULL, __ATOMIC_RELEASE);
  __atomic_store_n(&concList2->lastChunk, NULL, __ATOMIC_RELEASE);
  pthread_mutex_unlock(&concList2->mutex);

  if (firstChunk == NULL || lastChunk == NULL) {
    return;
  }


  pthread_mutex_lock(&concList1->mutex);
  if (concList1->lastChunk == NULL) {
    __atomic_store_n(&concList1->firstChunk, firstChunk, __ATOMIC_RELEASE);
    __atomic_store_n(&concList1->lastChunk, lastChunk, __ATOMIC_RELEASE);
  }
  else {
    __atomic_store_n(&concList1->lastChunk->retireChunk, true, __ATOMIC_RELAXED);
    __atomic_store_n(&concList1->lastChunk->nextChunk, firstChunk, __ATOMIC_RELEASE);
    firstChunk->prevChunk = concList1->lastChunk;
    __atomic_store_n(&concList1->lastChunk, lastChunk, __ATOMIC_RELEASE);
  }
  pthread_mutex_unlock(&concList1->mutex);
}

void CC_freeChunksInConcListWithInfo(GC_state s, CC_concList concList, void *info, enum BlockPurpose purpose) {
  struct HM_chunkList _chunkList;
  CC_popAsChunkList(concList, &(_chunkList));
  HM_freeChunksInListWithInfo(s, &(_chunkList), info, purpose);
}
