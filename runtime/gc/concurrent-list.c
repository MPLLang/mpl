void CC_initConcList(CC_concList concList) {
  concList->firstChunk = NULL;
  concList->lastChunk = NULL;
  pthread_mutex_init(&(concList->mutex), NULL);
}


void allocateChunkInConcList(
  CC_concList concList,
  size_t objSize,
  HM_chunk lastChunk,
  enum BlockPurpose purpose)
{
  GC_state s = pthread_getspecific(gcstate_key);

  // pthread_mutex_lock(&concList->mutex);
  if(concList->lastChunk != lastChunk) {
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

  bool success = false;
  if(concList->lastChunk == lastChunk) {
    pthread_mutex_lock(&concList->mutex);
    if (concList->lastChunk == lastChunk) {
      if (concList->firstChunk == NULL) {
        concList->firstChunk = chunk;
      }
      if(lastChunk != NULL) {
        lastChunk->nextChunk = chunk;
      }
      concList->lastChunk = chunk;
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


pointer CC_storeInConcListWithPurpose(CC_concList concList, void* p, size_t objSize, enum BlockPurpose purpose){
  assert(concList != NULL);
  // pthread_mutex_lock(&concList->mutex);
  while(TRUE) {
    HM_chunk chunk = concList->lastChunk;
    if (NULL == chunk) {
      allocateChunkInConcList(concList, objSize, chunk, purpose);
      continue;
    }
    else {
      pointer frontier = HM_getChunkFrontier(chunk);
      size_t sizePast = (size_t) (chunk->limit - frontier);
      if (sizePast < objSize) {
        allocateChunkInConcList(concList, objSize, chunk, purpose);
        continue;
      }

      pointer new_frontier = frontier + objSize;
      bool success = __sync_bool_compare_and_swap(&(chunk->frontier), frontier, new_frontier);
      if (success)
      {
        memcpy(frontier, p, objSize);
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
  concList->firstChunk = NULL;
  chunkList->lastChunk = concList->lastChunk;
  concList->lastChunk = NULL;
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
  concList2->firstChunk = NULL;
  concList2->lastChunk = NULL;
  pthread_mutex_unlock(&concList2->mutex);

  if (firstChunk == NULL || lastChunk == NULL) {
    return;
  }


  pthread_mutex_lock(&concList1->mutex);
  if (concList1->lastChunk == NULL) {
    concList1->firstChunk = firstChunk;
    concList1->lastChunk = lastChunk;
  }
  else {
    concList1->lastChunk->nextChunk = firstChunk;
    concList1->lastChunk->retireChunk = true;
    firstChunk->prevChunk = concList1->lastChunk;
    concList1->lastChunk = lastChunk;
  }
  pthread_mutex_unlock(&concList1->mutex);
}

void CC_freeChunksInConcListWithInfo(GC_state s, CC_concList concList, void *info) {
  struct HM_chunkList _chunkList;
  CC_popAsChunkList(concList, &(_chunkList));
  HM_freeChunksInListWithInfo(s, &(_chunkList), info, BLOCK_FOR_UNKNOWN_PURPOSE);
}