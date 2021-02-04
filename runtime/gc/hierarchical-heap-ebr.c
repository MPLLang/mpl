/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** Some helper stuff, if we use DEBRA later instead of regular EBR.
  * DEBRA uses an additional "quiescent bit", where each process
  * not only announces its current epoch, but also whether or not it is
  * currently quiescent. This optimization is useful for identifying
  * grace periods, but is not essential.
  */
#if 0
#define PACK(epoch, qbit) ((((size_t)(epoch)) << 1) | ((qbit) & 1))
#define UNPACK_EPOCH(announcement) ((announcement) >> 1)
#define UNPACK_QBIT(announcement) ((announcement) & 1)

#define SET_Q_TRUE(announcement) ((announcement) | (size_t)1)
#define SET_Q_FALSE(announcement) ((announcement) & (~(size_t)1))
#endif


void HH_EBR_init(GC_state s) {
  HH_EBR_shared ebr = malloc(sizeof(struct HH_EBR_shared));

  ebr->epoch = 0;
  ebr->announce = malloc(s->numberOfProcs * sizeof(size_t));
  ebr->local = malloc(s->numberOfProcs * sizeof(struct HH_EBR_local));

  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    // Everyone starts by announcing epoch = 0
    ebr->announce[i] = 0;
    for (int j = 0; j < 3; j++)
      HM_initChunkList(&(ebr->local[i].limboBags[j]));
  }

  s->hhEBR = ebr;
}


void HH_EBR_enterQuiescentState(__attribute__((unused)) GC_state s) {
  /** SAM_NOTE: no-op for now. Not needed for normal EBR. But later if we
    * use DEBRA, we need to set the quiescent bit.
    */
  return;
}


void HH_EBR_leaveQuiescentState(GC_state s) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  uint32_t numProcs = s->numberOfProcs;

  size_t globalEpoch = ebr->epoch;
  size_t myAnnouncement = ebr->announce[mypid];
  assert(globalEpoch == myAnnouncement || globalEpoch == myAnnouncement+1);

  if (myAnnouncement != globalEpoch) {
    /** Advance into the current epoch. To do so, we need to clear the limbo
      * bag of the epoch we're moving into.
      */

    int limboIdx = globalEpoch % 3;
    HM_chunkList limboBag = &(ebr->local[mypid].limboBags[limboIdx]);

    // Free all HH records in the limbo bag.
    for (HM_chunk chunk = HM_getChunkListFirstChunk(limboBag);
         NULL != chunk;
         chunk = chunk->nextChunk)
    {
      for (pointer p = HM_getChunkStart(chunk);
           p < HM_getChunkFrontier(chunk);
           p += sizeof(HM_HierarchicalHeap *))
      {
        freeFixedSize(getHHAllocator(s), *(HM_HierarchicalHeap*)p);
      }
    }

    HM_appendChunkList(getFreeListSmall(s), limboBag);
    HM_initChunkList(limboBag); // clear it out

    ebr->announce[mypid] = globalEpoch;
  }

  // Check: has everyone entered the current global epoch?
  bool everyoneInSameEpoch = TRUE;
  for (uint32_t otherpid = 0; otherpid < numProcs; otherpid++) {
    if (ebr->announce[otherpid] != globalEpoch) {
      everyoneInSameEpoch = FALSE;
      break;
    }
  }

  if (everyoneInSameEpoch && (ebr->epoch == globalEpoch)) {
    __sync_val_compare_and_swap(&(ebr->epoch), globalEpoch, globalEpoch+1);
  }

}


void HH_EBR_retire(GC_state s, HM_HierarchicalHeap hh) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  size_t epoch = ebr->announce[mypid];
  int limboIdx = epoch % 3;
  HM_chunkList limboBag = &(ebr->local[mypid].limboBags[limboIdx]);
  HM_chunk chunk = HM_getChunkListLastChunk(limboBag);

  // fast path: bump frontier in chunk

  if (NULL != chunk &&
      HM_getChunkSizePastFrontier(chunk) >= sizeof(HM_HierarchicalHeap *))
  {
    pointer p = chunk->frontier;
    *(HM_HierarchicalHeap *)p = hh;
    chunk->frontier += sizeof(HM_HierarchicalHeap *);
    assert(chunk->limit >= chunk->frontier);
    return;
  }

  // slow path: allocate new chunk

  chunk = HM_allocateChunk(limboBag, sizeof(HM_HierarchicalHeap *));

  assert(NULL != chunk &&
    HM_getChunkSizePastFrontier(chunk) >= sizeof(HM_HierarchicalHeap *));

  pointer p = chunk->frontier;
  *(HM_HierarchicalHeap *)p = hh;
  chunk->frontier += sizeof(HM_HierarchicalHeap *);
  assert(chunk->limit >= chunk->frontier);
  return;
}



#endif // MLTON_GC_INTERNAL_FUNCS
