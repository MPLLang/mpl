/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** Helpers for packing/unpacking announcements. DEBRA packs epochs with a
  * "quiescent" bit, the idea being that processors should set the bit during
  * quiescent periods (between operations) and have it unset otherwise (i.e.
  * during an operation). Being precise about quiescent periods in this way
  * is helpful for reclamation, because in order to advance the epoch, all we
  * need to know is that every processor has been in a quiescent period since
  * the beginning of the last epoch.
  *
  * But note that updating the quiescent bits is only efficient if we can
  * amortize the cost of the setting/unsetting the bit with other nearby
  * operations. If we assumed that the typical state for each processor
  * is quiescent and then paid for non-quiescent periods, this would
  * be WAY too expensive. In our case, processors are USUALLY NON-QUIESCENT,
  * due to depth queries at the write-barrier.
  *
  * So
  */
#define PACK(epoch, qbit) ((((size_t)(epoch)) << 1) | ((qbit) & 1))
#define UNPACK_EPOCH(announcement) ((announcement) >> 1)
#define UNPACK_QBIT(announcement) ((announcement) & 1)
#define SET_Q_TRUE(announcement) ((announcement) | (size_t)1)
#define SET_Q_FALSE(announcement) ((announcement) & (~(size_t)1))

#define ANNOUNCEMENT_PADDING 16

static inline size_t getAnnouncement(GC_state s, uint32_t pid) {
  return s->hhEBR->announce[ANNOUNCEMENT_PADDING*pid];
}

static inline void setAnnouncement(GC_state s, uint32_t pid, size_t ann) {
  s->hhEBR->announce[ANNOUNCEMENT_PADDING*pid] = ann;
}

void HH_EBR_enterQuiescentState(GC_state s) {
  uint32_t mypid = s->procNumber;
  setAnnouncement(s, mypid, SET_Q_TRUE(getAnnouncement(s, mypid)));
}

static void rotateAndReclaim(GC_state s) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;

  int limboIdx = (ebr->local[mypid].limboIdx + 1) % 3;
  ebr->local[mypid].limboIdx = limboIdx;
  HM_chunkList limboBag = &(ebr->local[mypid].limboBags[limboIdx]);

  // Free all HH records in the limbo bag.
  for (HM_chunk chunk = HM_getChunkListFirstChunk(limboBag);
       NULL != chunk;
       chunk = chunk->nextChunk)
  {
    for (pointer p = HM_getChunkStart(chunk);
         p < HM_getChunkFrontier(chunk);
         p += sizeof(HM_UnionFindNode *))
    {
      freeFixedSize(getUFAllocator(s), *(HM_UnionFindNode*)p);
    }
  }

  HM_appendChunkList(getFreeListSmall(s), limboBag);
  HM_initChunkList(limboBag); // clear it out
}


void HH_EBR_init(GC_state s) {
  HH_EBR_shared ebr = malloc(sizeof(struct HH_EBR_shared));
  s->hhEBR = ebr;

  ebr->epoch = 0;
  ebr->announce =
    malloc(s->numberOfProcs * ANNOUNCEMENT_PADDING * sizeof(size_t));
  ebr->local =
    malloc(s->numberOfProcs * sizeof(struct HH_EBR_local));

  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    // Everyone starts by announcing epoch = 0 and is non-quiescent
    setAnnouncement(s, i, PACK(0,0));
    ebr->local[i].limboIdx = 0;
    ebr->local[i].checkNext = 0;
    for (int j = 0; j < 3; j++)
      HM_initChunkList(&(ebr->local[i].limboBags[j]));
  }
}


void HH_EBR_leaveQuiescentState(GC_state s) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  uint32_t numProcs = s->numberOfProcs;

  size_t globalEpoch = ebr->epoch;
  size_t myann = getAnnouncement(s, mypid);
  size_t myEpoch = UNPACK_EPOCH(myann);
  assert(globalEpoch >= myEpoch);

  if (myEpoch != globalEpoch) {
    ebr->local[mypid].checkNext = 0;
    /** Advance into the current epoch. To do so, we need to clear the limbo
      * bag of the epoch we're moving into.
      */
    rotateAndReclaim(s);
  }

  uint32_t otherpid = (ebr->local[mypid].checkNext) % numProcs;
  size_t otherann = getAnnouncement(s, otherpid);
  if ( UNPACK_EPOCH(otherann) == globalEpoch || UNPACK_QBIT(otherann) ) {
    uint32_t c = ++ebr->local[mypid].checkNext;
    if (c >= numProcs) {
      __sync_val_compare_and_swap(&(ebr->epoch), globalEpoch, globalEpoch+1);
    }
  }

  setAnnouncement(s, mypid, PACK(globalEpoch, 0));
}


void HH_EBR_retire(GC_state s, HM_UnionFindNode hhuf) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  int limboIdx = ebr->local[mypid].limboIdx;
  HM_chunkList limboBag = &(ebr->local[mypid].limboBags[limboIdx]);
  HM_chunk chunk = HM_getChunkListLastChunk(limboBag);

  // fast path: bump frontier in chunk

  if (NULL != chunk &&
      HM_getChunkSizePastFrontier(chunk) >= sizeof(HM_UnionFindNode *))
  {
    pointer p = HM_getChunkFrontier(chunk);
    *(HM_UnionFindNode *)p = hhuf;
    HM_updateChunkFrontier(chunk, p + sizeof(HM_UnionFindNode *));
    return;
  }

  // slow path: allocate new chunk

  chunk = HM_allocateChunk(limboBag, sizeof(HM_UnionFindNode *));

  assert(NULL != chunk &&
    HM_getChunkSizePastFrontier(chunk) >= sizeof(HM_UnionFindNode *));

  pointer p = HM_getChunkFrontier(chunk);
  *(HM_UnionFindNode *)p = hhuf;
  HM_updateChunkFrontier(chunk, p + sizeof(HM_UnionFindNode *));
  return;
}



#endif // MLTON_GC_INTERNAL_FUNCS
