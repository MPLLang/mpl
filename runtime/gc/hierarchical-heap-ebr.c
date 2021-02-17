/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define PACK(epoch, qbit) ((((size_t)(epoch)) << 1) | ((qbit) & 1))
#define UNPACK_EPOCH(announcement) ((announcement) >> 1)
#define UNPACK_QBIT(announcement) ((announcement) & 1)

#define SET_Q_TRUE(announcement) ((announcement) | (size_t)1)
#define SET_Q_FALSE(announcement) ((announcement) & (~(size_t)1))


static inline bool getQBit(GC_state s, uint32_t pid) {
  size_t ann = s->hhEBR->announce[pid];
  return UNPACK_QBIT(ann);
}

static inline void setQBitTrue(GC_state s, uint32_t pid) {
  size_t ann = s->hhEBR->announce[pid];
  s->hhEBR->announce[pid] = SET_Q_TRUE(ann);
}

static inline void setQBitFalse(GC_state s, uint32_t pid) {
  size_t ann = s->hhEBR->announce[pid];
  s->hhEBR->announce[pid] = SET_Q_FALSE(ann);
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
         p += sizeof(HM_HierarchicalHeap *))
    {
      freeFixedSize(getHHAllocator(s), *(HM_HierarchicalHeap*)p);
    }
  }

  HM_appendChunkList(getFreeListSmall(s), limboBag);
  HM_initChunkList(limboBag); // clear it out
}


void HH_EBR_init(GC_state s) {
  HH_EBR_shared ebr = malloc(sizeof(struct HH_EBR_shared));

  ebr->epoch = 0;
  ebr->announce = malloc(s->numberOfProcs * sizeof(size_t));
  ebr->local = malloc(s->numberOfProcs * sizeof(struct HH_EBR_local));

  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    // Everyone starts by announcing epoch = 0, and starts in quiescent state
    ebr->announce[i] = PACK(0, 1);
    ebr->local[i].limboIdx = 0;
    for (int j = 0; j < 3; j++)
      HM_initChunkList(&(ebr->local[i].limboBags[j]));
  }

  s->hhEBR = ebr;
}


void HH_EBR_enterQuiescentState(GC_state s) {
  setQBitTrue(s, s->procNumber);
}

void HH_EBR_fastLeaveQuiescentState(GC_state s) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  size_t globalEpoch = ebr->epoch;
  size_t myEpoch = UNPACK_EPOCH(ebr->announce[mypid]);
  assert(globalEpoch >= myEpoch);
  if (myEpoch != globalEpoch) {
    /** Advance into the current epoch. To do so, we need to clear the limbo
      * bag of the epoch we're moving into.
      */
    rotateAndReclaim(s);
  }
  ebr->announce[mypid] = PACK(globalEpoch, 0);
}

void HH_EBR_leaveQuiescentState(GC_state s) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  uint32_t numProcs = s->numberOfProcs;

  size_t globalEpoch = ebr->epoch;
  size_t myEpoch = UNPACK_EPOCH(ebr->announce[mypid]);
  assert(globalEpoch >= myEpoch);

  if (myEpoch != globalEpoch) {
    /** Advance into the current epoch. To do so, we need to clear the limbo
      * bag of the epoch we're moving into.
      */
    rotateAndReclaim(s);
  }

  // Check: has everyone entered the current global epoch?
  bool everyoneInSameEpochOrQuiescent = TRUE;
  for (uint32_t otherpid = 0; otherpid < numProcs; otherpid++) {
    if ( !(UNPACK_EPOCH(ebr->announce[otherpid]) == globalEpoch
           || getQBit(s, otherpid)) )
    {
      everyoneInSameEpochOrQuiescent = FALSE;
      break;
    }
  }

  if (everyoneInSameEpochOrQuiescent && (ebr->epoch == globalEpoch)) {
    __sync_val_compare_and_swap(&(ebr->epoch), globalEpoch, globalEpoch+1);
  }

  ebr->announce[mypid] = PACK(globalEpoch, 0);
}


void HH_EBR_retire(GC_state s, HM_HierarchicalHeap hh) {
  HH_EBR_shared ebr = s->hhEBR;
  uint32_t mypid = s->procNumber;
  int limboIdx = ebr->local[mypid].limboIdx;
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
