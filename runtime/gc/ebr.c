/* Copyright (C) 2021 Sam Westrick
 * Copyright (C) 2022 Jatin Arora
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined(MLTON_GC_INTERNAL_FUNCS))

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
#define PACK(epoch, qbit) ((((size_t)(epoch)) << 1) | ((qbit)&1))
#define UNPACK_EPOCH(announcement) ((announcement) >> 1)
#define UNPACK_QBIT(announcement) ((announcement)&1)
#define SET_Q_TRUE(announcement) ((announcement) | (size_t)1)
#define SET_Q_FALSE(announcement) ((announcement) & (~(size_t)1))

#define ANNOUNCEMENT_PADDING 16

static inline size_t getAnnouncement(EBR_shared ebr, uint32_t pid)
{
  return ebr->announce[ANNOUNCEMENT_PADDING * pid];
}

static inline void setAnnouncement(EBR_shared ebr, uint32_t pid, size_t ann)
{
  ebr->announce[ANNOUNCEMENT_PADDING * pid] = ann;
}

void EBR_enterQuiescentState(GC_state s, EBR_shared ebr)
{
  uint32_t mypid = s->procNumber;
  setAnnouncement(ebr, mypid, SET_Q_TRUE(getAnnouncement(ebr, mypid)));
}

static void rotateAndReclaim(GC_state s, EBR_shared ebr)
{
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
         p += sizeof(void *))
    {
      ebr->freeFun(s, *(void **)p);
      // HM_UnionFindNode hufp = *(HM_UnionFindNode *)p;
      // assert(hufp->payload != NULL);
      // freeFixedSize(getHHAllocator(s), hufp->payload);
      // freeFixedSize(getUFAllocator(s), hufp);
    }
  }

  HM_freeChunksInListWithInfo(s, limboBag, NULL, BLOCK_FOR_EBR);
  HM_initChunkList(limboBag); // clear it out
}

EBR_shared EBR_new(GC_state s, EBR_freeRetiredObj freeFun)
{
  EBR_shared ebr = malloc(sizeof(struct EBR_shared));

  ebr->epoch = 0;
  ebr->announce =
      malloc(s->numberOfProcs * ANNOUNCEMENT_PADDING * sizeof(size_t));
  ebr->local =
      malloc(s->numberOfProcs * sizeof(struct EBR_local));
  ebr->freeFun = freeFun;

  for (uint32_t i = 0; i < s->numberOfProcs; i++)
  {
    // Everyone starts by announcing epoch = 0 and is non-quiescent
    setAnnouncement(ebr, i, PACK(0, 0));
    ebr->local[i].limboIdx = 0;
    ebr->local[i].checkNext = 0;
    for (int j = 0; j < 3; j++)
      HM_initChunkList(&(ebr->local[i].limboBags[j]));
  }
  return ebr;
}

void EBR_leaveQuiescentState(GC_state s, EBR_shared ebr)
{
  uint32_t mypid = s->procNumber;
  uint32_t numProcs = s->numberOfProcs;

  size_t globalEpoch = ebr->epoch;
  size_t myann = getAnnouncement(ebr, mypid);
  size_t myEpoch = UNPACK_EPOCH(myann);
  assert(globalEpoch >= myEpoch);

  if (myEpoch != globalEpoch)
  {
    ebr->local[mypid].checkNext = 0;
    /** Advance into the current epoch. To do so, we need to clear the limbo
     * bag of the epoch we're moving into.
     */
    rotateAndReclaim(s, ebr);
  }
  // write a function which takes a number of reads of otherann as an argument
  uint32_t otherpid = (ebr->local[mypid].checkNext) % numProcs;
  size_t otherann = getAnnouncement(ebr, otherpid);
  if (UNPACK_EPOCH(otherann) == globalEpoch || UNPACK_QBIT(otherann))
  {
    uint32_t c = ++ebr->local[mypid].checkNext;
    if (c >= numProcs)
    {
      __sync_val_compare_and_swap(&(ebr->epoch), globalEpoch, globalEpoch + 1);
    }
  }

  setAnnouncement(ebr, mypid, PACK(globalEpoch, 0));
}

void EBR_retire(GC_state s, EBR_shared ebr, void *ptr)
{
  uint32_t mypid = s->procNumber;
  int limboIdx = ebr->local[mypid].limboIdx;
  HM_chunkList limboBag = &(ebr->local[mypid].limboBags[limboIdx]);
  HM_chunk chunk = HM_getChunkListLastChunk(limboBag);

  // fast path: bump frontier in chunk

  if (NULL != chunk &&
      HM_getChunkSizePastFrontier(chunk) >= sizeof(void *))
  {
    pointer p = HM_getChunkFrontier(chunk);
    *(void **) p = ptr;
    HM_updateChunkFrontierInList(limboBag, chunk, p + sizeof(void *));
    return;
  }

  // slow path: allocate new chunk

  chunk = HM_allocateChunkWithPurpose(
    limboBag,
    sizeof(void *),
    BLOCK_FOR_EBR);

  assert(NULL != chunk &&
         HM_getChunkSizePastFrontier(chunk) >= sizeof(void *));

  pointer p = HM_getChunkFrontier(chunk);
  *(void **) p = ptr;
  HM_updateChunkFrontierInList(limboBag, chunk, p + sizeof(void *));
  return;
}

#endif // MLTON_GC_INTERNAL_FUNCS
