static inline bool suspicious_header(GC_header h) {
  return (1 == ((h & SUSPECT_MASK) >> SUSPECT_SHIFT));
}

static inline bool mark_suspect(objptr op)
{
  pointer p = objptrToPointer(op, NULL);
  GC_header header = __sync_fetch_and_or(getHeaderp(p), SUSPECT_MASK);
  assert(1 == (header & GC_VALID_HEADER_MASK));
  // while (TRUE)
  // {
  //   GC_header header = getHeader(p);
  //   GC_header newHeader = header | SUSPECT_MASK;
  //   if (header == newHeader)
  //   {
  //     /*
  //        just return because the suspect bit is already set
  //      */
  //     return false;
  //   }
  //   else
  //   {
  //     /*
  //        otherwise, install the new header with the bit set. this might fail
  //        if a concurrent thread changes the header first.
  //      */
  //     if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader))
  //       return true;
  //   }
  // }
  // DIE("should be impossible to reach here");
  /*return true if this call marked the header, false if someone else did*/
  return !suspicious_header(header);
}

static inline bool is_suspect(objptr op)
{
  pointer p = objptrToPointer(op, NULL);
  GC_header h = getHeader(p);
  /* have to check first that the header is valid
   * (otherwise, there could be a forward pointer in this spot)
   */
  return (1 == (h & GC_VALID_HEADER_MASK)) && suspicious_header(h);
}

void clear_suspect(
    GC_state s,
    objptr *opp,
    objptr op,
    void *rawArgs)
{
  pointer p = objptrToPointer(op, NULL);
  ES_clearArgs eargs = (ES_clearArgs) rawArgs;

  GC_header header = getHeader(p);
  uint32_t unpinDepth = (header & UNPIN_DEPTH_MASK) >> UNPIN_DEPTH_SHIFT;

  if (pinType(header) == PIN_ANY && unpinDepth < eargs->heapDepth) {
    /* Not ready to be cleared */
    HM_HierarchicalHeap unpinHeap = HM_HH_getHeapAtDepth(s, eargs->thread, unpinDepth);
    HM_storeInChunkListWithPurpose(HM_HH_getSuspects(unpinHeap), opp, sizeof(objptr), BLOCK_FOR_SUSPECTS);
    return;
  }

  GC_header newHeader = header & ~(SUSPECT_MASK);
  if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader)) {
    /* clearing successful */
    return;
  }
  else {
    /*oops something changed in b/w, let's try at the next join*/
    HM_storeInChunkListWithPurpose(eargs->newList, opp, sizeof(objptr), BLOCK_FOR_SUSPECTS);
  }
}

bool ES_contains(__attribute__((unused)) HM_chunkList es, objptr op)
{
  return is_suspect(op);
}

bool ES_mark(__attribute__((unused)) GC_state s, objptr op) {
  return mark_suspect(op);
}

void ES_unmark(GC_state s, objptr op) {
  clear_suspect(s, &op, op, NULL);
}

void ES_add(__attribute__((unused)) GC_state s, HM_chunkList es, objptr op)
{

  if (!mark_suspect(op))
  {
    /* if op is already in es, skip*/
    return;
  }
  s->cumulativeStatistics->numSuspectsMarked++;
  HM_storeInChunkListWithPurpose(es, &op, sizeof(objptr), BLOCK_FOR_SUSPECTS);
}

int ES_foreachSuspect(
    GC_state s,
    HM_chunkList storage,
    GC_foreachObjptrClosure fObjptrClosure)
{
  if (storage == NULL)
    return 0;
  int count = 0;
  HM_chunk chunk = HM_getChunkListFirstChunk(storage);
  while (chunk != NULL)
  {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    while (p < frontier)
    {
      // objptr* opp = (objptr*)p;
      callIfIsObjptr(s, fObjptrClosure, (objptr *)p);
      p += sizeof(void *);
    }
    count += ((int)(frontier - HM_getChunkStart(chunk)))/(sizeof(void*));
    chunk = chunk->nextChunk;
  }
  return count;
}

HM_chunkList ES_append(
  __attribute__((unused)) GC_state s,
  HM_chunkList es1,
  HM_chunkList es2)
{
  assert(es1 != NULL);
  HM_appendChunkList(es1 ,es2);
  return es1;
}

void ES_move(HM_chunkList list1, HM_chunkList list2) {
  HM_appendChunkList(list1, list2);
  HM_initChunkList(list2);
}

static size_t SUSPECTS_THRESHOLD = 10000;


void ES_clear(GC_state s, HM_HierarchicalHeap hh)
{
  struct timespec startTime;
  struct timespec stopTime;

  HM_chunkList es = HM_HH_getSuspects(hh);
  uint32_t heapDepth = HM_HH_getDepth(hh);
  struct HM_chunkList oldList = *(es);
  HM_initChunkList(HM_HH_getSuspects(hh));

  size_t numSuspects = HM_getChunkListUsedSize(&oldList) / sizeof(objptr);
  if (numSuspects >= SUSPECTS_THRESHOLD) {
    timespec_now(&startTime);
  }

  struct ES_clearArgs eargs = {
    .newList = HM_HH_getSuspects(hh),
    .heapDepth = heapDepth,
    .thread = getThreadCurrent(s)
  };
  
  struct GC_foreachObjptrClosure fObjptrClosure =
      {.fun = clear_suspect, .env = &(eargs)};
  int ns = ES_foreachSuspect(s, &oldList, &fObjptrClosure);
  assert(numSuspects == ns);
  s->cumulativeStatistics->numSuspectsCleared += numSuspects;

  HM_freeChunksInListWithInfo(s, &(oldList), NULL, BLOCK_FOR_SUSPECTS);

  if (numSuspects >= SUSPECTS_THRESHOLD) {
    timespec_now(&stopTime);
    timespec_sub(&stopTime, &startTime);
    LOG(LM_HIERARCHICAL_HEAP, LL_FORCE,
      "time to clear %zu suspects at depth %u: %d.%09d",
      numSuspects,
      HM_HH_getDepth(hh),
      stopTime.tv_sec,
      stopTime.tv_nsec
    );
  }
}
