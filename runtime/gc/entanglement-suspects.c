static inline bool suspicious_header(GC_header h) {
  return (1 == ((h & SUSPECT_MASK) >> SUSPECT_SHIFT));
}

static inline bool mark_suspect(objptr op)
{
  pointer p = objptrToPointer(op, NULL);
  GC_header header = __sync_fetch_and_or(getHeaderp(p), SUSPECT_MASK);
  assert (1 == (header & GC_VALID_HEADER_MASK));

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
    __attribute__((unused)) GC_state s,
    objptr *opp,
    __attribute__((unused)) void *rawArgs)
{
  objptr op = *opp;
  pointer p = objptrToPointer(op, NULL);
  assert(isObjptr(op) && is_suspect(p));
  __sync_fetch_and_add(getHeaderp(p), ~(SUSPECT_MASK));
}

bool ES_contains(__attribute__((unused)) HM_chunkList es, objptr op)
{
  return is_suspect(op);
}

void ES_add(__attribute__((unused)) GC_state s, HM_chunkList es, objptr op)
{

  if (!mark_suspect(op))
  {
    /* if op is already in es, skip*/
    return;
  }
  s->cumulativeStatistics->numSuspectsMarked++;
  HM_storeInchunkList(es, &op, sizeof(objptr));
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

void ES_clear(GC_state s, HM_chunkList es)
{
  struct GC_foreachObjptrClosure fObjptrClosure =
    {.fun = clear_suspect, .env = NULL};
  int numSuspects = ES_foreachSuspect(s, es, &fObjptrClosure);
  s->cumulativeStatistics->numSuspectsCleared+=numSuspects;

  HM_freeChunksInList(s, es);
}
