static inline bool mark_suspect(objptr op)
{
  pointer p = objptrToPointer(op, NULL);
  while (TRUE)
  {
    GC_header header = getHeader(p);
    GC_header newHeader = header | SUSPECT_MASK;
    if (header == newHeader)
    {
      /*
        just return because the suspect bit is already set
      */
      return false;
    }
    else
    {
      /*
        otherwise, install the new header with the bit set. this might fail
        if a concurrent thread changes the header first.
      */
      if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader))
        return true;
    }
  }
  DIE("should be impossible to reach here");
}

static inline bool is_suspect(objptr op)
{
  pointer p = objptrToPointer(op, NULL);
  GC_header h = getHeader(p);

  /* have to check first that the header is valid
   * (otherwise, there could be a forward pointer in this spot)
   */
  return (1 == (h & GC_VALID_HEADER_MASK)) &&
         (1 == ((h & SUSPECT_MASK) >> SUSPECT_SHIFT));
}

void clear_suspect(
    __attribute__((unused)) GC_state s,
    objptr *opp,
    __attribute__((unused)) void *rawArgs)
{
  objptr op = *opp;
  assert(isObjptr(op));
  pointer p = objptrToPointer(op, NULL);
  while (TRUE)
  {
    assert(is_suspect(op)); // no one else should clear it!
    GC_header header = getHeader(p);
    GC_header newHeader = header & (~SUSPECT_MASK);
    // the header may be updated concurrently by a CC
    if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader))
      return;
  }
  DIE("should be impossible to reach here");
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

void ES_clear(GC_state s, HM_chunkList es)
{
  struct GC_foreachObjptrClosure fObjptrClosure =
    {.fun = clear_suspect, .env = NULL};
  int numSuspects = ES_foreachSuspect(s, es, &fObjptrClosure);
  s->cumulativeStatistics->numSuspectsCleared+=numSuspects;

  HM_freeChunksInList(s, es);
}
