/* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2009-2012 Matthew Fluet.
 * Copyright (C) 2005-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT
bool threadAndHeapOkay(GC_state s) {
  return BOGUS_OBJPTR != getThreadCurrentObjptr(s)
      && getThreadCurrent(s)->hierarchicalHeap != NULL;
}
#endif

bool isPointerInRootHeap(__attribute__((unused)) GC_state s,
                         pointer p)
{
  HM_chunkList list = HM_getLevelHead(HM_getChunkOf(p));
  assert(list != NULL);
  bool result = (0 == HM_getChunkListLevel(list));
  return result;
}

bool isObjptrInRootHeap(GC_state s, objptr op) {
  return isPointerInRootHeap(s, objptrToPointer(op, NULL));
}
