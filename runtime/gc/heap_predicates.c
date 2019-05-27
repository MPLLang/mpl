/* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool isPointerInGlobalHeap(GC_state s, pointer p) {
  HM_chunkList list = HM_getLevelHead(HM_getChunkOf(p));
  assert(list != NULL);
  bool result = (0 == HM_getChunkListLevel(list));

// #if ASSERT
#if 0
  if (result) {
    /* Make sure that the list which contains this pointer is one of the global
     * heap lists. */
    bool foundIt = FALSE;
    for (int i = 0; i < s->numberOfProcs; i++) {
      if (list == s->procStates[i].globalHeap) {
        foundIt = TRUE;
        break;
      }
    }
    assert(foundIt);
  } else {
    assert(list->containingHH != NULL);
  }
#endif

  return result;
}

bool isObjptrInGlobalHeap(GC_state s, objptr op) {
  return isPointerInGlobalHeap(s, objptrToPointer(op, NULL));
}
