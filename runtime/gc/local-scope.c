/* Copyright (C) 2019 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

uint64_t lockLocalScope(GC_state s) {
  uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);

  uint64_t val = atomicLoadU64(top);
  size_t terminateCheckCounter = 0;
  while (true) {
    uint64_t oldval = val;
    val = __sync_val_compare_and_swap(top, val, val | TOPBIT64);
    if (oldval == val)
      break;
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
  }

  return val;
}

void unlockLocalScope(GC_state s, uint64_t oldval) {
  uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint64_t tag = UNPACK_TAG(oldval);
  uint64_t idx = UNPACK_IDX(oldval);
  tag = tag+1;
  atomicStoreU64(top, PACK_TAGIDX(tag, idx));
  return;
}

uint32_t pollCurrentLocalScope(GC_state s) {
  if (s->wsQueueTop == BOGUS_OBJPTR) {
    return 0;
  }
  uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  return UNPACK_IDX(*top);
}
