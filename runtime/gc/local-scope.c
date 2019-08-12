/* Copyright (C) 2019 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

// uint64_t lockLocalScope(GC_state s) {
//   uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);

//   uint64_t val = atomicLoadU64(top);
//   size_t terminateCheckCounter = 0;
//   while (true) {
//     uint64_t oldval = val;
//     val = __sync_val_compare_and_swap(top, val, val | TOPBIT64);
//     if (oldval == val)
//       break;
//     GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
//   }

//   return val;
// }

// void unlockLocalScope(GC_state s, uint64_t oldval) {
//   uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
//   uint64_t tag = UNPACK_TAG(oldval);
//   uint64_t idx = UNPACK_IDX(oldval);
//   tag = tag+1;
//   atomicStoreU64(top, PACK_TAGIDX(tag, idx));
//   return;
// }

// uint32_t pollCurrentLocalScope(GC_state s) {
//   if (s->wsQueueTop == BOGUS_OBJPTR) {
//     return 0;
//   }
//   uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
//   return UNPACK_IDX(*top);
// }

bool tryClaimLocalScope(GC_state s) {
  uint64_t *top = (uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t *bot = (uint32_t*)objptrToPointer(s->wsQueueBot, NULL);

  uint32_t oldBot = atomicLoadU32(bot);
  if (oldBot == 0) {
    return FALSE;
  }

  uint32_t newBot = oldBot-1;
  atomicStoreU32(bot, newBot);
  __sync_synchronize();

  uint64_t oldTop = atomicLoadU64(top);
  uint32_t idx = UNPACK_IDX(oldTop);

  if (newBot > idx) {
    return TRUE;
  }

  if (newBot < idx) {
    atomicStoreU32(bot, idx);
    __sync_synchronize();
    return FALSE;
  }

  /* In this case, newBot == idx, so we are racing with a concurrent steal, but
   * we haven't lost the race yet. Despite this, let's be conservative and leave
   * that one element available for stealing. This has the nice property that
   * the GC will never interfere with the deque top-idx tag. */
  atomicStoreU32(bot, oldBot);
  __sync_synchronize();
  return FALSE;
}

void releaseLocalScope(GC_state s, uint32_t originalBot) {
  uint32_t *bot = (uint32_t*)objptrToPointer(s->wsQueueBot, NULL);
  atomicStoreU32(bot, originalBot);
  __sync_synchronize();
}

uint32_t pollCurrentLocalScope(GC_state s) {
  uint32_t *bot = (uint32_t*)objptrToPointer(s->wsQueueBot, NULL);
  return *bot;
}
