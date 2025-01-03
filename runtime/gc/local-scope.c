/* Copyright (C) 2019-2025 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

bool tryClaimLocalScope(GC_state s) {
  objptr result = ABP_deque_try_pop_bot(
    s,
    s->wsQueueTop,
    s->wsQueueBot,
    s->wsQueue,
    BOGUS_OBJPTR
  );
  return (result != BOGUS_OBJPTR);
}

void releaseLocalScope(GC_state s, uint32_t originalBot) {
  uint32_t *bot = (uint32_t*)objptrToPointer(s->wsQueueBot, NULL);
  __atomic_store_n(bot, originalBot, __ATOMIC_SEQ_CST);
}

uint32_t pollCurrentLocalScope(GC_state s) {
  uint32_t *bot = (uint32_t*)objptrToPointer(s->wsQueueBot, NULL);
  return __atomic_load_n(bot, __ATOMIC_SEQ_CST);
}
