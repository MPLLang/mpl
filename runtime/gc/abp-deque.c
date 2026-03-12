/* Copyright (C) 2024-2025 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */


Bool ABP_deque_push_bot(
  [[maybe_unused]] GC_state s,
  [[maybe_unused]] objptr top_op,
  objptr bot_op,
  objptr data_op,
  objptr elem_to_push_op)
{
  uint32_t* bot = (uint32_t*)objptrToPointer(bot_op, NULL);
  objptr* data = (objptr*)objptrToPointer(data_op, NULL);

  uint32_t local_bot = atomic_load_explicit(bot, memory_order_acquire);

  if (local_bot == getSequenceLength(objptrToPointer(data_op, NULL))) {
    return (Bool)FALSE;
  }

  atomic_store_explicit(data + local_bot, elem_to_push_op, memory_order_release);
  atomic_store_explicit(bot, local_bot+1, memory_order_seq_cst);

  return (Bool)TRUE;
}


/* This is a sanity check, we could leave it as an assertion but I'd rather
 * see this error explicitly even in optimized builds. */
static void
ABP_deque_check_successful_pop(objptr result, objptr fail_value) {
  if (result == fail_value) {
    DIE("Scheduler bug: invalid pop in scheduler queue");
  }
}


objptr ABP_deque_try_pop_bot(
  [[maybe_unused]] GC_state s,
  objptr top_op,
  objptr bot_op,
  objptr data_op,
  objptr fail_value)
{
  uint64_t* top = (uint64_t*)objptrToPointer(top_op, NULL);
  uint32_t* bot = (uint32_t*)objptrToPointer(bot_op, NULL);
  objptr* data = (objptr*)objptrToPointer(data_op, NULL);

  uint32_t local_bot = atomic_load_explicit(bot, memory_order_acquire);
  if (local_bot == 0) {
    return fail_value;
  }

  local_bot--;
  atomic_store_explicit(bot, local_bot, memory_order_release);
  atomic_thread_fence(memory_order_seq_cst);
  objptr elem = atomic_load_explicit(data + local_bot, memory_order_acquire);
  uint64_t local_top = atomic_load_explicit(top, memory_order_acquire);
  uint32_t local_top_idx = UNPACK_IDX(local_top);

  if (local_bot > local_top_idx) {
    ABP_deque_check_successful_pop(elem, fail_value);
    return elem;
  }

  // If local_bot <= local_top_idx, then we might be racing with a concurrent
  // pop_top operation.

  uint64_t local_top_tag = UNPACK_TAG(local_top);
  uint64_t desired_top = PACK_TAGIDX(local_top_tag+1, local_top_idx);

  if (local_bot < local_top_idx) {
    // We are racing with a concurrent pop_top, but we already lost the race.
    // Revert the bot index to match the top, to indicate an empty deque.
    atomic_store_explicit(bot, local_top_idx, memory_order_release);
    atomic_store_explicit(top, desired_top, memory_order_seq_cst);
    return fail_value;
  }
  else {
    // We are racing with a concurrent pop_top, but we haven't lost the race yet
    uint64_t found_top = __sync_val_compare_and_swap(top, local_top, desired_top);
    if (found_top == local_top) {
      // We won the race
      ABP_deque_check_successful_pop(elem, fail_value);
      return elem;
    }
    else {
      // We lost the race

      // This could be an assertion, but I'd rather see the error explicitly,
      // even in optimized builds.
      if (UNPACK_IDX(found_top) != local_bot+1 ||
          UNPACK_TAG(found_top) != local_top_tag)
      {
        DIE("Scheduler bug: corrupted top");
      }

      uint64_t new_top = PACK_TAGIDX(local_top_tag+1, local_bot+1);
      atomic_store_explicit(bot, local_bot+1, memory_order_release);
      atomic_store_explicit(top, new_top, memory_order_seq_cst);
      return fail_value;
    }
  }
}


objptr ABP_deque_try_pop_top(
  [[maybe_unused]] GC_state s,
  objptr top_op,
  objptr bot_op,
  objptr data_op,
  objptr fail_value)
{
  uint64_t* top = (uint64_t*)objptrToPointer(top_op, NULL);
  uint32_t* bot = (uint32_t*)objptrToPointer(bot_op, NULL);
  objptr* data = (objptr*)objptrToPointer(data_op, NULL);

  uint64_t local_top = atomic_load_explicit(top, memory_order_acquire);
  atomic_thread_fence(memory_order_seq_cst);
  uint32_t local_bot = atomic_load_explicit(bot, memory_order_acquire);

  uint32_t local_top_idx = UNPACK_IDX(local_top);

  if (local_bot <= local_top_idx) {
    return fail_value;
  }

  objptr elem = atomic_load_explicit(data + local_top_idx, memory_order_acquire);
  uint64_t local_top_tag = UNPACK_TAG(local_top);
  uint64_t desired_top = PACK_TAGIDX(local_top_tag, local_top_idx+1);

  if (__sync_bool_compare_and_swap(top, local_top, desired_top)) {
    ABP_deque_check_successful_pop(elem, fail_value);
    return elem;
  }
  else {
    return fail_value;
  }
}


PRIVATE void ABP_deque_set_depth(
  [[maybe_unused]] GC_state s,
  objptr top_op,
  objptr bot_op,
  [[maybe_unused]] objptr data_op,
  uint32_t desired_depth)
{
  uint64_t* top = (uint64_t*)objptrToPointer(top_op, NULL);
  uint32_t* bot = (uint32_t*)objptrToPointer(bot_op, NULL);

  uint64_t local_top = atomic_load_explicit(top, memory_order_acquire);
  uint32_t local_bot = atomic_load_explicit(bot, memory_order_acquire);

  uint32_t local_top_idx = UNPACK_IDX(local_top);
  uint32_t local_top_tag = UNPACK_TAG(local_top);
  uint64_t desired_top = PACK_TAGIDX(local_top_tag+1, desired_depth);

  if (local_top_idx != local_bot) {
    DIE(
      "Bug! Attempt to set depth of non-empty deque! tag=%u top=%u bot=%u desired_depth=%u",
      local_top_tag,
      local_top_idx,
      local_bot,
      desired_depth
    );
  }

  /* Have to make sure the intermediate state of the deque still appears to
   * be empty, i.e., we need to maintain bot index <= top index. So, if the
   * desired depth is smaller, we move the bot first. Otherwise, we move th
   * top first.
   */

  if (desired_depth == local_bot) {
    return;
  }
  else if (desired_depth < local_bot) {
    atomic_store_explicit(bot, desired_depth, memory_order_seq_cst);
    atomic_store_explicit(top, desired_top, memory_order_seq_cst);
  }
  else {

    atomic_store_explicit(top, desired_top, memory_order_seq_cst);
    atomic_store_explicit(bot, desired_depth, memory_order_seq_cst);
  }
}
