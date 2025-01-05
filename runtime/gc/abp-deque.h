/* Copyright (C) 2024-2025 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

// Try to push `elem_to_push`, returning success or failure if the deque
// is at capacity.
PRIVATE Bool ABP_deque_push_bot(
  GC_state s,
  objptr top,
  objptr bot,
  objptr data,
  objptr elem_to_push
);


PRIVATE objptr ABP_deque_try_pop_bot(
  GC_state s,
  objptr top,
  objptr bot,
  objptr data,
  objptr fail_value   // should be the value NONE, to return in case of failure
);


PRIVATE objptr ABP_deque_try_pop_top(
  GC_state s,
  objptr top_op,
  objptr bot_op,
  objptr data_op,
  objptr fail_value   // should be the value NONE, to return in case of failure
);


PRIVATE void ABP_deque_set_depth(
  GC_state s,
  objptr top_op,
  objptr bot_op,
  objptr data_op,
  uint32_t desired_depth
);


#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
