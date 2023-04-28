/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void displayStack (__attribute__ ((unused)) GC_state s,
                   GC_stack stack,
                   FILE *stream) {
  fprintf(stream,
          "\t\treserved = %"PRIuMAX"\n"
          "\t\tused = %"PRIuMAX"\n",
          (uintmax_t)stack->reserved,
          (uintmax_t)stack->used);
}

bool isStackEmpty (GC_stack stack) {
  return 0 == stack->used;
}

#if ASSERT
bool isStackReservedAligned (GC_state s, size_t reserved) {
  return isAligned (GC_STACK_METADATA_SIZE + sizeof (struct GC_stack) + reserved,
                    s->alignment);
}
#endif

/* sizeofStackSlop returns the amount of "slop" space needed between
 * the top of the stack and the end of the stack space.
 */
size_t sizeofStackSlop (GC_state s) {
  return (size_t)(2 * s->maxFrameSize);
}


/* Pointer to the bottommost word in use on the stack. */
pointer getStackBottom (ARG_USED_FOR_ASSERT GC_state s, GC_stack stack) {
  pointer res;

  res = ((pointer)stack) + sizeof (struct GC_stack);
  assert (isAligned ((size_t)res, s->alignment));
  return res;
}

/* Pointer to the topmost word in use on the stack. */
pointer getStackTop (GC_state s, GC_stack stack) {
  pointer res;

  res = getStackBottom (s, stack) + stack->used;
  assert (isAligned ((size_t)res, s->alignment));
  return res;
}

/* Pointer to the end of stack. */
pointer getStackLimitPlusSlop (GC_state s, GC_stack stack) {
  pointer res;

  res = getStackBottom (s, stack) + stack->reserved;
  // assert (isAligned ((size_t)res, s->alignment));
  return res;
}

/* The maximum value which is valid for stackTop. */
pointer getStackLimit (GC_state s, GC_stack stack) {
  pointer res;

  res  = getStackLimitPlusSlop (s, stack) - sizeofStackSlop (s);
  // assert (isAligned ((size_t)res, s->alignment));
  return res;
}

GC_frameIndex getCachedStackTopFrameIndex (GC_state s) {
  GC_frameIndex res;

  res =
    getFrameIndexFromReturnAddress
    (s, *((GC_returnAddress*)(s->stackTop - GC_RETURNADDRESS_SIZE)));
  return res;
}

GC_frameIndex getStackTopFrameIndex (GC_state s, GC_stack stack) {
  GC_frameIndex res;

  res =
    getFrameIndexFromReturnAddress
    (s, *((GC_returnAddress*)(getStackTop (s, stack) - GC_RETURNADDRESS_SIZE)));
  return res;
}

GC_frameInfo getStackTopFrameInfo (GC_state s, GC_stack stack) {
  GC_frameInfo frameInfo;

  frameInfo = getFrameInfoFromFrameIndex (s, getStackTopFrameIndex (s, stack));
  return frameInfo;
}

uint16_t getStackTopFrameSize (GC_state s, GC_stack stack) {
  GC_frameInfo frameInfo;

  assert (not (isStackEmpty (stack)));
  frameInfo = getStackTopFrameInfo (s, stack);
  return frameInfo->size;
}


size_t alignStackReserved (GC_state s, size_t reserved) {
  size_t res;

  res = alignWithExtra (s, reserved, GC_STACK_METADATA_SIZE + sizeof (struct GC_stack));
  if (DEBUG_STACKS)
    fprintf (stderr, "%"PRIuMAX" = alignStackReserved (%"PRIuMAX")\n",
             (uintmax_t)res, (uintmax_t)reserved);
  assert (isStackReservedAligned (s, res));
  return res;
}

size_t sizeofStackWithMetaData (ARG_USED_FOR_ASSERT GC_state s, size_t reserved) {
  size_t res;

  assert (isStackReservedAligned (s, reserved));
  res = GC_STACK_METADATA_SIZE + sizeof (struct GC_stack) + reserved;
  if (DEBUG_STACKS)
    fprintf (stderr, "%"PRIuMAX" = sizeofStackWithMetaData (%"PRIuMAX")\n",
             (uintmax_t)res, (uintmax_t)reserved);
  assert (isAligned (res, s->alignment));
  return res;
}

size_t sizeofStackInitialReserved (GC_state s) {
  size_t res;

  res = alignStackReserved(s, sizeofStackSlop (s));
  return res;
}

size_t sizeofStackMinimumReserved (GC_state s, GC_stack stack) {
  size_t res;

  res = alignStackReserved (s,
                            stack->used
                            + sizeofStackSlop (s)
                            - getStackTopFrameSize (s, stack));
  return res;
}

size_t sizeofStackGrowReserved (GC_state s, GC_stack stack) {
  double reservedD;
  size_t reservedGrow, reservedMin, reservedNew;
  const size_t RESERVED_MAX = (SIZE_MAX >> 2);

  assert (isStackReservedAligned (s, stack->reserved));
  reservedD = (double)(stack->reserved);
  double reservedGrowD =
    (double)s->controls->ratios.stackCurrentGrow * reservedD;
  reservedGrow =
    reservedGrowD > (double)RESERVED_MAX
    ? RESERVED_MAX
    : (size_t)reservedGrowD;
  reservedMin = sizeofStackMinimumReserved (s, stack);
  reservedNew =
    alignStackReserved
    (s, max (reservedGrow, reservedMin));
  assert (isStackReservedAligned (s, reservedNew));
  return reservedNew;
}

size_t sizeofStackShrinkReserved (GC_state s, GC_stack stack, bool current) {
  double usedD, reservedD;
  size_t reservedMax, reservedShrink, reservedMin, reservedNew;
  const size_t RESERVED_MAX = (SIZE_MAX >> 2);

  assert (isStackReservedAligned (s, stack->reserved));
  usedD = (double)(stack->used);
  reservedD = (double)(stack->reserved);
  if (current) {
    /* Shrink current stacks. */
    double reservedMaxD =
      (double)(s->controls->ratios.stackCurrentMaxReserved) * usedD;
    reservedMax =
      reservedMaxD > (double)RESERVED_MAX
      ? RESERVED_MAX
      : (size_t)reservedMaxD;
    double reservedPermitD =
      (double)(s->controls->ratios.stackCurrentPermitReserved) * usedD;
    size_t reservedPermit =
      reservedPermitD > (double)RESERVED_MAX
      ? RESERVED_MAX
      : (size_t)reservedPermitD;
    reservedShrink =
      (stack->reserved <= reservedPermit)
      ? stack->reserved
      : (size_t)((double)(s->controls->ratios.stackCurrentShrink) * reservedD);
    reservedMin = sizeofStackMinimumReserved (s, stack);
  } else {
    /* Shrink paused stacks. */
    double reservedMaxD =
      (double)(s->controls->ratios.stackMaxReserved) * usedD;
    reservedMax =
      reservedMaxD > (double)RESERVED_MAX
      ? RESERVED_MAX
      : (size_t)reservedMaxD;
    reservedShrink =
      (size_t)((double)s->controls->ratios.stackShrink * reservedD);
    reservedMin = stack->used;
  }
  reservedNew =
    alignStackReserved
    (s, max(min(reservedMax,reservedShrink),reservedMin));
  /* It's possible that reservedNew > stack->reserved for the current
   * stack if the stack invariant is violated.  In that case, we want
   * to leave the stack alone, because some other part of the gc will
   * grow the stack.  We cannot do any growing here because we may run
   * out of to space.
   */
  assert (current or reservedNew <= stack->reserved);
  reservedNew = min (stack->reserved, reservedNew);
  assert (isStackReservedAligned (s, reservedNew));
  return reservedNew;
}

void copyStack (GC_state s, GC_stack from, GC_stack to) {
  pointer fromBottom, toBottom;

  fromBottom = getStackBottom (s, from);
  toBottom = getStackBottom (s, to);
  assert (from->used <= to->reserved);
  to->used = from->used;
  if (DEBUG_STACKS)
    fprintf (stderr, "stackCopy from "FMTPTR" to "FMTPTR" of length %"PRIuMAX"\n",
             (uintptr_t)fromBottom,
             (uintptr_t)toBottom,
             (uintmax_t)from->used);
  GC_memcpy (fromBottom, toBottom, from->used);
}

void copyStackFrameToNewStack (
  GC_state s,
  pointer frame,
  ARG_USED_FOR_ASSERT GC_stack from,
  GC_stack to)
{
  assert(getStackBottom(s, from) < frame);
  assert(frame <= getStackTop(s, from));

  GC_returnAddress ret = *((GC_returnAddress*)(frame - GC_RETURNADDRESS_SIZE));
  GC_frameInfo fi = getFrameInfoFromReturnAddress(s, ret);
  assert(fi->kind == PCALL_PARL_FRAME);
  pointer frameBottom = frame - fi->size;
  GC_memcpy(frameBottom, getStackBottom(s, to), fi->size);
  to->used = fi->size;
}

pointer findPromotableFrame (GC_state s, GC_stack stack) {

  pointer top = getStackTop(s, stack);
  pointer bottom = getStackBottom(s, stack);

  size_t numFrames = 0;
  size_t numCFrames = 0;
  size_t numLFrames = 0;
  size_t numRFrames = 0;

  pointer oldestCFrame = NULL;

  pointer cursor = top;
  while (cursor > bottom) {
    GC_returnAddress ret = *((GC_returnAddress*)(cursor - GC_RETURNADDRESS_SIZE));
    GC_frameInfo fi = getFrameInfoFromReturnAddress(s, ret);

    switch (fi->kind) {
      case PCALL_CONT_FRAME: {
        numCFrames++;
        oldestCFrame = cursor;
        break;
      }
      case PCALL_PARL_FRAME:
        numLFrames++;
        break;
      case PCALL_PARR_FRAME:
        numRFrames++;
        break;
      default:
        break;
    }

    numFrames++;
    cursor = cursor - fi->size;
  }

  // LOG(LM_PARALLEL, LL_FORCE,
  //   "frames %zu, cont %zu, parl %zu, parr %zu",
  //   numFrames,
  //   numCFrames,
  //   numLFrames,
  //   numRFrames
  // );

  if (oldestCFrame == NULL) {
    return NULL;
  }

  // GC_returnAddress orig =
  //   *((GC_returnAddress*)(oldestWaitingCFrame - GC_RETURNADDRESS_SIZE));
  // GC_returnAddress left =
  //   *((GC_returnAddress*)(oldestWaitingCFrame - 2*GC_RETURNADDRESS_SIZE));
  // GC_returnAddress right =
  //   *((GC_returnAddress*)(oldestWaitingCFrame - 3*GC_RETURNADDRESS_SIZE));

  // LOG(LM_PARALLEL, LL_FORCE,
  //   "oldest cont frame: orig "FMTRA", left "FMTRA", right "FMTRA,
  //   orig,
  //   left,
  //   right);

  return oldestCFrame;
}


pointer findYoungestPromotableFrame (GC_state s, GC_stack stack) {

  pointer top = getStackTop(s, stack);
  pointer bottom = getStackBottom(s, stack);

  pointer youngestCFrame = NULL;

  pointer cursor = top;
  while (cursor > bottom && youngestCFrame == NULL) {
    GC_returnAddress ret = *((GC_returnAddress*)(cursor - GC_RETURNADDRESS_SIZE));
    GC_frameInfo fi = getFrameInfoFromReturnAddress(s, ret);

    switch (fi->kind) {
      case PCALL_CONT_FRAME: {
        youngestCFrame = cursor;
        break;
      }
      case PCALL_PARL_FRAME:
        break;
      case PCALL_PARR_FRAME:
        break;
      default:
        break;
    }

    cursor = cursor - fi->size;
  }

  if (youngestCFrame == NULL) {
    return NULL;
  }

  return youngestCFrame;
}