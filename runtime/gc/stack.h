/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Stack objects have the following layout:
 *
 * header ::
 * markTop (native-pointer) ::
 * markIndex (word32) ::
 * promoStackTop (native-pointer) ::
 * promoStackBot (native-pointer) ::
 * reserved ::
 * promoStackReserved ::
 * used ::
 * ... reserved bytes ...
 * ... promoStackReserved bytes ...
 *
 * The markTop and markIndex are used by the mark-compact GC.  The
 * reserved size gives the number of bytes for the stack (before the
 * next ML object).  The used size gives the number of bytes currently
 * used by the stack.  The sequence of reserved bytes correspond to ML
 * stack frames, which will be discussed in more detail in "frame.h".
*/
typedef struct GC_stack {
  /* markTop and markIndex are only used during marking.  They record
   * the current pointer in the stack that is being followed.  markTop
   * points to the top of the stack frame containing the pointer and
   * markIndex is the index in that frame's frameOffsets of the pointer
   * slot.  So, when the GC pointer reversal gets back to the stack,
   * it can continue with the next pointer (either in the current
   * frame or the next frame).
   */
  pointer markTop;
  uint32_t markIndex;
  /* Promotion Stack: Each slot is a pointer to a promotable frame of this
   * stack. Youngest at the top, oldest at the bottom. The compiler inserts
   * code to manage the promotion stack.
   * At every PCall:
   *   *(pointer*)promoStackTop = StackTop  // pointing to the current (now promotable) frame
   *   promoStackTop += sizeof(pointer)
   * At every seq (unpromoted) continuation:
   *   promoStackTop -= sizeof(pointer)
   * At every sync (promoted) continuation:
   *   promoStackTop -= sizeof(pointer)
   *   promoStackBot = promoStackTop
   * At promotion:
   *   promoStackBot += sizeof(pointer)
   */
  pointer promoStackTop;
  pointer promoStackBot;
  /* reserved is the number of bytes reserved for stack,
   * i.e. its maximum size.
   */
  size_t reserved;
  size_t promoStackReserved;
  /* used is the number of bytes used by the stack.
   * Stacks with used == reserved are continuations.
   */
  size_t used;
  /* The next address is the bottom of the stack, and the following
   * reserved bytes hold space for the stack. After that, the following
   * promoStackReserved bytes hold space for the promotion stack.
   */
} *GC_stack;

#define GC_STACK_METADATA_SIZE (GC_HEADER_SIZE)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayStack (GC_state s, GC_stack stack, FILE *stream);

static inline bool isStackEmpty (GC_stack stack);
#if ASSERT
static inline bool isStackReservedAligned (GC_state s, size_t reserved);
#endif

static inline size_t sizeofStackSlop (GC_state s);

static inline pointer getStackBottom (GC_state s, GC_stack stack);
static inline pointer getStackTop (GC_state s, GC_stack stack);
static inline pointer getStackLimitPlusSlop (GC_state s, GC_stack stack);
static inline pointer getStackLimit (GC_state s, GC_stack stack);
static inline GC_frameIndex getCachedStackTopFrameIndex (GC_state s);
static inline GC_frameIndex getStackTopFrameIndex (GC_state s, GC_stack stack);
static inline GC_frameInfo getStackTopFrameInfo (GC_state s, GC_stack stack);
static inline uint16_t getStackTopFrameSize (GC_state s, GC_stack stack);

static inline size_t getPromoStackSizeInBytes(GC_state s, GC_stack stack);
static inline size_t getPromoStackBotOffsetInBytes(GC_state s, GC_stack stack);

static inline size_t alignStackReserved (GC_state s, size_t reserved);
static inline size_t sizeofStackWithMetaData (GC_state s, size_t reserved, size_t promoStackReserved);
static inline size_t desiredPromoStackReserved (GC_state s, size_t reserved);
static inline size_t sizeofStackInitialReserved (GC_state s);
static inline size_t sizeofStackInitialPromoStackReserved (GC_state s);
static inline size_t sizeofStackMinimumReserved (GC_state s, GC_stack stack);
static inline size_t sizeofStackGrowReserved (GC_state s, GC_stack stack);
static inline size_t sizeofStackShrinkReserved (GC_state s, GC_stack stack, bool current);

// pointer to frame that is promotable, or NULL if no such frame
#if ASSERT
pointer findPromotableFrame (GC_state s, GC_stack stack);
pointer findYoungestPromotableFrame (GC_state s, GC_stack stack);
#endif
pointer getPromoStackOldestPromotableFrame (GC_state s, GC_stack stack);

void copyStackFrameToNewStack (GC_state s, pointer frame, GC_stack from, GC_stack to);

static inline void copyStack (GC_state s, GC_stack from, GC_stack to);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
