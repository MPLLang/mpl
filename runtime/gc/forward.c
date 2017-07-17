/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if ASSERT
bool isPointerInToSpace (GC_state s, pointer p) {
  return (not (isPointer (p))
          or (s->forwardState.toStart <= p and p < s->forwardState.toLimit));
}

bool isObjptrInToSpace (GC_state s, objptr op) {
  pointer p;

  if (not (isObjptr (op)))
    return TRUE;
  p = objptrToPointer (op, s->forwardState.toStart);
  return isPointerInToSpace (s, p);
}
#endif

/* getFwdPtrp (p)
 *
 * Returns a pointer to the forwarding pointer for the object pointed to by p.
 */
objptr* getFwdPtrp (pointer p) {
  return (objptr*)(p
                   - OBJPTR_SIZE);
}

/* getFwdPtr (p)
 *
 * Returns the forwarding pointer for the object pointed to by p.
 */
objptr getFwdPtr (pointer p) {
  return *(getFwdPtrp(p));
}

/* hasFwdPtr (p)
 *
 * Returns true if the object pointed to by p has a valid forwarding pointer.
 */
bool hasFwdPtr (pointer p) {
  return (getFwdPtr (p) != BOGUS_OBJPTR);
}

/* forward (s, opp)
 * Forwards the object pointed to by *opp and updates *opp to point to
 * the new object.
 */
void forwardObjptr (GC_state s, objptr *opp, void* ignored) {
  objptr op;
  pointer p;

  /* silence compiler warning */
  ((void)(ignored));

  op = *opp;
  p = objptrToPointer (op, s->heap->start);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "forwardObjptr  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR"\n",
             (uintptr_t)opp, op, (uintptr_t)p);

  if (HM_HH_objptrInHierarchicalHeap(s, *opp)) {
    /*
     * We do not support collecting the global heap while the hierarchical
     * heaps are in play
     */
    die(__FILE__ ":%d: forwardObjptr(): Tried to collect global heap while "
        "hierarchical heaps are used!",
        __LINE__);
  }

  assert (isObjptrInFromSpace (s, *opp));
  if (DEBUG_DETAILED and hasFwdPtr(p))
    fprintf (stderr, "  already FORWARDED\n");
  if (not (hasFwdPtr(p))) { /* forward the object */
    size_t size, skip;

    GC_header header;
    size_t metaDataBytes, objectBytes;
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs, numObjptrs;

    header = getHeader(p);
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

    /* Compute the space taken by the metadata and object body. */
    if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
      metaDataBytes = GC_NORMAL_METADATA_SIZE;
      objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
      skip = 0;
    } else if (ARRAY_TAG == tag) {
      metaDataBytes = GC_ARRAY_METADATA_SIZE;
      objectBytes = sizeofArrayNoMetaData (s, getArrayLength (p),
                                           bytesNonObjptrs, numObjptrs);
      skip = 0;
    } else { /* Stack. */
      bool current;
      size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      metaDataBytes = GC_STACK_METADATA_SIZE;
      stack = (GC_stack)p;

      /* Check if the pointer is the current stack of any processor. */
      current = false;
      for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
        current = current || (getStackCurrent(&s->procStates[proc]) == stack);
      }
      /* If the primary thread invokes a GC before the secondary
       * threads begin executing, then their initial stacks are empty
       * and should be treated as inactive.
       */
      current = current && not isStackEmpty(stack);

      reservedNew = sizeofStackShrinkReserved (s, stack, current);
      if (reservedNew < stack->reserved) {
        if (DEBUG_STACKS or s->controls->messages)
          fprintf (stderr,
                   "[GC: Shrinking stack at "FMTPTR" of size %s bytes to size %s bytes, using %s bytes.]\n",
                   ((uintptr_t)(stack)),
                   uintmaxToCommaString(stack->reserved),
                   uintmaxToCommaString(reservedNew),
                   uintmaxToCommaString(stack->used));
        stack->reserved = reservedNew;
      }
      objectBytes = sizeof (struct GC_stack) + stack->used;
      skip = stack->reserved - stack->used;
    }
    size = metaDataBytes + objectBytes;
    assert (s->forwardState.back + size + skip <= s->forwardState.toLimit);
    /* Copy the object. */
    GC_memcpy (p - metaDataBytes, s->forwardState.back, size);
    /* If the object has a valid weak pointer, link it into the weaks
     * for update after the copying GC is done.
     */
    if ((WEAK_TAG == tag) and (numObjptrs == 1)) {
      GC_weak w;

      w = (GC_weak)(s->forwardState.back + GC_NORMAL_METADATA_SIZE + offsetofWeak (s));
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarding weak "FMTPTR" ",
                 (uintptr_t)w);
      if (isObjptr (w->objptr)
          and (not s->forwardState.amInMinorGC
               or isObjptrInNursery (s, w->objptr))) {
        if (DEBUG_WEAK)
          fprintf (stderr, "linking\n");
        w->link = s->weaks;
        s->weaks = w;
      } else {
        if (DEBUG_WEAK)
          fprintf (stderr, "not linking\n");
      }
    }

    /* Store the forwarding pointer in the old object metadata. */
    *(getFwdPtrp(p)) = pointerToObjptr (s->forwardState.back + metaDataBytes,
                                        s->forwardState.toStart);
    assert (hasFwdPtr(p));
    /* Update the back of the queue. */
    s->forwardState.back += size + skip;
    assert (isAligned ((size_t)s->forwardState.back + GC_NORMAL_METADATA_SIZE,
                       s->alignment));

    if (GC_HIERARCHICAL_HEAP_HEADER == header) {
      /* update level chunk head containingHH pointers */
      HM_HH_updateLevelListPointers(getFwdPtr(p));
    }
  }
  *opp = getFwdPtr(p);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "forwardObjptr --> *opp = "FMTPTR"\n",
             (uintptr_t)*opp);
  assert (isObjptrInToSpace (s, *opp));
}

void forwardObjptrIfInNursery (GC_state s, objptr *opp, void* ignored) {
  objptr op;
  pointer p;

  /* silence compiler warning */
  ((void)(ignored));

  op = *opp;
  p = objptrToPointer (op, s->heap->start);
  if (p < s->heap->nursery)
    return;
  if (DEBUG_GENERATIONAL)
    fprintf (stderr,
             "forwardObjptrIfInNursery  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR"\n",
             (uintptr_t)opp, op, (uintptr_t)p);
  /* RAM_NOTE: Should this just be limitPlusSlop like in upstream? */
  assert (s->heap->nursery <= p and p < s->heap->frontier);
  forwardObjptr (s, opp, NULL);
}

/* Walk through all the cards and forward all intergenerational pointers. */
void forwardInterGenerationalObjptrs (GC_state s) {
  GC_cardMapElem *cardMap;
  GC_crossMapElem *crossMap;
  pointer oldGenStart, oldGenEnd;

  size_t cardIndex, maxCardIndex;
  pointer cardStart, cardEnd;
  pointer objectStart;

  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "Forwarding inter-generational pointers.\n");
  updateCrossMap (s);
  /* Constants. */
  cardMap = s->generationalMaps.cardMap;
  crossMap = s->generationalMaps.crossMap;
  maxCardIndex = sizeToCardMapIndex (align (s->heap->oldGenSize, CARD_SIZE));
  oldGenStart = s->heap->start;
  oldGenEnd = oldGenStart + s->heap->oldGenSize;
  /* Loop variables*/
  objectStart = alignFrontier (s, s->heap->start);
  cardIndex = 0;
  cardStart = oldGenStart;
checkAll:
  assert (cardIndex <= maxCardIndex);
  assert (isFrontierAligned (s, objectStart));
  if (cardIndex == maxCardIndex)
    goto done;
checkCard:
  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "checking card %"PRIuMAX"  objectStart = "FMTPTR"\n",
             (uintmax_t)cardIndex, (uintptr_t)objectStart);
  assert (objectStart < oldGenStart + cardMapIndexToSize (cardIndex + 1));
  if (cardMap[cardIndex]) {
    pointer lastObject;

    s->cumulativeStatistics->numCardsMarked++;
    if (DEBUG_GENERATIONAL)
      fprintf (stderr, "card %"PRIuMAX" is marked  objectStart = "FMTPTR"\n",
               (uintmax_t)cardIndex, (uintptr_t)objectStart);
    assert (isFrontierAligned (s, objectStart));
    cardEnd = cardStart + CARD_SIZE;
    if (oldGenEnd < cardEnd)
      cardEnd = oldGenEnd;
    assert (objectStart < cardEnd);
    lastObject = objectStart;
    /* If we ever add Weak.set, then there could be intergenerational
     * weak pointers, in which case we would need to link the weak
     * objects into s->weaks.  But for now, since there is no
     * Weak.set, the foreachObjptrInRange will do the right thing on
     * weaks, since the weak pointer will never be into the nursery.
     */
    objectStart = foreachObjptrInRange (s,
                                        objectStart,
                                        &cardEnd,
                                        FALSE,
                                        NULL,
                                        trueObjptrPredicate,
                                        NULL,
                                        forwardObjptrIfInNursery,
                                        NULL);
    s->cumulativeStatistics->bytesScannedMinor += (uintmax_t)(objectStart - lastObject);
    if (objectStart == oldGenEnd)
      goto done;
    cardIndex = sizeToCardMapIndex ((size_t)(objectStart - oldGenStart));
    cardStart = oldGenStart + cardMapIndexToSize (cardIndex);
    goto checkCard;
  } else {
    unless (CROSS_MAP_EMPTY == crossMap[cardIndex])
      objectStart = cardStart + (size_t)(crossMap[cardIndex] * CROSS_MAP_OFFSET_SCALE);
    if (DEBUG_GENERATIONAL)
      fprintf (stderr,
               "card %"PRIuMAX" is not marked"
               "  crossMap[%"PRIuMAX"] == %"PRIuMAX""
               "  objectStart = "FMTPTR"\n",
               (uintmax_t)cardIndex, (uintmax_t)cardIndex,
               (uintmax_t)(crossMap[cardIndex] * CROSS_MAP_OFFSET_SCALE),
               (uintptr_t)objectStart);
    cardIndex++;
    cardStart += CARD_SIZE;
    goto checkAll;
  }
  assert (FALSE);
done:
  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "Forwarding inter-generational pointers done.\n");
}
