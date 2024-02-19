/* Copyright (C) 2016,2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void callIfIsObjptr (GC_state s, GC_foreachObjptrClosure f, objptr *opp) {
  objptr op = *opp;
  if (isObjptr(op)) {
    LOG(LM_FOREACH, LL_DEBUG,
        "Calling for opp "FMTPTR" op "FMTOBJPTR,
        ((uintptr_t)(opp)),
        *opp);
    f->fun(s, opp, op, f->env);
  }
}

/* foreachGlobalObjptr (s, f)
 *
 * Apply f to each global object pointer into the heap.
 */
void foreachGlobalObjptr (GC_state s, GC_foreachObjptrClosure f) {
  for (unsigned int i = 0; i < s->globalsLength; ++i) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "foreachGlobal %u\n", i);
    callIfIsObjptr (s, f, &s->globals [i]);
  }
  foreachObjptrInStaticHeap (s, &s->staticHeaps.root, f, TRUE);
  if (DEBUG_DETAILED)
    fprintf (stderr, "foreachGlobal threads\n");
  if (s->procStates) {
    for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
      callIfIsObjptr (s, f, &s->procStates[proc].callFromCHandlerThread);
      callIfIsObjptr (s, f, &s->procStates[proc].currentThread);
      callIfIsObjptr (s, f, &s->procStates[proc].wsQueue);
      callIfIsObjptr (s, f, &s->procStates[proc].savedThread);
      callIfIsObjptr (s, f, &s->procStates[proc].signalHandlerThread);

      if (s->procStates[proc].roots) {
        for (uint32_t i = 0; i < s->procStates[proc].rootsLength; i++) {
          callIfIsObjptr (s, f, &s->procStates[proc].roots[i]);
        }
      }
    }
  } else {
    callIfIsObjptr (s, f, &s->callFromCHandlerThread);
    callIfIsObjptr (s, f, &s->currentThread);
    callIfIsObjptr (s, f, &s->wsQueue);
    callIfIsObjptr (s, f, &s->savedThread);
    callIfIsObjptr (s, f, &s->signalHandlerThread);
  }
}

struct objectInfo {
  objptr op;
  unsigned int objectTypeIndex;
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  bool hasIdentity;
};

void printObjectInfo(struct objectInfo *args);
void printObjectInfo(struct objectInfo *args) {
  printf("object %p index %u tag %s bytes %u ptrs %u identity %d\n",
    (void *)args->op,
    args->objectTypeIndex,
    objectTypeTagToString(args->tag),
    args->bytesNonObjptrs,
    args->numObjptrs,
    args->hasIdentity);
}

void printObjectsInRange(GC_state s,
                         pointer front,
                         pointer back) {
  GC_header header;
  struct objectInfo oi;

  pointer p = front;
  while (p < back) {
    p = advanceToObjectData(s, p);
    header = getHeader(p);
    oi.objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
    splitHeader(s, header, &(oi.tag), &(oi.hasIdentity), &(oi.bytesNonObjptrs), &(oi.numObjptrs));
    oi.op = pointerToObjptr(p, NULL);

    if (NORMAL_TAG == oi.tag) {
      p += oi.bytesNonObjptrs + (oi.numObjptrs * OBJPTR_SIZE);
    }
    else if (SEQUENCE_TAG == oi.tag) {
      size_t dataBytes = getSequenceLength(p) * (oi.bytesNonObjptrs + (oi.numObjptrs * OBJPTR_SIZE));
      p += alignWithExtra (s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
    }
    else if (STACK_TAG == oi.tag) {
      p += sizeof (struct GC_stack) + ((GC_stack)p)->reserved;
    }
    else {
      fprintf(stderr, "cannot handle tag %u", oi.tag);
      return;
    }

    printObjectInfo(&oi);
  }
}

/* foreachObjptrInObject (s, p, f, skipWeaks)
 *
 * Applies f to each object pointer in the object pointed to by p.
 * Returns pointer to the end of object, i.e. just past object.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */
pointer foreachObjptrInObject (GC_state s, pointer p,
                               GC_objptrPredicateClosure pred,
                               GC_foreachObjptrClosure f,
                               bool skipWeaks) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  bool skip = !pred->fun(s, p, pred->env);

  header = getRacyHeader (p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "foreachObjptrInObject ("FMTPTR")"
             "  header = "FMTHDR
             "  tag = %s"
             "  bytesNonObjptrs = %d"
             "  numObjptrs = %d\n",
             (uintptr_t)p, header, objectTypeTagToString (tag),
             bytesNonObjptrs, numObjptrs);
  if (NORMAL_TAG == tag) {
    p += bytesNonObjptrs;
    pointer max = p + (numObjptrs * OBJPTR_SIZE);

    if (skip) {
      p = max;
      goto DONE;
    }

    /* Apply f to all internal pointers. */
    for ( ; p < max; p += OBJPTR_SIZE) {
      if (DEBUG_DETAILED)
        fprintf (stderr,
                 "  p = "FMTPTR"  *p = "FMTOBJPTR"\n",
                 (uintptr_t)p, *(objptr*)p);
      callIfIsObjptr (s, f, ((objptr*)(p)));
    }
  } else if (WEAK_TAG == tag) {
    p += bytesNonObjptrs;
    if (1 == numObjptrs) {
      if ((!skipWeaks) && (!skip)) {
        callIfIsObjptr (s, f, ((objptr*)(p)));
      }
      p += OBJPTR_SIZE;
    }
  } else if (SEQUENCE_TAG == tag) {
    size_t bytesPerElement;
    size_t dataBytes;
    pointer last;
    GC_sequenceLength numElements;

    numElements = getSequenceLength (p);
    bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    dataBytes = numElements * bytesPerElement;
    if (0 == numObjptrs) {
      /* No objptrs to process. */
      ;
    } else {
      last = p + dataBytes;

      if (skip) {
        goto ARRAY_DOME;
      }

      if (0 == bytesNonObjptrs)
        /* Sequence with only pointers. */
        for ( ; p < last; p += OBJPTR_SIZE)
          callIfIsObjptr (s, f, ((objptr*)(p)));
      else {
        /* Sequence with a mix of pointers and non-pointers. */
        size_t bytesObjptrs;

        bytesObjptrs = numObjptrs * OBJPTR_SIZE;

        /* For each sequence element. */
        for ( ; p < last; ) {
          pointer next;

          /* Skip the non-pointers. */
          p += bytesNonObjptrs;
          next = p + bytesObjptrs;
          /* For each internal pointer. */
          for ( ; p < next; p += OBJPTR_SIZE) {
            callIfIsObjptr (s, f, ((objptr*)(p)));
          }
        }
      }
      assert (p == last);
      p -= dataBytes;
    }

 ARRAY_DOME:
    p += alignWithExtra (s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
  } else if (STACK_TAG == tag) {
    GC_stack stack;
    pointer top, bottom;
    unsigned int i;
    GC_returnAddress returnAddress;
    GC_frameInfo frameInfo;
    GC_frameOffsets frameOffsets;


    stack = (GC_stack)p;
    bottom = getStackBottom (s, stack);
    top = getStackTop (s, stack);
    if (DEBUG) {
      fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR"\n",
               (uintptr_t)bottom, (uintptr_t)top);
    }

    if (skip) {
      goto STACK_DONE;
    }

    assert (stack->used <= stack->reserved);
    while (top > bottom) {
      /* Invariant: top points just past a "return address". */
      returnAddress = *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
      if (DEBUG) {
        fprintf (stderr, "  top = "FMTPTR"  return address = "FMTRA"\n",
                 (uintptr_t)top, returnAddress);
      }
      frameInfo = getFrameInfoFromReturnAddress (s, returnAddress);
      frameOffsets = frameInfo->offsets; // index zero of this array is size
      top -= frameInfo->size;
      for (i = 0 ; i < frameOffsets[0] ; ++i) {
        if (DEBUG) {
          fprintf(stderr, "  offset %"PRIx16"  address "FMTOBJPTR"\n",
                  frameOffsets[i + 1], *(objptr*)(top + frameOffsets[i + 1]));
        }

        callIfIsObjptr (s, f, ((objptr*)(top + frameOffsets[i + 1])));
      }
    }
    assert(top == bottom);

 STACK_DONE:
    p += sizeof (struct GC_stack) + stack->reserved;
  } else {
    assert (0 and "unknown object tag type");
  }

DONE:
  return p;
}


/* ========================================================================= */

void foreachObjptrInSequenceSlice(
  GC_state s,
  pointer p,
  GC_foreachObjptrClosure f,
  uint64_t startIdx,
  uint64_t stopIdx)
{
  GC_header header = getRacyHeader(p);
  GC_objectTypeTag tag;
  uint16_t numObjptrs;
  uint16_t bytesNonObjptrs;
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  if (SEQUENCE_TAG != tag) {
    DIE("argument object is not a sequence");
  }

  GC_sequenceLength numElements = getSequenceLength(p);

  if (startIdx > stopIdx || stopIdx > numElements) {
    DIE("invalid index range");
  }

  size_t bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  if (0 == numObjptrs) {
    /* No objptrs to process. */
    return;
  }

  pointer start = p + (bytesPerElement * startIdx);
  pointer stop = p + (bytesPerElement * stopIdx);

  if (0 == bytesNonObjptrs) {
    /* Sequence with only pointers. */
    for (pointer cursor = start; cursor < stop; cursor += OBJPTR_SIZE) {
      callIfIsObjptr(s, f, (objptr*)cursor);
    }
  } else {
    /* Sequence with a mix of pointers and non-pointers. */
    size_t bytesObjptrs = numObjptrs * OBJPTR_SIZE;

    for (pointer cursor = start; cursor < stop; ) {
      /* Skip the non-pointers. */
      cursor += bytesNonObjptrs;
      pointer next = cursor + bytesObjptrs;
      /* For each internal pointer. */
      for ( ; cursor < next; cursor += OBJPTR_SIZE) {
        callIfIsObjptr(s, f, (objptr*)cursor);
      }
    }
  }

}

/* ========================================================================= */


/* foreachObjptrInRange (s, front, back, f, skipWeaks)
 *
 * Apply f to each pointer between front and *back, which should be a
 * contiguous sequence of objects, where front points at the beginning
 * of the first object and *back points just past the end of the last
 * object.  f may increase *back (for example, this is done by
 * forward).  foreachObjptrInRange returns a pointer to the end of
 * the last object it visits.
 *
 * If skipWeaks, then the object pointer in weak objects is skipped.
 */

pointer foreachObjptrInRange (GC_state s, pointer front, pointer *back,
                              GC_foreachObjptrClosure f, bool skipWeaks) {
  pointer b;

  assert (isFrontierAligned (s, front));
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "foreachObjptrInRange  front = "FMTPTR"  *back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)(*back));
  b = *back;
  assert (front <= b);
  while (front < b) {
    while (front < b) {
      assert (isAligned ((size_t)front, GC_MODEL_MINALIGN));
      if (DEBUG_DETAILED)
        fprintf (stderr,
                 "  front = "FMTPTR"  *back = "FMTPTR"\n",
                 (uintptr_t)front, (uintptr_t)(*back));
      pointer p = advanceToObjectData (s, front);
      assert (isAligned ((size_t)p, s->alignment));
      front = foreachObjptrInObject (s, p, &trueObjptrPredicateClosure, f, skipWeaks);
    }
    b = *back;
  }
  return front;
}

void foreachObjptrInStaticHeap (GC_state s,
                                struct GC_staticHeap *staticHeap,
                                GC_foreachObjptrClosure f,
                                bool skipWeaks) {
  pointer front = alignFrontier (s, staticHeap->start);
  pointer back = staticHeap->start + staticHeap->size;
  foreachObjptrInRange (s, front, &back, f, skipWeaks);
}

/* Apply f to the frame index of each frame in the current thread's stack. */
void foreachStackFrame (GC_state s, GC_foreachStackFrameClosure f) {
  pointer bottom;
  GC_frameIndex frameIndex;
  GC_frameInfo frameInfo;
  GC_returnAddress returnAddress;
  pointer top;

  if (DEBUG_PROFILE)
    fprintf (stderr, "foreachStackFrame\n");
  bottom = getStackBottom (s, getStackCurrent(s));
  if (DEBUG_PROFILE)
    fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR".\n",
             (uintptr_t)bottom, (uintptr_t)s->stackTop);
  top = s->stackTop;
  while (top > bottom) {
    returnAddress = *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
    frameIndex = getFrameIndexFromReturnAddress (s, returnAddress);
    if (DEBUG_PROFILE)
      fprintf (stderr, "top = "FMTPTR"  frameIndex = "FMTFI"\n",
               (uintptr_t)top, frameIndex);
    unless (frameIndex < s->frameInfosLength)
      die ("top = "FMTPTR"  returnAddress = "FMTRA"  frameIndex = "FMTFI"\n",
           (uintptr_t)top, (uintptr_t)returnAddress, frameIndex);
    frameInfo = &(s->frameInfos[frameIndex]);
    assert (frameInfo->size > 0);
    top -= frameInfo->size;
    f->fun (s, frameIndex, frameInfo, top, f->env);
  }
  if (DEBUG_PROFILE)
    fprintf (stderr, "done foreachStackFrame\n");
}

bool trueObjptrPredicate(GC_state s, pointer p, void* args) {
  /* silence warnings */
  ((void)(s));
  ((void)(p));
  ((void)(args));

  return TRUE;
}

struct GC_objptrPredicateClosure trueObjptrPredicateClosure =
  {.fun = trueObjptrPredicate, .env = NULL};
