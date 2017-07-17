/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Depth-first Marking                        */
/* ---------------------------------------------------------------- */

static bool noopDescendHook(GC_state s, void* rawArgs, objptr object);
static void noopAscendHook(GC_state s, void* rawArgs, objptr* objectObjptr);

bool isPointerMarked (pointer p) {
  return MARK_MASK & getHeader (p);
}

bool isPointerMarkedByMode (pointer p, GC_markMode m) {
  switch (m) {
  case MARK_MODE:
    return isPointerMarked (p);
  case UNMARK_MODE:
    return not isPointerMarked (p);
  default:
    die ("bad mark mode %u", m);
  }
}

/*
 * RAM_NOTE: DFS marking, hash-cons'ing, and weak-link'ing should be folded into
 * descendHook() and ascendHook()
 */
/* dfsMarkByModeCustom (s, r, m, shc, slw, dh, ah)
 *
 * Sets all the mark bits in the object graph pointed to by r.
 *
 * If m is MARK_MODE, it sets the bits to 1.
 * If m is UNMARK_MODE, it sets the bits to 0.
 *
 * If shc, it hash-conses the objects marked.
 *
 * If slw, it links the weak objects marked.
 *
 * dh is called before descending into an object and returns if it should be
 * descended into
 *
 * ah is called before ascending to an object's parent in the dfs tree.
 *
 * It returns the total size in bytes of the objects marked.
 */
size_t dfsMarkByModeCustom (GC_state s, pointer root,
                            GC_markMode mode,
                            bool shouldHashCons,
                            bool shouldLinkWeaks,
                            dfsDescendHookFun descendHook,
                            void* descendHookArgs,
                            dfsAscendHookFun ascendHook,
                            void* ascendHookArgs
                            ) {
  GC_header mark; /* Used to set or clear the mark bit. */
  size_t size; /* Total number of bytes marked. */
  pointer cur; /* The current object being marked. */
  pointer prev; /* The previous object on the mark stack. */
  pointer next; /* The next object to mark. */
  pointer todo; /* A pointer to the pointer in cur to next. */
  GC_header header;
  GC_header* headerp;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  uint32_t objptrIndex; /* The i'th pointer in the object (element) being marked. */
  GC_header nextHeader;
  GC_header* nextHeaderp;
  GC_arrayCounter arrayIndex;
  pointer top; /* The top of the next stack frame to mark. */
  GC_returnAddress returnAddress;
  GC_frameLayout frameLayout;
  GC_frameOffsets frameOffsets;
  uint32_t processor = Proc_processorNumber (s);

  LOG(LM_DFS_MARK, LL_DEBUG, "dfsMark(%p, %d)", ((void*)(root)), mode);

  if (DEBUG_DFS_MARK) {
    fprintf(stderr,
            "[%d] dfsMarkByMode: START %s\n",
            processor,
            (mode == MARK_MODE) ? "mark" : "unmark");
    fflush(stderr);
  }

  LOG(LM_DFS_MARK, LL_DEBUG, "descend(%p)@%d", ((void*)(root)), __LINE__);
  if (isPointerMarkedByMode (root, mode) ||
      !descendHook(s, descendHookArgs, pointerToObjptr(root, s->heap->start))) {
    /* Object has already been marked or should not be descended into. */
    if (DEBUG_DFS_MARK) {
      fprintf(stderr,
              "[%d] dfsMarkByMode: END %s\n",
              processor,
              (mode == MARK_MODE) ? "mark" : "unmark");
      fflush(stderr);
    }

    return 0;
  }
  mark = (MARK_MODE == mode) ? MARK_MASK : 0;
  size = 0;
  cur = root;
  prev = NULL;
  headerp = getHeaderp (cur);
  header = *headerp;
  goto mark;
markNext:
  /* cur is the object that was being marked.
   * prev is the mark stack.
   * next is the unmarked object to be marked.
   * nextHeaderp points to the header of next.
   * nextHeader is the header of next.
   * todo is a pointer to the pointer inside cur that points to next.
   */
  if (DEBUG_DFS_MARK) {
    fprintf (stderr,
             "[%d] markNext"
             "  cur = "FMTPTR"  next = "FMTPTR
             "  prev = "FMTPTR"  todo = "FMTPTR"\n",
             processor,
             (uintptr_t)cur, (uintptr_t)next,
             (uintptr_t)prev, (uintptr_t)todo);
    fflush(stderr);
  }
  assert (not isPointerMarkedByMode (next, mode));
  assert (nextHeaderp == getHeaderp (next));
  assert (nextHeader == getHeader (next));
  // assert (*(pointer*) todo == next);
  assert (fetchObjptrToPointer (todo, s->heap->start) == next);
  headerp = nextHeaderp;
  header = nextHeader;
  // *(pointer*)todo = prev;
  storeObjptrFromPointer (todo, prev, s->heap->start);
  prev = cur;
  cur = next;
  LOG(LM_DFS_MARK, LL_DEBUG, "descend(%p)@%d", ((void*)(cur)), __LINE__);
  if (!descendHook(s, descendHookArgs, pointerToObjptr(cur, s->heap->start))) {
    // should not descend into this object, so pop back
    if (DEBUG_DFS_MARK) {
      fprintf (stderr,
               "[%d] dfsMarkByModeCustom: markNext: descendHook(cur = %p) = FALSE\n",
               processor,
               ((void*)(cur)));
      fflush(stderr);
    }

    goto ret;
  }
mark:
  if (DEBUG_DFS_MARK) {
    fprintf (stderr, "[%d] mark  cur = "FMTPTR"  prev = "FMTPTR"  mode = %s\n",
             processor,
             (uintptr_t)cur, (uintptr_t)prev,
             (mode == MARK_MODE) ? "mark" : "unmark");
    fflush(stderr);
  }
  /* cur is the object to mark.
   * prev is the mark stack.
   * headerp points to the header of cur.
   * header is the header of cur.
   */
  assert (not isPointerMarkedByMode (cur, mode));
  assert (header == getHeader (cur));
  assert (headerp == getHeaderp (cur));
  header ^= MARK_MASK;
  /* Store the mark.  In the case of an object that contains a pointer to
   * itself, it is essential that we store the marked header before marking
   * the internal pointers (markInNormal below).  If we didn't, then we
   * would see the object as unmarked and traverse it again.
   */
  *headerp = header;
  splitHeader (s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if (NORMAL_TAG == tag) {
    size +=
      GC_NORMAL_METADATA_SIZE
      + bytesNonObjptrs
      + (numObjptrs * OBJPTR_SIZE);
    if (0 == numObjptrs) {
      /* There is nothing to mark. */
normalDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    todo = cur + bytesNonObjptrs;
    objptrIndex = 0;
markInNormal:
    if (DEBUG_DFS_MARK) {
      fprintf (stderr,
               "[%d] markInNormal  objptrIndex = %"PRIu32"\n",
               processor,
               objptrIndex);
      fflush(stderr);
    }
    assert (objptrIndex < numObjptrs);
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap->start);
    if (not isPointer (next)) {
markNextInNormal:
      assert (objptrIndex < numObjptrs);
      objptrIndex++;
      if (objptrIndex == numObjptrs) {
        /* Done.  Clear out the counters and return. */
        *headerp = header & ~COUNTER_MASK;
        goto normalDone;
      }
      todo += OBJPTR_SIZE;
      goto markInNormal;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons) {
        shareObjptr (s, (objptr*)todo);
      }
      LOG(LM_DFS_MARK, LL_DEBUG, "ascend(%p)@%d", ((void*)(todo)), __LINE__);
      /* Call the ascendHook since we will not be descending into 'next' */
      ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
      goto markNextInNormal;
    }
    *headerp = (header & ~COUNTER_MASK) | (objptrIndex << COUNTER_SHIFT);
    goto markNext;
  } else if (WEAK_TAG == tag) {
    /* Store the marked header and don't follow any pointers. */
    if (shouldLinkWeaks) {
      GC_weak w;

      w = (GC_weak)(cur + offsetofWeak (s));
      if (DEBUG_WEAK) {
        fprintf (stderr, "[%d] marking weak "FMTPTR" ",
                 processor,
                 (uintptr_t)w);
        fflush(stderr);
      }
      if (isObjptr (w->objptr)) {
        if (DEBUG_WEAK) {
          fprintf (stderr, "[%d] linking\n", processor);
          fflush(stderr);
        }
        w->link = s->weaks;
        s->weaks = w;
      } else {
        if (DEBUG_WEAK) {
          fprintf (stderr, "[%d] not linking\n", processor);
          fflush(stderr);
        }
      }
    }
    goto ret;
  } else if (ARRAY_TAG == tag) {
    /* When marking arrays:
     *   arrayIndex is the index of the element to mark.
     *   cur is the pointer to the array.
     *   objptrIndex is the index of the pointer within the element
     *     (i.e. the i'th pointer is at index i).
     *   todo is the start of the element.
     */
    size +=
      GC_ARRAY_METADATA_SIZE
      + sizeofArrayNoMetaData (s, getArrayLength (cur), bytesNonObjptrs, numObjptrs);
    if (0 == numObjptrs or 0 == getArrayLength (cur)) {
      /* There is nothing to mark. */
arrayDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    /* Begin marking first element. */
    arrayIndex = 0;
    todo = cur;
markArrayElt:
    assert (arrayIndex < getArrayLength (cur));
    objptrIndex = 0;
    /* Skip to the first pointer. */
    todo += bytesNonObjptrs;
markInArray:
    if (DEBUG_DFS_MARK) {
      fprintf (stderr, "[%d] markInArray arrayIndex = %"PRIxARRCTR" objptrIndex = %"PRIu32"\n",
               processor,
               arrayIndex, objptrIndex);
      fflush(stderr);
    }
    assert (arrayIndex < getArrayLength (cur));
    assert (objptrIndex < numObjptrs);
    assert (todo == indexArrayAtObjptrIndex (s, cur, arrayIndex, objptrIndex));
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap->start);
    if (not (isPointer(next))) {
markNextInArray:
      assert (arrayIndex < getArrayLength (cur));
      assert (objptrIndex < numObjptrs);
      assert (todo == indexArrayAtObjptrIndex (s, cur, arrayIndex, objptrIndex));
      todo += OBJPTR_SIZE;
      objptrIndex++;
      if (objptrIndex < numObjptrs)
        goto markInArray;
      arrayIndex++;
      if (arrayIndex < getArrayLength (cur))
        goto markArrayElt;
      /* Done.  Clear out the counters and return. */
      *getArrayCounterp (cur) = 0;
      *headerp = header & ~COUNTER_MASK;
      goto arrayDone;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons) {
        shareObjptr (s, (objptr*)todo);
      }
      LOG(LM_DFS_MARK, LL_DEBUG, "ascend(%p)@%d", ((void*)(todo)), __LINE__);
      /* Call the ascendHook since we will not be descending into 'next' */
      ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
      goto markNextInArray;
    }
    /* Recur and mark next. */
    *getArrayCounterp (cur) = arrayIndex;
    *headerp = (header & ~COUNTER_MASK) | (objptrIndex << COUNTER_SHIFT);
    goto markNext;
  } else {
    assert (STACK_TAG == tag);
    size +=
      GC_STACK_METADATA_SIZE
      + sizeof (struct GC_stack) + ((GC_stack)cur)->reserved;
    top = getStackTop (s, (GC_stack)cur);
    assert (((GC_stack)cur)->used <= ((GC_stack)cur)->reserved);
markInStack:
    /* Invariant: top points just past the return address of the frame
     * to be marked.
     */
    assert (getStackBottom (s, (GC_stack)cur) <= top);
    if (DEBUG_DFS_MARK) {
      fprintf (stderr, "[%d] markInStack  top = %"PRIuMAX"\n",
               processor,
               (uintmax_t)(top - getStackBottom (s, (GC_stack)cur)));
      fflush(stderr);
    }
    if (top == getStackBottom (s, (GC_stack)(cur)))
      goto ret;
    objptrIndex = 0;
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    ((GC_stack)cur)->markTop = top;
markInFrame:
    if (objptrIndex == frameOffsets [0]) {
      top -= frameLayout->size;
      goto markInStack;
    }
    todo = top - frameLayout->size + frameOffsets [objptrIndex + 1];
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap->start);
    if (DEBUG_DFS_MARK) {
      fprintf (stderr,
               "[%d]     offset %u  todo "FMTPTR"  next = "FMTPTR"\n",
               processor,
               frameOffsets [objptrIndex + 1],
               (uintptr_t)todo, (uintptr_t)next);
      fflush(stderr);
    }
    if (not isPointer (next)) {
      objptrIndex++;
      goto markInFrame;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      objptrIndex++;
      if (shouldHashCons) {
        shareObjptr (s, (objptr*)todo);
      }
      LOG(LM_DFS_MARK, LL_DEBUG, "ascend(%p)@%d", ((void*)(todo)), __LINE__);
      /* Call the ascendHook since we will not be descending into 'next' */
      ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
      goto markInFrame;
    }
    ((GC_stack)cur)->markIndex = objptrIndex;
    goto markNext;
  }
  assert (FALSE);
ret:
  /* Done marking cur, continue with prev.
   * Need to set the pointer in the prev object that pointed to cur
   * to point back to prev, and restore prev.
   */
  if (DEBUG_DFS_MARK) {
    fprintf (stderr, "[%d] return  cur = "FMTPTR"  prev = "FMTPTR"\n",
             processor,
             (uintptr_t)cur, (uintptr_t)prev);
    fflush(stderr);
  }
  /* RAM_NOTE: was 'assert (isPointerMarkedByMode (cur, mode));' */
  /*
   * RAM_NOTE: would be nice to have an equivalent assertion, perhaps by
   * restricting 'descendHook()'
   */
  if (NULL == prev) {
    if (DEBUG_DFS_MARK) {
      fprintf(stderr,
              "[%d] dfsMarkByMode: END %s\n",
              processor,
              (mode == MARK_MODE) ? "mark" : "unmark");
      fflush(stderr);
    }

    return size;
  }
  next = cur;
  cur = prev;
  headerp = getHeaderp (cur);
  header = *headerp;
  splitHeader (s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  /* It's impossible to get a WEAK_TAG here, since we would never
   * follow the weak object pointer.
   */
  assert (WEAK_TAG != tag);
  if (NORMAL_TAG == tag) {
    todo = cur + bytesNonObjptrs;
    objptrIndex = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += objptrIndex * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    LOG(LM_DFS_MARK, LL_DEBUG, "ascend(%p)@%d", ((void*)(todo)), __LINE__);
    /* Call the ascendHook now that we are done with 'cur' */
    ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
    if (DEBUG_DFS_MARK) {
      fprintf(stderr,
              "[%d] dfsMarkByMode: ret/NORMAL_TAG: ascendHook(todo = %p), "
              "*todo = %p\n",
              processor,
              ((void*)(todo)),
              *((void**)(todo)));
      fflush(stderr);
    }
    goto markNextInNormal;
  } else if (ARRAY_TAG == tag) {
    arrayIndex = getArrayCounter (cur);
    todo = cur + arrayIndex * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    objptrIndex = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += bytesNonObjptrs + objptrIndex * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    /* Call the ascendHook now that we are done with 'cur' */
    ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
    if (DEBUG_DFS_MARK) {
      fprintf(stderr,
              "[%d] dfsMarkByMode: ret/ARRAY_TAG: ascendHook(todo = %p), "
              "*todo = %p\n",
              processor,
              ((void*)(todo)),
              *((void**)(todo)));
      fflush(stderr);
    }
    goto markNextInArray;
  } else {
    assert (STACK_TAG == tag);
    objptrIndex = ((GC_stack)cur)->markIndex;
    top = ((GC_stack)cur)->markTop;
    /* Invariant: top points just past a "return address". */
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    todo = top - frameLayout->size + frameOffsets [objptrIndex + 1];
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    /* Call the ascendHook now that we are done with 'cur' */
    ascendHook(s, ascendHookArgs, ((objptr*)(todo)));
    if (DEBUG_DFS_MARK) {
      fprintf(stderr,
              "[%d] dfsMarkByMode: ret/STACK_TAG: ascendHook(todo = %p), "
              "*todo = %p\n",
              processor,
              ((void*)(todo)),
              *((void**)(todo)));
      fflush(stderr);
    }
    objptrIndex++;
    goto markInFrame;
  }
  assert (FALSE);
}

static size_t dfsMarkByMode (GC_state s, pointer root,
                             GC_markMode mode,
                             bool shouldHashCons,
                             bool shouldLinkWeaks) {
  return dfsMarkByModeCustom(s,
                             root,
                             mode,
                             shouldHashCons,
                             shouldLinkWeaks,
                             noopDescendHook,
                             NULL,
                             noopAscendHook,
                             NULL);
}

void dfsMarkWithHashConsWithLinkWeaks (GC_state s, objptr *opp, void* ignored) {
  pointer p;

  /* silence compiler warning */
  ((void)(ignored));

  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, MARK_MODE, TRUE, TRUE);
}

void dfsMarkWithoutHashConsWithLinkWeaks (GC_state s,
                                          objptr *opp,
                                          void* ignored) {
  pointer p;

  /* silence compiler warning */
  ((void)(ignored));

  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, MARK_MODE, FALSE, TRUE);
}

bool noopDescendHook(GC_state s, void* rawArgs, objptr object) {
  // silence compiler about unused variables
  ((void)(s));
  ((void)(rawArgs));
  ((void)(object));

  return TRUE;
}
void noopAscendHook(GC_state s, void* rawArgs, objptr* objectObjptr) {
  // silence compiler about unused variables
  ((void)(s));
  ((void)(rawArgs));
  ((void)(objectObjptr));

  return;
}
