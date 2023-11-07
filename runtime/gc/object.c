/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

const char * objectTypeTagToString (GC_objectTypeTag tag) {
  switch (tag) {
  case SEQUENCE_TAG:
    return "SEQUENCE";
  case NORMAL_TAG:
    return "NORMAL";
  case STACK_TAG:
    return "STACK";
  case WEAK_TAG:
    return "WEAK";
  default:
    die ("bad GC_objectTypeTag %u", tag);
  }
}

/* getHeaderp (p)
 *
 * Returns a pointer to the header for the object pointed to by p.
 */
GC_header* getHeaderp (pointer p) {
  return (GC_header*)(p
                      - GC_HEADER_SIZE);
}

/* getHeader (p)
 *
 * Returns the header for the object pointed to by p.
 */
GC_header getHeader (pointer p) {
  GC_header h = *(getHeaderp(p));
  return h;
}

GC_header getRacyHeader (pointer ptr) {
  GC_header header = getHeader(ptr);
  while (isFwdHeader(header)) {
    ptr = (pointer) header;
    header = getHeader(ptr);
  }
  return header;
}

/*
 * Build the header for an object, given the index to its type info.
 */
GC_header buildHeaderFromTypeIndex (uint32_t t) {
  assert (t < TWOPOWER (TYPE_INDEX_BITS));
  return 1 | (t << 1);
}

void splitHeader(GC_state s, GC_header header,
                 GC_objectTypeTag *tagRet, bool *hasIdentityRet,
                 uint16_t *bytesNonObjptrsRet, uint16_t *numObjptrsRet) {
  unsigned int objectTypeIndex;
  GC_objectType objectType;
  GC_objectTypeTag tag;
  bool hasIdentity;
  uint16_t bytesNonObjptrs, numObjptrs;

  assert (1 == (header & GC_VALID_HEADER_MASK));
  objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  assert (objectTypeIndex < s->objectTypesLength);
  objectType = &(s->objectTypes[objectTypeIndex]);
  tag = objectType->tag;
  hasIdentity = objectType->hasIdentity;
  bytesNonObjptrs = objectType->bytesNonObjptrs;
  numObjptrs = objectType->numObjptrs;

  if (DEBUG_DETAILED) {
    fprintf (stderr,
             "splitHeader ("FMTHDR")"
             "  objectTypeIndex = %u"
             "  tag = %s"
             "  hasIdentity = %s"
             "  bytesNonObjptrs = %"PRIu16
             "  numObjptrs = %"PRIu16"\n",
             header,
             objectTypeIndex,
             objectTypeTagToString(tag),
             boolToString(hasIdentity),
             bytesNonObjptrs, numObjptrs);
  }

  if (tagRet != NULL)
    *tagRet = tag;
  if (hasIdentityRet != NULL)
    *hasIdentityRet = hasIdentity;
  if (bytesNonObjptrsRet != NULL)
    *bytesNonObjptrsRet = bytesNonObjptrs;
  if (numObjptrsRet != NULL)
    *numObjptrsRet = numObjptrs;
}

static inline bool isMutableH(GC_state s, GC_header header) {
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  bool hasIdentity;
  splitHeader(s, header, &tag, &hasIdentity, &bytesNonObjptrs, &numObjptrs);
  return hasIdentity;
}

static inline bool isMutable(GC_state s, pointer p) {
  GC_header header = getHeader(p);
  return isMutableH(s, header);
}

/* advanceToObjectData (s, p)
 *
 * If p points at the beginning of an object, then advanceToObjectData
 * returns a pointer to the start of the object data.
 */
pointer advanceToObjectData (ARG_USED_FOR_ASSERT GC_state s, pointer p) {
  GC_header header;
  pointer res;

  assert (isFrontierAligned (s, p));
  header = *(GC_header*)p;
  if (0 == header)
    /* Looking at the counter word in a sequence. */
    res = p + GC_SEQUENCE_METADATA_SIZE;
  else
    /* Looking at a header word. */
    res = p + GC_NORMAL_METADATA_SIZE;
  assert (isAligned ((uintptr_t)res, s->alignment));
  if (DEBUG_DETAILED)
    fprintf (stderr, FMTPTR" = advanceToObjectData ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)p);
  return res;
}

/* return the size of the object pointed to by p */
size_t objectSize(GC_state s, pointer p) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  size_t result;

  header = getHeader(p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  if (NORMAL_TAG == tag || WEAK_TAG == tag) {
    result = (size_t)bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  }
  else if (SEQUENCE_TAG == tag) {
    size_t bytesPerElement;
    size_t dataBytes;
    GC_sequenceLength numElements;

    numElements = getSequenceLength(p);
    bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    dataBytes = numElements * bytesPerElement;

    result = alignWithExtra(s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
  }
  else if (STACK_TAG == tag) {
    result = sizeof(struct GC_stack) + ((GC_stack)p)->reserved;
  }
  else {
    DIE("objectSize: cannot handle tag %u", tag);
  }

  return result;
}
