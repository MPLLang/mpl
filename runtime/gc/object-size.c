/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofArrayNoMetaData (GC_state s,
                              GC_arrayLength numElements,
                              uint16_t bytesNonObjptrs, uint16_t numObjptrs) {
  size_t result;

  result = numElements * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
  return alignWithExtra (s, result, GC_ARRAY_METADATA_SIZE);
}

size_t sizeofStackNoMetaData (__attribute__ ((unused)) GC_state s,
                              GC_stack stack) {
  size_t result;

  result = sizeof (struct GC_stack) + stack->reserved;
  return result;
}

size_t sizeofObject (GC_state s, pointer p) {
  size_t metaDataBytes, objectBytes;
  GC_header header;
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs, numObjptrs;

  header = getHeader (p);
  splitHeader (s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { 
    metaDataBytes = GC_NORMAL_METADATA_SIZE;
    objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  } else if (ARRAY_TAG == tag) {
    metaDataBytes = GC_ARRAY_METADATA_SIZE;
    objectBytes = sizeofArrayNoMetaData (s, getArrayLength (p),
                                         bytesNonObjptrs, numObjptrs);
  } else if (STACK_TAG == tag) {
    metaDataBytes = GC_STACK_METADATA_SIZE;
    objectBytes = sizeofStackNoMetaData (s, (GC_stack)p);
  }
  else if (HEADER_ONLY_TAG == tag) {
    metaDataBytes = GC_HEADER_SIZE;
    objectBytes = 0;
  }
  else if (FILL_TAG == tag) {
    GC_smallGapSize bytes;
    metaDataBytes = GC_HEADER_SIZE;
    bytes = *((GC_smallGapSize *)p);
    objectBytes = GC_SMALL_GAP_SIZE_SIZE + bytes;
  }
  else {
    metaDataBytes = 0;
    objectBytes = 0;
    assert (0 and "unknown tag in sizeofObject");
  }
  return metaDataBytes + objectBytes;
}
