/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* getFwdPtrp (p)
 *
 * Returns a pointer to the forwarding pointer for the object pointed to by p.
 */
objptr* getFwdPtrp (pointer p) {
  return (objptr*)(getHeaderp(p));
}

/* getFwdPtr (p)
 *
 * Returns the forwarding pointer for the object pointed to by p.
 */
objptr getFwdPtr (pointer p) {
  assert(hasFwdPtr(p));
  assert(isObjptr(*(getFwdPtrp(p))));
  return *(getFwdPtrp(p));
}

bool isFwdHeader (GC_header h) {
  return (not (GC_VALID_HEADER_MASK & h));
}

/* hasFwdPtr (p)
 *
 * Returns true if the object pointed to by p has a valid forwarding pointer.
 */
bool hasFwdPtr (pointer p) {
  return isFwdHeader (getHeader(p));
}
