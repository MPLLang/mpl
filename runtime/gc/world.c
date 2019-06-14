/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

__attribute__((noreturn))
void GC_saveWorld (__attribute__((unused)) GC_state s,
                   __attribute__((unused)) NullString8_t fileName)
{
  DIE("GC_saveWorld is unsupported");
}

C_Errno_t(Bool_t) GC_getSaveWorldStatus (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (Bool_t)(s->saveWorldStatus);
}
