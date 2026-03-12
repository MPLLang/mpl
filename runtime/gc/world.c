/* Copyright (C) 2019,2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

[[noreturn]]
void GC_saveWorld ([[maybe_unused]] GC_state s,
                   [[maybe_unused]] NullString8_t fileName)
{
  DIE("GC_saveWorld is unsupported");
}

C_Errno_t(Bool_t) GC_getSaveWorldStatus (GC_state s) {
  return (Bool_t)(s->saveWorldStatus);
}
