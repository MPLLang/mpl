/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

__attribute__((noreturn))
size_t GC_size (__attribute__((unused)) GC_state s,
                __attribute__((unused)) pointer root)
{
  DIE("GC_size unsupported");
}
