/* Copyright (C) 2004-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "platform.h"

Bool MLton_Platform_CygwinUseMmap = TRUE;

void GC_setCygwinUseMmap (bool b) {
  MLton_Platform_CygwinUseMmap = b;
}

void MLton_init (int argc, char **argv, GC_state s) {
  int start;

  Posix_ProcEnv_environ = (C_StringArray_t)environ;
  start = GC_init (s, argc, argv);
  /* Setup argv and argc that SML sees. */
  /* start is now the index of the first real arg. */
  CommandLine_commandName = (C_String_t)(argv[0]);
  CommandLine_argc = argc - start;
  CommandLine_argv = (C_StringArray_t)(argv + start);
}

void MLton_halt (GC_state s, C_Int_t status) {
  if (!GC_TryToTerminate(s))
    pthread_exit(NULL);

  GC_done (s);

  /* Since we are going to call finish, pthread destructors will not run and we
   * need to flush our trace buffer manually. */
  GC_traceFinish(s);

  exit (status);
}

void MLton_heapCheckTooLarge (void) {
  die ("Out of memory.  Unable to check heap for more than %"PRIuMAX" bytes.\n",
       (uintmax_t)SIZE_MAX);
}
