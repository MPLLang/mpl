/* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "util.h"

void die (const char *fmt, ...) {
  va_list args;

  fflush(stdout);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, "\n");
  abort();
}

void diee (const char * fmt, ...) {
  va_list args;

  fflush(stdout);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  fprintf(stderr, " (%s)\n", strerror(errno));
  abort();
}
