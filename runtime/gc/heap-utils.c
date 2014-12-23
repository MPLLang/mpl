/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file heap-utils.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the HeapManagement utility functions described in
 * heap-utils.h
 */

void HeapManagement_debugMessage (GC_state s, const char* format, ...) {
  if (DEBUG_HEAP_MANAGEMENT or s->controls->heapManagementMessages) {
    va_list substitutions;
    va_start(substitutions, format);
    vfprintf (stderr, format, substitutions);
    va_end(substitutions);
  }
}
