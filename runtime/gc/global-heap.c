/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file global-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the Global Heap interface defined in
 * global-heap.h.
 */

/******************************/
/* Static Function Prototypes */
/******************************/
static void HeapManagement_debugMessage (GC_state s, const char* format, ...)
    __attribute__((format (printf, 2, 3)));

/************************/
/* Function Definitions */
/************************/
void GC_enterGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  HeapManagement_debugMessage(s, "--> GLOBAL HEAP\n");
}

void GC_exitGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  HeapManagement_debugMessage(s, "<-- GLOBAL HEAP\n");
}

void HeapManagement_debugMessage (GC_state s, const char* format, ...) {
  if (DEBUG_HEAP_MANAGEMENT or s->controls->heapManagementMessages) {
    va_list substitutions;
    va_start(substitutions, format);
    vfprintf (stderr, format, substitutions);
    va_end(substitutions);
  }
}
