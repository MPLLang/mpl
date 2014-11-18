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

void GC_enterGlobalHeap (void) {
  fprintf (stderr, "--> GLOBAL HEAP\n");
}

void GC_exitGlobalHeap (void) {
  fprintf (stderr, "<-- GLOBAL HEAP\n");
}
