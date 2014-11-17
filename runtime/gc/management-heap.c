/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file management-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the Management Heap interface defined in
 * management-heap.h.
 */

void GC_enterManagementHeap (void) {
  fprintf (stderr, "--> MANAGEMENT HEAP\n");
}

void GC_exitManagementHeap (void) {
  fprintf (stderr, "<-- MANAGEMENT HEAP\n");
}
