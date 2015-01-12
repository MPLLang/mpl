/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file local-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the local heap interface defined in
 * local-heap.h.
 */

#include "local-heap.h"

#include "heap-utils.h"

/******************************/
/* Static Function Prototypes */
/******************************/
static pointer HM_calculateLimitFromChunk (void* chunk);

/************************/
/* Function Definitions */
/************************/
void HM_enterLocalHeap (GC_state s) {
#pragma message "Unimplemented!"
}

void HM_exitLocalHeap (GC_state s) {
#pragma message "Unimplemented!"
}
