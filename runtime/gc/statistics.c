/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_cumulativeStatistics *newCumulativeStatistics(void) {
  struct GC_cumulativeStatistics *cumulativeStatistics;

  cumulativeStatistics = (struct GC_cumulativeStatistics *)
    malloc (sizeof (struct GC_cumulativeStatistics));
  cumulativeStatistics->bytesAllocated = 0;
  cumulativeStatistics->bytesFilled = 0;
  cumulativeStatistics->bytesCopied = 0;
  cumulativeStatistics->bytesCopiedMinor = 0;
  cumulativeStatistics->bytesHashConsed = 0;
  cumulativeStatistics->bytesMarkCompacted = 0;
  cumulativeStatistics->bytesScannedMinor = 0;
  cumulativeStatistics->bytesHHLocaled = 0;
  cumulativeStatistics->maxBytesLive = 0;
  cumulativeStatistics->maxBytesLiveSinceReset = 0;
  cumulativeStatistics->maxHeapSize = 0;
  cumulativeStatistics->maxPauseTime = 0;
  cumulativeStatistics->maxStackSize = 0;
  cumulativeStatistics->syncForOldGenArray = 0;
  cumulativeStatistics->syncForNewGenArray = 0;
  cumulativeStatistics->syncForStack = 0;
  cumulativeStatistics->syncForHeap = 0;
  cumulativeStatistics->syncMisc = 0;
  cumulativeStatistics->numCardsMarked = 0;
  cumulativeStatistics->numCopyingGCs = 0;
  cumulativeStatistics->numHashConsGCs = 0;
  cumulativeStatistics->numMarkCompactGCs = 0;
  cumulativeStatistics->numMinorGCs = 0;
  cumulativeStatistics->numHHLocalGCs = 0;
  rusageZero (&cumulativeStatistics->ru_gc);
  rusageZero (&cumulativeStatistics->ru_gcCopying);
  rusageZero (&cumulativeStatistics->ru_gcMarkCompact);
  rusageZero (&cumulativeStatistics->ru_gcMinor);
  rusageZero (&cumulativeStatistics->ru_gcHHLocal);
  rusageZero (&cumulativeStatistics->ru_crit);
  rusageZero (&cumulativeStatistics->ru_sync);
  rusageZero (&cumulativeStatistics->ru_bsp);

  return cumulativeStatistics;
}

struct GC_lastMajorStatistics *newLastMajorStatistics(void) {
  struct GC_lastMajorStatistics *lastMajorStatistics;

  lastMajorStatistics = (struct GC_lastMajorStatistics *)
    malloc (sizeof (struct GC_lastMajorStatistics));
  lastMajorStatistics->bytesHashConsed = 0;
  lastMajorStatistics->bytesLive = 0;
  /* RAM_NOTE: Does this need to be under an option? */
  lastMajorStatistics->kind = GC_COPYING;
  lastMajorStatistics->numMinorGCs = 0;

  return lastMajorStatistics;
}
