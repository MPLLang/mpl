/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/******************************/
/* Static Function Prototypes */
/******************************/
void outputCollectionStatisticsJSON(FILE* out,
                                    const char *type,
                                    struct rusage *ru,
                                    uintmax_t num,
                                    uintmax_t bytes);

void outputSyncStatisticsJSON(FILE* out,
                              const char* type,
                              uintmax_t num);

/************************/
/* Function Definitions */
/************************/

struct GC_globalCumulativeStatistics* newGlobalCumulativeStatistics(void) {
  struct GC_globalCumulativeStatistics* stats;

  stats = malloc(sizeof(struct GC_globalCumulativeStatistics));
  stats->maxHeapOccupancy = 0;

  return stats;
}

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
  cumulativeStatistics->maxHHLCS = 0;
  cumulativeStatistics->maxHHLCHS = 0;
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

void S_outputCumulativeStatisticsJSON(
    FILE* out, struct GC_cumulativeStatistics* statistics) {
  uintmax_t gcTime;
  uintmax_t syncTime;
  uintmax_t critTime;
  uintmax_t bspTime;

  gcTime = rusageTime (&statistics->ru_gc);
  syncTime = rusageTime (&statistics->ru_sync);
  critTime = rusageTime (&statistics->ru_crit);
  bspTime = rusageTime (&statistics->ru_bsp);

  fprintf(out, "{ ");
  {
    fprintf(out, "\"gcStats\" : ");
    fprintf(out, "[");
    {
      outputCollectionStatisticsJSON(out,
                                     "Copying",
                                     &statistics->ru_gcCopying,
                                     statistics->numCopyingGCs,
                                     statistics->bytesCopied);

      fprintf(out, ", ");

      outputCollectionStatisticsJSON(out,
                                     "Mark-Compact",
                                     &statistics->ru_gcMarkCompact,
                                     statistics->numMarkCompactGCs,
                                     statistics->bytesMarkCompacted);

      fprintf(out, ", ");

      outputCollectionStatisticsJSON(out,
                                     "Generational Minor",
                                     &statistics->ru_gcMinor,
                                     statistics->numMinorGCs,
                                     statistics->bytesCopiedMinor);

      fprintf(out, ", ");

      outputCollectionStatisticsJSON(out,
                                     "Hierarchical Heap Local",
                                     &statistics->ru_gcHHLocal,
                                     statistics->numHHLocalGCs,
                                     statistics->bytesHHLocaled);
    }
    fprintf(out, "]");

    fprintf(out, ", ");

    fprintf(out, "\"gcTime\" : %"PRIuMAX, gcTime);

    fprintf(out, ", ");

    fprintf(out, "\"syncTime\" : %"PRIuMAX, syncTime);

    fprintf(out, ", ");

    fprintf(out, "\"syncStats\" : ");
    fprintf(out, "[");
    {
      outputSyncStatisticsJSON(out,
                               "Old Generation Array",
                               statistics->syncForOldGenArray);

      fprintf(out, ", ");

      outputSyncStatisticsJSON(out,
                               "New Generation Array",
                               statistics->syncForNewGenArray);

      fprintf(out, ", ");

      outputSyncStatisticsJSON(out,
                               "Stack",
                               statistics->syncForStack);

      fprintf(out, ", ");

      outputSyncStatisticsJSON(out,
                               "Heap",
                               statistics->syncForHeap);

      fprintf(out, ", ");

      outputSyncStatisticsJSON(out,
                               "Miscellaneous",
                               statistics->syncMisc);
    }
    fprintf(out, "]");

    fprintf(out, ", ");

    fprintf(out, "\"critTime\" : %"PRIuMAX, critTime);

    fprintf(out, ", ");

    fprintf(out, "\"bspTime\" : %"PRIuMAX, bspTime);

    fprintf(out, ", ");

    fprintf(out, "\"maxPauseTime\" : %"PRIuMAX, statistics->maxPauseTime);

    fprintf(out, ", ");

    fprintf(out, "\"bytesAllocated\" : %"PRIuMAX, statistics->bytesAllocated);

    fprintf(out, ", ");

    fprintf(out, "\"maxGlobalHeapBytesLive\" : %"PRIuMAX, statistics->maxBytesLive);

    fprintf(out, ", ");

    fprintf(out, "\"maxGlobalHeapSize\" : %"PRIuMAX, statistics->maxHeapSize);

    fprintf(out, ", ");

    fprintf(out,
            "\"maxHHLCS\" : %"PRIuMAX,
            statistics->maxHHLCS);

    fprintf(out, ", ");

    fprintf(out,
            "\"maxHHLCHS\" : %"PRIuMAX,
            statistics->maxHHLCHS);

    fprintf(out, ", ");

    fprintf(out, "\"maxStackSize\" : %"PRIuMAX, statistics->maxStackSize);

    fprintf(out, ", ");

    fprintf(out, "\"numCardsMarked\" : %"PRIuMAX, statistics->numCardsMarked);

    fprintf(out, ", ");

    fprintf(out,
            "\"bytesScannedMinor\" : %"PRIuMAX,
            statistics->bytesScannedMinor);

    fprintf(out, ", ");

    fprintf(out, "\"bytesHashConsed\" : %"PRIuMAX, statistics->bytesHashConsed);
  }
  fprintf(out, " }");
}

/*******************************/
/* Static Function Definitions */
/*******************************/

void outputCollectionStatisticsJSON(FILE* out,
                                    const char *type,
                                    struct rusage *ru,
                                    uintmax_t num,
                                    uintmax_t bytes) {
  uintmax_t ms = rusageTime(ru);

  fprintf(out, "{ ");
  {
    fprintf(out, "\"type\" : \"%s\"", type);

    fprintf(out, ", ");

    fprintf(out, "\"time\" : %"PRIuMAX, ms);

    fprintf(out, ", ");

    fprintf(out, "\"number\" : %"PRIuMAX, num);

    fprintf(out, ", ");

    fprintf(out, "\"bytes\" : %"PRIuMAX, bytes);
  }
  fprintf(out, " }");
}

void outputSyncStatisticsJSON(FILE* out,
                              const char* type,
                              uintmax_t num) {
  fprintf(out, "{ ");
  {
    fprintf(out, "\"type\" : \"%s\"", type);

    fprintf(out, ", ");

    fprintf(out, "\"number\" : %"PRIuMAX, num);
  }
  fprintf(out, " }");
}
