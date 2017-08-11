/* Copyright (C) 2012,2015 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static void displayCol (FILE *out, size_t width, const char *s) {
  size_t extra;
  size_t i;
  size_t len;

  len = strlen (s);
  if (len < width) {
    extra = width - len;
    for (i = 0; i < extra; i++)
      fprintf (out, " ");
  }
  fprintf (out, "%s\t", s);
}

static void displayCollectionStats (FILE *out, const char *name, struct rusage *ru,
                                    uintmax_t num, uintmax_t bytes) {
  uintmax_t ms;

  ms = rusageTime (ru);
  fprintf (out, "%s", name);
  displayCol (out, 7, uintmaxToCommaString (ms));
  displayCol (out, 7, uintmaxToCommaString (num));
  displayCol (out, 15, uintmaxToCommaString (bytes));
  displayCol (out, 15,
              (ms > 0)
              ? uintmaxToCommaString ((uintmax_t)(1000.0 * (float)bytes/(float)ms))
              : "-");
  fprintf (out, "\n");
}

static void displayGlobalCumulativeStatistics (
    FILE *out,
    struct GC_globalCumulativeStatistics* globalCumulativeStatistics) {
    fprintf (out, "max global heap occupancy: %s bytes\n",
           uintmaxToCommaString (globalCumulativeStatistics->maxHeapOccupancy));
    fprintf (out, "max chunk pool occupancy: %s bytes\n",
             uintmaxToCommaString (ChunkPool_maxAllocated ()));
}


static void displayCumulativeStatistics (FILE *out, struct GC_cumulativeStatistics *cumulativeStatistics) {
  struct rusage ru_total;
  uintmax_t totalTime;
  uintmax_t gcTime;
  uintmax_t syncTime;
  uintmax_t critTime;
  uintmax_t bspTime;

  getrusage (RUSAGE_THREAD, &ru_total);
  totalTime = rusageTime (&ru_total);
  gcTime = rusageTime (&cumulativeStatistics->ru_gc);
  syncTime = rusageTime (&cumulativeStatistics->ru_sync);
  critTime = rusageTime (&cumulativeStatistics->ru_crit);
  bspTime = rusageTime (&cumulativeStatistics->ru_bsp);
  fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
  fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
  displayCollectionStats
    (out, "copying\t\t",
     &cumulativeStatistics->ru_gcCopying,
     cumulativeStatistics->numCopyingGCs,
     cumulativeStatistics->bytesCopied);
  displayCollectionStats
    (out, "mark-compact\t",
     &cumulativeStatistics->ru_gcMarkCompact,
     cumulativeStatistics->numMarkCompactGCs,
     cumulativeStatistics->bytesMarkCompacted);
  displayCollectionStats
    (out, "minor\t\t",
     &cumulativeStatistics->ru_gcMinor,
     cumulativeStatistics->numMinorGCs,
     cumulativeStatistics->bytesCopiedMinor);
  displayCollectionStats
      (out, "HHLocal\t\t",
       &cumulativeStatistics->ru_gcHHLocal,
       cumulativeStatistics->numHHLocalGCs,
       cumulativeStatistics->bytesHHLocaled);
  fprintf (out, "total time: %s ms\n",
           uintmaxToCommaString (totalTime));
  fprintf (out, "total GC time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (gcTime),
           (0 == totalTime) ?
           0.0 : 100.0 * ((double) gcTime) / (double)totalTime);
  fprintf (out, "total sync time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (syncTime),
           (0 == totalTime) ?
           0.0 : 100.0 * ((double) syncTime) / (double)totalTime);
  fprintf (out, "total crit time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (critTime),
           (0 == totalTime) ?
           0.0 : 100.0 * ((double) critTime) / (double)totalTime);
  fprintf (out, "total bsp time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (bspTime),
           (0 == totalTime) ?
           0.0 : 100.0 * ((double) bspTime) / (double)totalTime);
  fprintf (out, "max pause time: %s ms\n",
           uintmaxToCommaString (cumulativeStatistics->maxPauseTime));
  fprintf (out, "total bytes allocated: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->bytesAllocated));
  fprintf (out, "max global heap bytes live: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->maxBytesLive));
  fprintf (out, "max global heap size: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->maxHeapSize));
  fprintf (out, "max hierarchical heap LC size: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->maxHHLCS));
  fprintf (out, "max hierarchical heap LC heap size: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->maxHHLCHS));
  fprintf (out, "max stack size: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->maxStackSize));
  fprintf (out, "num cards marked: %s\n",
           uintmaxToCommaString (cumulativeStatistics->numCardsMarked));
  fprintf (out, "bytes scanned: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->bytesScannedMinor));
  fprintf (out, "bytes hash consed: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->bytesHashConsed));
  fprintf (out, "sync for old gen array: %s\n",
           uintmaxToCommaString (cumulativeStatistics->syncForOldGenArray));
  fprintf (out, "sync for new gen array: %s\n",
           uintmaxToCommaString (cumulativeStatistics->syncForNewGenArray));
  fprintf (out, "sync for stack: %s\n",
           uintmaxToCommaString (cumulativeStatistics->syncForStack));
  fprintf (out, "sync for heap: %s\n",
           uintmaxToCommaString (cumulativeStatistics->syncForHeap));
  fprintf (out, "sync misc: %s\n",
           uintmaxToCommaString (cumulativeStatistics->syncMisc));
}

static void displayCumulativeStatisticsJSON (FILE *out, GC_state s) {
  struct rusage ru_total;
  uintmax_t totalTime;

  getrusage (RUSAGE_THREAD, &ru_total);
  totalTime = rusageTime (&ru_total);

  fprintf(out, "{ ");

  {
    /* Print per-thread statistics */
    fprintf(out, "\"perThread\" : ");
    fprintf(out, "[");
    {
      if (s->procStates) {
        /* print cumulativeStatistics for each processor, separated by commas */
        uint32_t proc;
        for (proc = 0; proc < s->numberOfProcs - 1; proc++) {
          S_outputCumulativeStatisticsJSON(
              out, s->procStates[proc].cumulativeStatistics);
          fprintf(out, ", ");
        }
        S_outputCumulativeStatisticsJSON(
            out, s->procStates[proc].cumulativeStatistics);
      } else {
        S_outputCumulativeStatisticsJSON(out, s->cumulativeStatistics);
      }
    }
    fprintf(out, "]");

    fprintf(out, ", ");

    /* print global statistics */
    fprintf(out, "\"totalTime\" : %"PRIuMAX, totalTime);

    fprintf(out, ", ");

    fprintf(out,
            "\"maxGlobalHeapOccupancy\" : %"PRIuMAX,
            s->globalCumulativeStatistics->maxHeapOccupancy);

    fprintf(out, ", ");

    fprintf(out,
            "\"maxChunkPoolOccupancy\" : %"PRIuMAX,
            ChunkPool_maxAllocated ());
  }

  fprintf(out, " }");
}

void GC_done (GC_state s) {
  GC_PthreadAtExit(s);
  minorGC (s);

  if (s->controls->summary) {
    if (HUMAN == s->controls->summaryFormat) {
      fprintf (s->controls->summaryFile, "Global::\n");
      displayGlobalCumulativeStatistics
              (s->controls->summaryFile,
               s->globalCumulativeStatistics);
      if (s->procStates) {
        for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
          fprintf (s->controls->summaryFile, "Thread [%d]::\n", proc);
          displayCumulativeStatistics
              (s->controls->summaryFile,
               s->procStates[proc].cumulativeStatistics);
        }
      } else {
        displayCumulativeStatistics(s->controls->summaryFile,
                                    s->cumulativeStatistics);
      }
    } else if (JSON == s->controls->summaryFormat) {
      displayCumulativeStatisticsJSON(s->controls->summaryFile, s);
      fprintf(s->controls->summaryFile, "\n");
    }
  }
  releaseHeap(s, s->heap);
  releaseHeap(s, s->secondaryHeap);
}
