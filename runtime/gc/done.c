/* Copyright (C) 2012,2015 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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
    // SAM_NOTE: TODO: removed for now; will need to replace with blocks statistics
    // fprintf (out, "max chunk pool occupancy: %s bytes\n",
    //          uintmaxToCommaString (ChunkPool_maxAllocated ()));
}

static void displayHHAllocStats(FILE *out, GC_state s) {
  FixedSizeAllocator fsa = getHHAllocator(s);
  fprintf(out, "num hh allocated: %zu\n", numFixedSizeAllocated(fsa));
  fprintf(out, "num hh freed: %zu\n", numFixedSizeFreed(fsa));
  fprintf(out, "num hh shared freed: %zu\n", numFixedSizeSharedFreed(fsa));
  fprintf(out, "num hh currently in use: %zu\n", numFixedSizeCurrentlyInUse(fsa));
  fprintf(out, "current hh alloc capacity: %zu\n", currentFixedSizeCapacity(fsa));
  fprintf(out, "current hh space util: %.1f%%\n",
          100.0 * currentFixedSizeSpaceUtilization(fsa));

  size_t maxSize = 0;
  size_t maxHeight = 0;
  for (HM_HierarchicalHeap cursor = getHierarchicalHeapCurrent(s);
       cursor != NULL;
       cursor = cursor->nextAncestor)
  {
    if (cursor->numDependants > maxSize)
      maxSize = cursor->numDependants;
    if (cursor->heightDependants > maxHeight) maxHeight =
      cursor->heightDependants;
  }

  fprintf(out, "current max num hh dependants: %zu\n", maxSize);
  fprintf(out, "current max dependant tree height: %zu\n", maxHeight);
}


static void displayCumulativeStatistics (FILE *out, struct GC_cumulativeStatistics *cumulativeStatistics) {
  struct rusage ru_total;
  uintmax_t totalTime;
  uintmax_t gcTime;
  uintmax_t syncTime;
  uintmax_t critTime;
  uintmax_t bspTime;

#ifdef __MACH__
  getrusage_thread (&ru_total);
#else
  getrusage (RUSAGE_THREAD, &ru_total);
#endif
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
  fprintf (out, "total bytes promoted: %s bytes\n",
           uintmaxToCommaString (cumulativeStatistics->bytesPromoted));
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

#ifdef __MACH__
  getrusage_thread (&ru_total);
#else
  getrusage (RUSAGE_THREAD, &ru_total);
#endif
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

    // SAM_NOTE: TODO: removed for now; will need to replace with blocks statistics
    // fprintf(out,
    //         "\"maxChunkPoolOccupancy\" : %"PRIuMAX,
    //         ChunkPool_maxAllocated ());
  }

  fprintf(out, " }");
}


void displayHeartbeatStatistics(FILE *out, GC_state s) {
  TimeHistogram signalsH = s->cumulativeStatistics->heartbeatSignals;
  double signalsDist[TimeHistogram_numBuckets(signalsH)];
  size_t totalS = TimeHistogram_reportDistribution(signalsH, signalsDist);

  TimeHistogram handlersH = s->cumulativeStatistics->heartbeatHandlers;
  double handlersDist[TimeHistogram_numBuckets(handlersH)];
  size_t totalH = TimeHistogram_reportDistribution(handlersH, handlersDist);

  fprintf(out, "              ");
  fprintf(out, "  count ");
  for (size_t bucket = 0; bucket < TimeHistogram_numBuckets(signalsH); bucket++) {
    fprintf(out, "%*zu ", 3, bucket);
  }
  fprintf(out, "\n");

  fprintf(out, "[%*d] signals  ", 2, Proc_processorNumber(s));
  fprintf(out, "%*zu ", 7, totalS); 
  for (size_t bucket = 0; bucket < TimeHistogram_numBuckets(signalsH); bucket++) {
    fprintf(out, "%*d%% ", 2, (int)(100.0 * signalsDist[bucket]));
  }
  fprintf(out, "\n");

  fprintf(out, "[%*d] handlers ", 2, Proc_processorNumber(s));
  fprintf(out, "%*zu ", 7, totalH);
  for (size_t bucket = 0; bucket < TimeHistogram_numBuckets(handlersH); bucket++) {
    fprintf(out, "%*d%% ", 2, (int)(100.0 * handlersDist[bucket]));
  }
  fprintf(out, "\n");
}

void GC_done(GC_state s) {
  GC_PthreadAtExit(s);

  if (s->controls->heartbeatStats) {
    for (uint32_t proc = 0; proc < s->numberOfProcs; proc++) {
      displayHeartbeatStatistics(s->controls->summaryFile, &(s->procStates[proc]));
    }
  }

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
          displayHHAllocStats(s->controls->summaryFile, &(s->procStates[proc]));
        }
      } else {
        displayCumulativeStatistics(s->controls->summaryFile,
                                    s->cumulativeStatistics);
        displayHHAllocStats(s->controls->summaryFile, s);
      }
    } else if (JSON == s->controls->summaryFormat) {
      displayCumulativeStatisticsJSON(s->controls->summaryFile, s);
      fprintf(s->controls->summaryFile, "\n");
    }
  }
}
