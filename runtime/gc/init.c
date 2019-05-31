/* Copyright (C) 2009,2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

#include <stdlib.h>
#include <strings.h>

static bool stringToBool (char *s) {
  if (0 == strcmp (s, "false"))
    return FALSE;
  if (0 == strcmp (s, "true"))
    return TRUE;
  die ("Invalid @MLton bool: %s.", s);
}

// From gdtoa/gdtoa.h.
// Can't include the whole thing because it brings in too much junk.
float gdtoa__strtof (const char *, char **);

static float stringToFloat (char *s) {
  char *endptr;
  float f;

  f = gdtoa__strtof (s, &endptr);
  if (s == endptr)
    die ("Invalid @MLton float: %s.", s);
  return f;
}

static int32_t stringToInt (char *s) {
  char *endptr;
  int i;

  i = strtol (s, &endptr, 10);
  if (s == endptr)
    die ("Invalid @MLton int: %s.", s);
  return i;
}

static size_t stringToBytes (const char *s) {
  double d;
  char *endptr;
  size_t factor;

  d = strtod (s, &endptr);
  if (s == endptr)
    goto bad;
  switch (*endptr++) {
  case 'g':
  case 'G':
    factor = 1024 * 1024 * 1024;
    break;
  case 'k':
  case 'K':
    factor = 1024;
    break;
  case 'm':
  case 'M':
    factor = 1024 * 1024;
    break;
  default:
    goto bad;
  }
  d *= factor;
  unless (*endptr == '\0'
          and 0.0 <= d
          and d <= (double)SIZE_MAX)
    goto bad;
  return (size_t)d;
bad:
  die ("Invalid @MLton memory amount: %s.", s);
}

/* ---------------------------------------------------------------- */
/*                             GC_init                              */
/* ---------------------------------------------------------------- */

int processAtMLton (GC_state s, int start, int argc, char **argv,
                    char **worldFile) {
  int i;

  i = start;
  while (s->controls->mayProcessAtMLton
         and i < argc
         and (0 == strcmp (argv [i], "@MLton"))) {
    bool done;

    i++;
    done = FALSE;
    while (!done) {
      if (i == argc)
        die ("Missing -- at end of @MLton args.");
      else {
        char *arg;

        arg = argv[i];
        if (0 == strcmp (arg, "copy-generational-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton copy-generational-ratio missing argument.");
          s->controls->ratios.copyGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.copyGenerational)
            die ("@MLton copy-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "copy-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton copy-ratio missing argument.");
          s->controls->ratios.copy = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.copy)
            die ("@MLton copy-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "gc-messages")) {
          i++;
          s->controls->messages = TRUE;
        } else if (0 == strcmp (arg, "hm-messages")) {
          i++;
          s->controls->HMMessages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          i++;
          s->controls->summary = TRUE;
        } else if (0 == strcmp (arg, "gc-summary-file")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("@MLton gc-summary-file missing argument.");
          const char* filePath = argv[i++];
          if (0 == strcmp(filePath, "-")) {
            s->controls->summaryFile = stdout;
          } else {
            s->controls->summaryFile = fopen(filePath, "w");
            if (s->controls->summaryFile == NULL) {
              die ("Invalid @MLton gc-summary-file %s (%s).", filePath, strerror(errno));
            }
          }
        } else if (0 == strcmp (arg, "gc-summary-format")) {
          i++;

          if (i == argc) {
            die ("@MLton gc-summary-format missing argument.");
          }

          const char* format = argv[i++];
          if (0 == strcmp (format, "human")) {
            s->controls->summaryFormat = HUMAN;
          } else if (0 == strcmp (format, "json")) {
            s->controls->summaryFormat = JSON;
          } else {
            die ("@MLton gc-summary-format \"%s\" invalid. Must be one of "
                 "human or json.",
                 format);
          }
        } else if (0 == strcmp(arg, "gc-summary-file")) {
          i++;
          if (i == argc) {
            die("@MLton gc-summary-file missing argument.");
          }

          const char* filePath = argv[i++];
          if (0 == strcmp(filePath, "-")) {
            s->controls->summaryFile = stdout;
          } else {
            FILE* file = fopen(filePath, "w");
            if (NULL == file) {
              diee("@MLton Could not open specified GC summary file.");
            } else {
              s->controls->summaryFile = file;
            }
          }
        } else if (0 == strcmp (arg, "set-affinity")) {
          i++;
          s->controls->setAffinity = TRUE;
        } else if (0 == strcmp (arg, "affinity-base")) {
          i++;
          if (i == argc)
            die ("@MLton affinity-base missing argument.");
          s->controls->affinityBase = stringToInt (argv[i++]);
        } else if (0 == strcmp (arg, "affinity-stride")) {
          i++;
          if (i == argc)
            die ("@MLton affinity-stride missing argument.");
          s->controls->affinityStride = stringToInt (argv[i++]);
        } else if (0 == strcmp (arg, "available-ratio")) {
          i++;
          if (i == argc)
            die ("@MLton available-ratio missing argument.");
          s->controls->ratios.available = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.available)
            die ("@MLton available-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "grow-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton grow-ratio missing argument.");
          s->controls->ratios.grow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.grow)
            die ("@MLton grow-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "hash-cons")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton hash-cons missing argument.");
          s->controls->ratios.hashCons = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.hashCons
                  and s->controls->ratios.hashCons <= 1.0)
            die ("@MLton hash-cons argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "live-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton live-ratio missing argument.");
          s->controls->ratios.live = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.live)
            die ("@MLton live-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls->mayLoadWorld)
            die ("May not load world.");
          i++;
          s->amOriginal = FALSE;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton load-world missing argument.");
          *worldFile = argv[i++];
        } else if (0 == strcmp(arg, "log-file")) {
          i++;
          if (i == argc) {
            die("@MLton log-file missing argument.");
          }

          const char* filePath = argv[i++];
          if (0 == strcmp(filePath, "-")) {
            L_setFile(stdout);
          } else {
            FILE* file = fopen(filePath, "w");
            if (NULL == file) {
              diee("@MLton Could not open specified log file.");
            } else {
              L_setFile(file);
            }
          }
        } else if (0 == strcmp(arg, "log-level")) {
          i++;
          if (i == argc) {
            die("@MLton log-level missing argument.");
          }

          char* levelString = argv[i++];
          if (!initLogLevels(levelString)) {
            die ("@MLton log-level invalid argument");
          }
        } else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton mark-compact-generational-ratio missing argument.");
          s->controls->ratios.markCompactGenerational = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.markCompactGenerational)
            die ("@MLton mark-compact-generational-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "mark-compact-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton mark-compact-ratio missing argument.");
          s->controls->ratios.markCompact = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.markCompact)
            die ("@MLton mark-compact-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "no-load-world")) {
          i++;
          s->controls->mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "nursery-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton nursery-ratio missing argument.");
          s->controls->ratios.nursery = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.nursery)
            die ("@MLton nursery-ratio argument must be greater than 1.0.");
        } else if (0 == strcmp (arg, "ram-slop")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton ram-slop missing argument.");
          s->controls->ratios.ramSlop = stringToFloat (argv[i++]);
        } else if (0 == strcmp (arg, "show-sources")) {
          showSources (s);
          exit (0);
        } else if (0 == strcmp (arg, "stop")) {
          i++;
          s->controls->mayProcessAtMLton = FALSE;
        } else if (0 == strcmp (arg, "stack-current-grow-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-grow-ratio missing argument.");
          s->controls->ratios.stackCurrentGrow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentGrow)
            die ("@MLton stack-current-grow-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-max-reserved-ratio missing argument.");
          s->controls->ratios.stackCurrentMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentMaxReserved)
            die ("@MLton stack-current-max-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-permit-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-permit-reserved-ratio missing argument.");
          s->controls->ratios.stackCurrentPermitReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentPermitReserved)
            die ("@MLton stack-current-permit-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-current-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-current-shrink-ratio missing argument.");
          s->controls->ratios.stackCurrentShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.stackCurrentShrink
                  and s->controls->ratios.stackCurrentShrink <= 1.0)
            die ("@MLton stack-current-shrink-ratio argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "stack-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-max-reserved-ratio missing argument.");
          s->controls->ratios.stackMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackMaxReserved)
            die ("@MLton stack-max-reserved-ratio argument must greater than 1.0.");
        } else if (0 == strcmp (arg, "stack-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton stack-shrink-ratio missing argument.");
          s->controls->ratios.stackShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.stackShrink
                  and s->controls->ratios.stackShrink <= 1.0)
            die ("@MLton stack-shrink-ratio argument must be between 0.0 and 1.0.");
        } else if (0 == strcmp (arg, "use-mmap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("@MLton use-mmap missing argument.");
          GC_setCygwinUseMmap (stringToBool (argv[i++]));
        } else if (0 == strcmp (arg, "number-processors")) {
          i++;
          if (i == argc)
            die ("@MLton number-processors missing argument.");
          if (!s->amOriginal)
            die ("@MLton number-processors incompatible with loaded worlds.");
          s->numberOfProcs = stringToFloat (argv[i++]);
          /* Turn off loaded worlds -- they are unsuppoed in multi-proc mode */
          s->controls->mayLoadWorld = FALSE;
        } else if (0 == strcmp(arg, "min-chunk")) {
          i++;
          if (i == argc) {
            die ("@MLton min-chunk missing argument.");
          }
          s->controls->minChunkSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "alloc-chunk")) {
          i++;
          if (i == argc) {
            die ("@MLton alloc-chunk missing argument.");
          }
          s->controls->allocChunkSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp (arg, "disable-ancestor-chunk-opt")) {
          i++;
          s->controls->mayUseAncestorChunk = FALSE;
        } else if (0 == strcmp(arg, "hh-allocated-ratio")) {
          i++;
          if (i == argc) {
            die ("@MLton hh-allocated-ratio missing argument.");
          }

          s->controls->hhConfig.allocatedRatio = stringToFloat(argv[i++]);
          if (s->controls->hhConfig.allocatedRatio < 2.0) {
            die("@MLton hh-allocated-ratio must be at least 2.0");
          }
        } else if (0 == strcmp (arg, "hh-collection-level")) {
          i++;
          if (i == argc) {
            die ("@MLton hh-collection-level missing argument.");
          }
          const char* level = argv[i++];
          if (0 == strcmp (level, "none")) {
            s->controls->hhCollectionLevel = NONE;
          } else if (0 == strcmp (level, "superlocal")) {
            s->controls->hhCollectionLevel = SUPERLOCAL;
          } else if (0 == strcmp (level, "local")) {
            s->controls->hhCollectionLevel = LOCAL;
          } else {
            die ("@MLton hh-collection-level \"%s\" invalid. Must be one of "
                 "none, superlocal, or local.",
                 level);
          }
        } else if (0 == strcmp(arg, "hh-live-lc-ratio")) {
          i++;
          if (i == argc) {
            die ("@MLton hh-live-lc-ratio missing argument.");
          }

          s->controls->hhConfig.liveLCRatio = stringToFloat(argv[i++]);
          if (s->controls->hhConfig.liveLCRatio < 1.0) {
            die("@MLton hh-live-lc-ratio must be at least 2.0");
          }
        } else if (0 == strcmp(arg, "hh-initial-lc-heap-size")) {
          i++;
          if (i == argc) {
            die ("@MLton hh-initial-lc-heap-size missing argument.");
          }

          s->controls->hhConfig.initialLCHS = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "hh-max-lc-heap-size")) {
          i++;
          if (i == argc) {
            die ("@MLton hh-max-lc-heap-size missing argument.");
          }

          s->controls->hhConfig.maxLCHS = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "trace-buffer-size")) {
          i++;
          if (i == argc) {
            die ("@MLton trace-buffer-size missing argument.");
          }

          s->controls->traceBufferSize = stringToInt(argv[i++]);
        } else if (0 == strcmp(arg, "disable-coalesce")) {
          i++;
          s->controls->freeListCoalesce = FALSE;
        } else if (0 == strcmp(arg, "disable-promotion")) {
          i++;
          s->controls->deferredPromotion = FALSE;
        } else if (0 == strcmp(arg, "use-old-hh-gc-policy")) {
          i++;
          s->controls->oldHHGCPolicy = TRUE;
        } else if (0 == strcmp (arg, "--")) {
          i++;
          done = TRUE;
        } else if (i > 1)
          die ("Strange @MLton arg: %s", argv[i]);
        else done = TRUE;
      }
    }
  }

  return i;
}

int GC_init (GC_state s, int argc, char **argv) {
  int res;

  assert (s->alignment >= GC_MODEL_MINALIGN);
  assert (isAligned (sizeof (struct GC_stack), s->alignment));
  // While the following asserts are manifestly true,
  // they check the asserts in sizeofThread and sizeofWeak.
  assert (sizeofThread (s) == sizeofThread (s));
  assert (sizeofWeak (s) == sizeofWeak (s));

  s->amInGC = TRUE;
  s->amOriginal = TRUE;
  s->atomicState = 0;
  s->callFromCHandlerThread = BOGUS_OBJPTR;

  s->controls = (struct GC_controls *) malloc (sizeof (struct GC_controls));
  s->controls->mayLoadWorld = TRUE;
  s->controls->mayProcessAtMLton = TRUE;
  s->controls->messages = FALSE;
  s->controls->HMMessages = FALSE;
  s->controls->setAffinity = FALSE;
  s->controls->affinityBase = 0;
  s->controls->affinityStride = 1;
  s->controls->ratios.copy = 4.0f;
  s->controls->ratios.copyGenerational = 4.0f;
  s->controls->ratios.grow = 8.0f;
  s->controls->ratios.hashCons = 0.0f;
  s->controls->ratios.live = 8.0f;
  s->controls->ratios.markCompact = 1.04f;
  s->controls->ratios.markCompactGenerational = 8.0f;
  s->controls->ratios.nursery = 10.0f;
  s->controls->ratios.ramSlop = 0.5f;
  s->controls->ratios.stackCurrentGrow = 2.0f;
  s->controls->ratios.stackCurrentMaxReserved = 32.0f;
  s->controls->ratios.stackCurrentPermitReserved = 4.0f;
  s->controls->ratios.stackCurrentShrink = 0.5f;
  s->controls->ratios.stackMaxReserved = 8.0f;
  s->controls->ratios.stackShrink = 0.5f;
  s->controls->hhConfig.allocatedRatio = 2.0; /* RAM_NOTE: Arbitrary! */
  s->controls->hhConfig.liveLCRatio = 8.0; /* RAM_NOTE: Arbitrary! */
  s->controls->hhConfig.initialLCHS = 1 * 1024 * 1024; /* RAM_NOTE: Arbitrary! */
  s->controls->hhConfig.maxLCHS = MAX_LCHS_INFINITE;
  s->controls->rusageMeasureGC = FALSE;
  s->controls->summary = FALSE;
  s->controls->summaryFormat = HUMAN;
  s->controls->summaryFile = stderr;
  s->controls->hhCollectionLevel = ALL;
  s->controls->traceBufferSize = 10000;

  /* Not arbitrary; should be at least the page size and must also respect the
   * limit check coalescing amount in the compiler. */
  s->controls->minChunkSize = max(GC_pageSize(), 4096);
  s->controls->allocChunkSize = 16 * s->controls->minChunkSize;

  s->controls->mayUseAncestorChunk = TRUE;
  s->controls->freeListCoalesce = TRUE;
  s->controls->deferredPromotion = TRUE;
  s->controls->oldHHGCPolicy = FALSE;

  s->globalCumulativeStatistics = newGlobalCumulativeStatistics();
  s->cumulativeStatistics = newCumulativeStatistics();

  s->currentThread = BOGUS_OBJPTR;
  s->wsQueue = BOGUS_OBJPTR;
  s->ffiArgs = NULL;

  s->lastMajorStatistics = newLastMajorStatistics();

  s->numberOfProcs = 1;
  s->procStates = NULL;
  s->roots = NULL;
  s->rootsLength = 0;
  s->savedThread = BOGUS_OBJPTR;

  s->signalHandlerThread = BOGUS_OBJPTR;
  s->signalsInfo.amInSignalHandler = FALSE;
  s->signalsInfo.gcSignalHandled = FALSE;
  s->signalsInfo.gcSignalPending = FALSE;
  s->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&s->signalsInfo.signalsHandled);
  sigemptyset (&s->signalsInfo.signalsPending);
  s->self = pthread_self();
  s->terminationLeader = INVALID_PROCESSOR_NUMBER;
  s->sysvals.pageSize = GC_pageSize ();
  s->sysvals.physMem = GC_physMem ();
  s->weaks = NULL;
  s->saveWorldStatus = true;
  s->trace = NULL;
  srand48_r(0, &(s->tlsObjects.drand48_data));

  /* RAM_NOTE: Why is this not found in the Spoonhower copy? */
  initIntInf (s);
  initSignalStack (s);
  s->worldFile = NULL;

  L_setFile(stderr);
  processAtMLton (s, 0, s->atMLtonsLength, s->atMLtons, &s->worldFile);
  res = processAtMLton (s, 1, argc, argv, &s->worldFile);
  unless (s->controls->ratios.markCompact <= s->controls->ratios.copy
          and s->controls->ratios.copy <= s->controls->ratios.live)
    die ("Ratios must satisfy mark-compact-ratio <= copy-ratio <= live-ratio.");
  unless (s->controls->ratios.stackCurrentPermitReserved
          <= s->controls->ratios.stackCurrentMaxReserved)
    die ("Ratios must satisfy stack-current-permit-reserved <= stack-current-max-reserved.");
  /* We align s->sysvals.ram by s->sysvals.pageSize so that we can
   * test whether or not we we are using mark-compact by comparing
   * heap size to ram size.  If we didn't round, the size might be
   * slightly off.
   */
  uintmax_t ram;
  ram = alignMax ((uintmax_t)(s->controls->ratios.ramSlop * (double)(s->sysvals.physMem)),
                  (uintmax_t)(s->sysvals.pageSize));
  ram = min (ram, alignMaxDown((uintmax_t)SIZE_MAX, (uintmax_t)(s->sysvals.pageSize)));
  s->sysvals.ram = (size_t)ram;
  if (DEBUG or DEBUG_RESIZING or s->controls->messages)
    fprintf (stderr, "[GC: Found %s bytes of RAM; using %s bytes (%.1f%% of RAM).]\n",
             uintmaxToCommaString(s->sysvals.physMem),
             uintmaxToCommaString(s->sysvals.ram),
             100.0 * ((double)ram / (double)(s->sysvals.physMem)));
  if (DEBUG_SOURCES or DEBUG_PROFILE) {
    uint32_t i;
    for (i = 0; i < s->sourceMaps.frameSourcesLength; i++) {
      uint32_t j;
      uint32_t *sourceSeq;
      fprintf (stderr, "%"PRIu32"\n", i);
      sourceSeq = s->sourceMaps.sourceSeqs[s->sourceMaps.frameSources[i]];
      for (j = 1; j <= sourceSeq[0]; j++)
        fprintf (stderr, "\t%s\n",
                 s->sourceMaps.sourceNames[
                 s->sourceMaps.sources[sourceSeq[j]].sourceNameIndex
                 ]);
    }
  }

  unless (isAligned(s->controls->minChunkSize, s->sysvals.pageSize))
    die ("min-chunk must be a multiple of the system page size, %zu", s->sysvals.pageSize);

  unless (isAligned(s->controls->allocChunkSize, s->controls->minChunkSize))
    die ("alloc-chunk must be a multiple of the minimum chunk size, %zu", s->controls->minChunkSize);

  HM_configChunks(s);

  s->freeListSmall = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
  s->freeListLarge = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
  s->nextChunkAllocSize = s->controls->allocChunkSize;

  return res;
}

void GC_lateInit (GC_state s) {
  /* Initialize profiling.  This must occur after processing
   * command-line arguments, because those may just be doing a
   * show-sources, in which case we don't want to initialize the
   * atExit.
   */
  initProfiling (s);
  if (s->amOriginal) {
    initWorld (s);
  } else {
    DIE("loading world from file unsupported");
  }
  s->amInGC = FALSE;
}

void GC_duplicate (GC_state d, GC_state s) {
  // GC_init
  d->amInGC = s->amInGC;
  d->amOriginal = s->amOriginal;
  d->atomicState = 0;
  d->callFromCHandlerThread = BOGUS_OBJPTR;
  d->controls = s->controls;
  d->globalCumulativeStatistics = s->globalCumulativeStatistics;
  d->cumulativeStatistics = newCumulativeStatistics();
  d->currentThread = BOGUS_OBJPTR;
  d->wsQueue = BOGUS_OBJPTR;
  d->freeListSmall = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
  d->freeListLarge = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
  d->nextChunkAllocSize = s->nextChunkAllocSize;
  d->lastMajorStatistics = newLastMajorStatistics();
  d->numberOfProcs = s->numberOfProcs;
  d->roots = NULL;
  d->rootsLength = 0;
  d->savedThread = BOGUS_OBJPTR;
  d->signalHandlerThread = BOGUS_OBJPTR;
  d->signalsInfo.amInSignalHandler = FALSE;
  d->signalsInfo.gcSignalHandled = FALSE;
  d->signalsInfo.gcSignalPending = FALSE;
  d->signalsInfo.signalIsPending = FALSE;
  sigemptyset (&d->signalsInfo.signalsHandled);
  sigemptyset (&d->signalsInfo.signalsPending);
  d->self = s->self;
  d->terminationLeader = INVALID_PROCESSOR_NUMBER;
  d->sysvals.pageSize = s->sysvals.pageSize;
  d->sysvals.physMem = s->sysvals.physMem;
  d->weaks = s->weaks;
  d->saveWorldStatus = s->saveWorldStatus;
  d->trace = NULL;
  srand48_r(0, &(d->tlsObjects.drand48_data));

  // SPOONHOWER_NOTE: better duplicate?
  //initSignalStack (d);

  d->sysvals.ram = s->sysvals.ram;

  // SPOONHOWER_NOTE: better duplicate
  //initProfiling (d);

  // Multi-processor support is incompatible with saved-worlds
  assert(d->amOriginal);
  duplicateWorld(d, s);
  s->amInGC = FALSE;
}

// AG_NOTE: is this the proper place for this function?
void GC_traceInit(GC_state s) {
#ifdef ENABLE_TRACING
  char filename[256];
  const char *dir;

  dir = getenv("MLTON_TRACE_DIR");
  if (dir == NULL)
    return;

  snprintf(filename, 256, "%s/%d.%d.trace", dir, getpid(), s->procNumber);
  s->trace = TracingNewContext(filename, s->controls->traceBufferSize,
                               s->procNumber);
#endif
}

void GC_traceFinish(GC_state s) {
#ifdef ENABLE_TRACING
  TracingCloseAndFreeContext(&s->trace);
#endif
}
