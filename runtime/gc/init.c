/* Copyright (C) 2009,2012,2015,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

#include <stdlib.h>
#include <strings.h>

static bool stringToBool(char *s) {
  if (0 == strcmp (s, "false"))
    return FALSE;
  if (0 == strcmp (s, "true"))
    return TRUE;
  die ("Invalid @MLton/@mpl bool: %s.", s);
}

// From gdtoa/gdtoa.h.
// Can't include the whole thing because it brings in too much junk.
float gdtoa__strtof (const char *, char **);

static float stringToFloat(char *s) {
  char *endptr;
  float f;

  f = gdtoa__strtof (s, &endptr);
  if (s == endptr)
    die ("Invalid @MLton/@mpl float: %s.", s);
  return f;
}

static int32_t stringToInt(char *s) {
  char *endptr;
  int i;

  i = strtol (s, &endptr, 10);
  if (s == endptr)
    die ("Invalid @MLton/@mpl int: %s.", s);
  return i;
}

static size_t stringToBytes(const char *s) {
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
  die ("Invalid @MLton/@mpl memory amount: %s.", s);
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
         and ( (0 == strcmp (argv[i], "@MLton"))
               || (0 == strcmp (argv[i], "@mpl"))
             )
        )
  {
    bool done;

    const char *atName = "@MLton";
    if (0 == strcmp (argv[i], "@mpl")) {
      atName = "@mpl";
    }

    i++;
    done = FALSE;
    while (!done) {
      if (i == argc)
        die ("Missing -- at end of %s args.", atName);
      else {
        char *arg;

        arg = argv[i];
        if (0 == strcmp (arg, "gc-messages")) {
          i++;
          s->controls->messages = TRUE;
        } else if (0 == strcmp (arg, "gc-summary")) {
          i++;
          s->controls->summary = TRUE;
        } else if (0 == strcmp (arg, "gc-summary-file")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("%s gc-summary-file missing argument.", atName);
          const char* filePath = argv[i++];
          if (0 == strcmp(filePath, "-")) {
            s->controls->summaryFile = stdout;
          } else {
            s->controls->summaryFile = fopen(filePath, "w");
            if (s->controls->summaryFile == NULL) {
              die ("Invalid %s gc-summary-file %s (%s).", atName, filePath, strerror(errno));
            }
          }
        } else if (0 == strcmp (arg, "gc-summary-format")) {
          i++;

          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s gc-summary-format missing argument.", atName);
          }

          const char* format = argv[i++];
          if (0 == strcmp (format, "human")) {
            s->controls->summaryFormat = HUMAN;
          } else if (0 == strcmp (format, "json")) {
            s->controls->summaryFormat = JSON;
          } else {
            die ("%s gc-summary-format \"%s\" invalid. Must be one of "
                 "human or json.",
                 atName,
                 format);
          }
        } else if (0 == strcmp (arg, "set-affinity")) {
          i++;
          s->controls->setAffinity = TRUE;
        } else if (0 == strcmp (arg, "affinity-base")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("%s affinity-base missing argument.", atName);
          s->controls->affinityBase = stringToInt (argv[i++]);
        } else if (0 == strcmp (arg, "affinity-stride")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("%s affinity-stride missing argument.", atName);
          s->controls->affinityStride = stringToInt (argv[i++]);
        } else if (0 == strcmp (arg, "debug-keep-free-blocks")) {
          i++;
          s->controls->debugKeepFreeBlocks = TRUE;
        } else if (0 == strcmp (arg, "load-world")) {
          unless (s->controls->mayLoadWorld)
            die ("May not load world.");
          i++;
          s->amOriginal = FALSE;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s load-world missing argument.", atName);
          *worldFile = argv[i++];
        } else if (0 == strcmp(arg, "log-file")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die("%s log-file missing argument.", atName);
          }

          const char* filePath = argv[i++];
          if (0 == strcmp(filePath, "-")) {
            L_setFile(stdout);
          } else {
            FILE* file = fopen(filePath, "w");
            if (NULL == file) {
              diee("%s Could not open specified log file.", atName);
            } else {
              L_setFile(file);
            }
          }
        } else if (0 == strcmp(arg, "log-level")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die("%s log-level missing argument.", atName);
          }

          char* levelString = argv[i++];
          if (!initLogLevels(levelString)) {
            die ("%s log-level invalid argument", atName);
          }
        } else if (0 == strcmp (arg, "manage-entanglement")) {
          i++;
          die ("%s manage-entanglement not supported at the moment", atName);
          s->controls->manageEntanglement = TRUE;
        } else if (0 == strcmp (arg, "no-load-world")) {
          i++;
          s->controls->mayLoadWorld = FALSE;
        } else if (0 == strcmp (arg, "ram-slop")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s ram-slop missing argument.", atName);
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
            die ("%s stack-current-grow-ratio missing argument.", atName);
          s->controls->ratios.stackCurrentGrow = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentGrow)
            die ("%s stack-current-grow-ratio argument must greater than 1.0.", atName);
        } else if (0 == strcmp (arg, "stack-current-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s stack-current-max-reserved-ratio missing argument.", atName);
          s->controls->ratios.stackCurrentMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentMaxReserved)
            die ("%s stack-current-max-reserved-ratio argument must greater than 1.0.", atName);
        } else if (0 == strcmp (arg, "stack-current-permit-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s stack-current-permit-reserved-ratio missing argument.", atName);
          s->controls->ratios.stackCurrentPermitReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackCurrentPermitReserved)
            die ("%s stack-current-permit-reserved-ratio argument must greater than 1.0.", atName);
        } else if (0 == strcmp (arg, "stack-current-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s stack-current-shrink-ratio missing argument.", atName);
          s->controls->ratios.stackCurrentShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.stackCurrentShrink
                  and s->controls->ratios.stackCurrentShrink <= 1.0)
            die ("%s stack-current-shrink-ratio argument must be between 0.0 and 1.0.", atName);
        } else if (0 == strcmp (arg, "stack-max-reserved-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s stack-max-reserved-ratio missing argument.", atName);
          s->controls->ratios.stackMaxReserved = stringToFloat (argv[i++]);
          unless (1.0 < s->controls->ratios.stackMaxReserved)
            die ("%s stack-max-reserved-ratio argument must greater than 1.0.", atName);
        } else if (0 == strcmp (arg, "stack-shrink-ratio")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s stack-shrink-ratio missing argument.", atName);
          s->controls->ratios.stackShrink = stringToFloat (argv[i++]);
          unless (0.0 <= s->controls->ratios.stackShrink
                  and s->controls->ratios.stackShrink <= 1.0)
            die ("%s stack-shrink-ratio argument must be between 0.0 and 1.0.", atName);
        } else if (0 == strcmp (arg, "use-mmap")) {
          i++;
          if (i == argc || 0 == strcmp (argv[i], "--"))
            die ("%s use-mmap missing argument.", atName);
          GC_setCygwinUseMmap (stringToBool (argv[i++]));
        } else if ( (0 == strcmp (arg, "number-processors")) ||
                    (0 == strcmp (arg, "procs")) ) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--")))
            die ("%s %s missing argument.", atName, arg);
          if (!s->amOriginal)
            die ("%s %s incompatible with loaded worlds.", atName, arg);
          s->numberOfProcs = stringToFloat (argv[i++]);
          /* Turn off loaded worlds -- they are unsuppoed in multi-proc mode */
          s->controls->mayLoadWorld = FALSE;
        } else if ( (0 == strcmp(arg, "min-chunk")) ||
                    (0 == strcmp(arg, "block-size")) ) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s %s missing argument.", atName, arg);
          }
          s->controls->blockSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "alloc-chunk")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s alloc-chunk missing argument.", atName);
          }
          s->controls->allocChunkSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "alloc-blocks-min-size")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s alloc-blocks-min-size missing argument.", atName);
          }
          s->controls->allocBlocksMinSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "emptiness-fraction")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s emptiness-fraction missing argument.", atName);
          }
          s->controls->emptinessFraction = stringToFloat(argv[i++]);
          if (s->controls->emptinessFraction <= 0.0 ||
              s->controls->emptinessFraction >= 0.5)
          {
            die("%s emptiness-fraction must be strictly between 0 and 0.5", atName);
          }
        } else if (0 == strcmp(arg, "superblock-threshold")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s superblock-threshold missing argument.", atName);
          }
          int xx = stringToInt(argv[i++]);
          if (xx <= 0) {
            die("%s superblock-threshold must be at least 1", atName);
          }
          s->controls->superblockThreshold = xx;
        } else if (0 == strcmp(arg, "megablock-threshold")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s megablock-threshold missing argument.", atName);
          }
          int xx = stringToInt(argv[i++]);
          if (xx <= 0) {
            die("%s megablock-threshold must be at least 1", atName);
          }
          s->controls->megablockThreshold = xx;
        } else if (0 == strcmp (arg, "collection-type")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s collection-type missing argument.", atName);
          }
          const char* collectType = argv[i++];
          if (0 == strcmp (collectType, "none")) {
            s->controls->collectionType = NONE;
          } else if (0 == strcmp (collectType, "superlocal")) {
            s->controls->collectionType = SUPERLOCAL;
          } else if (0 == strcmp (collectType, "local")) {
            s->controls->collectionType = LOCAL;
          } else {
            die ("%s collection-type \"%s\" invalid. Must be one of "
                 "none, superlocal, or local.",
                 atName,
                 collectType);
          }
        } else if (0 == strcmp(arg, "collection-threshold-ratio")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s collection-threshold-ratio missing argument.", atName);
          }

          s->controls->hhConfig.collectionThresholdRatio = stringToFloat(argv[i++]);
          if (s->controls->hhConfig.collectionThresholdRatio < 1.0) {
            die("%s collection-threshold-ratio must be at least 1.0", atName);
          }
        } else if (0 == strcmp (arg, "cc-threshold-ratio")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s cc-threshold-ratio missing argument.", atName);
          }

          s->controls->hhConfig.ccThresholdRatio = stringToFloat(argv[i++]);
          if (s->controls->hhConfig.ccThresholdRatio <= 1.0) {
            die("%s cc-threshold-ratio must be > 1.0", atName);
          }
        } else if (0 == strcmp(arg, "min-collection-size")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s min-collection-size missing argument.", atName);
          }

          s->controls->hhConfig.minCollectionSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "min-cc-size")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s min-cc-size missing argument.", atName);
          }

          s->controls->hhConfig.minCCSize = stringToBytes(argv[i++]);
        } else if (0 == strcmp(arg, "max-cc-chain-length")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s max-cc-chain-length missing argument.", atName);
          }

          int len = stringToInt(argv[i++]);
          if (len <= 0) {
            die ("%s max-cc-chain-length must be >= 1", atName);
          }
          s->controls->hhConfig.maxCCChainLength = len;
        } else if (0 == strcmp(arg, "min-collection-depth")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s min-collection-depth missing argument.", atName);
          }

          int minDepth = stringToInt(argv[i++]);
          if (minDepth <= 0) {
            die ("%s min-collection-depth must be > 0", atName);
          }
          s->controls->hhConfig.minLocalDepth = minDepth;
        } else if (0 == strcmp(arg, "max-cc-depth")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s max-cc-depth missing argument.", atName);
          }

          int maxd = stringToInt(argv[i++]);
          if (maxd < 0) {
            die ("%s max-cc-depth must be >= 0", atName);
          }
          s->controls->hhConfig.maxCCDepth = maxd;
        } else if (0 == strcmp(arg, "trace-buffer-size")) {
          i++;
          if (i == argc || (0 == strcmp (argv[i], "--"))) {
            die ("%s trace-buffer-size missing argument.", atName);
          }

          s->controls->traceBufferSize = stringToInt(argv[i++]);
        } else if (0 == strcmp (arg, "--")) {
          i++;
          done = TRUE;
        } else if (i > 1)
          die ("Strange %s arg: %s", atName, argv[i]);
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
  s->currentCCTargetHH = NULL;
  // s->amInCC = FALSE;
  s->amOriginal = TRUE;
  s->atomicState = 0;
  s->callFromCHandlerThread = BOGUS_OBJPTR;

  s->controls = (struct GC_controls *) malloc (sizeof (struct GC_controls));
  s->controls->mayLoadWorld = FALSE; /* incompatible with mpl runtime */
  s->controls->mayProcessAtMLton = TRUE;
  s->controls->messages = FALSE;
  s->controls->setAffinity = FALSE;
  s->controls->affinityBase = 0;
  s->controls->affinityStride = 1;
  s->controls->ratios.ramSlop = 0.5f;
  s->controls->ratios.stackCurrentGrow = 2.0f;
  s->controls->ratios.stackCurrentMaxReserved = 32.0f;
  s->controls->ratios.stackCurrentPermitReserved = 4.0f;
  s->controls->ratios.stackCurrentShrink = 0.5f;
  s->controls->ratios.stackMaxReserved = 8.0f;
  s->controls->ratios.stackShrink = 0.5f;
  s->controls->hhConfig.collectionThresholdRatio = 8.0;
  s->controls->hhConfig.minCollectionSize = 1024L * 1024L;
  s->controls->hhConfig.minCCSize = 1024L * 1024L;
  s->controls->hhConfig.maxCCChainLength = 2;
  s->controls->hhConfig.ccThresholdRatio = 2.0f;
  s->controls->hhConfig.maxCCDepth = 3;
  s->controls->hhConfig.minLocalDepth = 2;
  s->controls->rusageMeasureGC = FALSE;
  s->controls->summary = FALSE;
  s->controls->summaryFormat = HUMAN;
  s->controls->summaryFile = stderr;
  s->controls->collectionType = ALL;
  s->controls->traceBufferSize = 10000;
  s->controls->emptinessFraction = 0.25;
  s->controls->superblockThreshold = 7;  // superblocks of 128 blocks
  s->controls->megablockThreshold = 18;
  s->controls->manageEntanglement = FALSE;

  /* Not arbitrary; should be at least the page size and must also respect the
   * limit check coalescing amount in the compiler. */
  s->controls->blockSize = max(GC_pageSize(), 4096);
  s->controls->allocBlocksMinSize = 1024 * s->controls->blockSize;

  /* check if still equal to 0 after process @mpl to see if the user wants
   * a particular size, and if not, set to default. */
  s->controls->allocChunkSize = 0;

  s->controls->freeListCoalesce = FALSE;
  s->controls->debugKeepFreeBlocks = FALSE;

  s->globalCumulativeStatistics = newGlobalCumulativeStatistics();
  s->cumulativeStatistics = newCumulativeStatistics();

  s->currentThread = BOGUS_OBJPTR;
  s->wsQueue = BOGUS_OBJPTR;
  s->wsQueueTop = BOGUS_OBJPTR;
  s->wsQueueBot = BOGUS_OBJPTR;

  s->lastMajorStatistics = newLastMajorStatistics();

  s->numberOfProcs = 1;
  s->procStates = NULL;
  s->roots = NULL;
  s->rootsLength = 0;
  s->savedThread = BOGUS_OBJPTR;

  initFixedSizeAllocator(getHHAllocator(s), sizeof(struct HM_HierarchicalHeap));
  initFixedSizeAllocator(getUFAllocator(s), sizeof(struct HM_UnionFindNode));
  s->numberDisentanglementChecks = 0;

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
  unless (s->controls->ratios.stackCurrentPermitReserved
          <= s->controls->ratios.stackCurrentMaxReserved)
    die ("Ratios must satisfy stack-current-permit-reserved <= stack-current-max-reserved.");

  /* SAM_NOTE: no longer used, but seems like this code may be useful in the
   * future, so I'm leaving it.
   *
   * OLD COMMNENT:
   * We align s->sysvals.ram by s->sysvals.pageSize so that we can
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
    for (i = 0; i < s->frameInfosLength; i++) {
      uint32_t j;
      const uint32_t *sourceSeq;
      fprintf (stderr, "%"PRIu32"\n", i);
      sourceSeq = s->sourceMaps.sourceSeqs[s->frameInfos[i].sourceSeqIndex];
      for (j = 1; j <= sourceSeq[0]; j++)
        fprintf (stderr, "\t%s\n",
                 s->sourceMaps.sourceNames[
                 s->sourceMaps.sources[sourceSeq[j]].sourceNameIndex
                 ]);
    }
  }

  unless (isAligned(s->controls->blockSize, s->sysvals.pageSize))
    die ("block-size must be a multiple of the system page size (%zu)", s->sysvals.pageSize);

  if (s->controls->allocChunkSize == 0) {
    /* user didn't specify a specify alloc-chunk size, so set a default. */
    size_t bs = s->controls->blockSize;
    s->controls->allocChunkSize = min(align(16 * GC_pageSize(), bs), 2 * bs);
  }

  unless (isAligned(s->controls->allocChunkSize, s->controls->blockSize))
    die ("alloc-chunk must be a multiple of the block-size (%zu)", s->controls->blockSize);

  unless (isAligned(s->controls->allocBlocksMinSize, s->controls->blockSize))
    die("alloc-blocks-min-size must be a multiple of the block-size (%zu)",
      s->controls->blockSize);

  unless (s->controls->superblockThreshold <= s->controls->megablockThreshold)
    die("superblock-threshold (currently %zu) must be at most the megablock-threshold (currently %zu)",
      s->controls->superblockThreshold,
      s->controls->megablockThreshold);

  return res;
}

void GC_lateInit(GC_state s) {

  /* this has to happen AFTER pthread_setspecific for the main thread */
  HM_configChunks(s);

  HH_EBR_init(s);

  initLocalBlockAllocator(s, initGlobalBlockAllocator(s));

  s->nextChunkAllocSize = s->controls->allocChunkSize;

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
  d->currentCCTargetHH = NULL;
  // d->amInCC = s->amInCC;
  d->amOriginal = s->amOriginal;
  d->atomicState = 0;
  d->callFromCHandlerThread = BOGUS_OBJPTR;
  d->controls = s->controls;
  d->globalCumulativeStatistics = s->globalCumulativeStatistics;
  d->cumulativeStatistics = newCumulativeStatistics();
  d->currentThread = BOGUS_OBJPTR;
  d->wsQueue = BOGUS_OBJPTR;
  d->wsQueueTop = BOGUS_OBJPTR;
  d->wsQueueBot = BOGUS_OBJPTR;
  initLocalBlockAllocator(d, s->blockAllocatorGlobal);
  initFixedSizeAllocator(getHHAllocator(d), sizeof(struct HM_HierarchicalHeap));
  initFixedSizeAllocator(getUFAllocator(d), sizeof(struct HM_UnionFindNode));
  d->hhEBR = s->hhEBR;
  d->nextChunkAllocSize = s->nextChunkAllocSize;
  d->lastMajorStatistics = newLastMajorStatistics();
  d->numberOfProcs = s->numberOfProcs;
  d->numberDisentanglementChecks = 0;
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
void GC_traceInit(ARG_USED_FOR_TRACING GC_state s) {
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

void GC_traceFinish(ARG_USED_FOR_TRACING GC_state s) {
#ifdef ENABLE_TRACING
  TracingCloseAndFreeContext(&s->trace);
#endif
}
