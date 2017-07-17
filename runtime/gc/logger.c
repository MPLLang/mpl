/* Copyright (C) 2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file logger.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the logger defined in logger.h
 */

#include "hierarchical-heap.h"

#include <string.h>
#include <strings.h>

/********************/
/* Extern Variables */
/********************/
enum LogLevel L_logLevels[NUM_LOG_MODULES] = {LL_ERROR};
bool L_flushLog[NUM_LOG_MODULES] = {FALSE};

/******************************/
/* Static Function Prototypes */
/******************************/
/**
 * This function converts a string to it's corresponding enum LogLevel.
 *
 * @param level The location to place the converted string into.
 * @param levelString The string to convert.
 *
 * @return TRUE if conversion succeeded, FALSE otherwise.
 */
bool stringToLogLevel(enum LogLevel* level, const char* levelString);

/**
 * This function converts a string to it's corresponding enum LogModule.
 *
 * @param module The location to place the converted string into.
 * @param moduleString The string to convert.
 *
 * @return TRUE if conversion succeeded, FALSE otherwise.
 */
bool stringToLogModule(enum LogModule* module, const char* moduleString);

/************************/
/* Function Definitions */
/************************/
bool initLogLevels(const char* arg) {
  char* argCopy = strdup(arg);
  bool retVal = TRUE;

  char* token = strtok(argCopy, " ");
  while (NULL != token) {
    /* check if this is a module-specific level */
    char moduleString[L_MAX_MODULE_LENGTH] = "";
    char levelFlushString[L_MAX_LOG_LEVEL_LENGTH + 2] = "error";

    enum LogModule module = NUM_LOG_MODULES;
    enum LogLevel level = LL_ERROR;
    bool flush = FALSE;

    assert(L_MAX_MODULE_LENGTH > 127);
    assert(L_MAX_LOG_LEVEL_LENGTH + 2 > 129);
    int retValMLF = sscanf(token,
                           "%127[^:]:%129[^:]",
                           moduleString,
                           levelFlushString);

    char levelString[L_MAX_LOG_LEVEL_LENGTH] = "error";
    char flushChar = 'f';
    int retValLF = 0;
    switch (retValMLF) {
      case (2):
        /* got module and level+flush */
        if (!stringToLogModule(&module, moduleString)) {
          fprintf(stderr,
                  "module \"%s\" in log specification \"%s\" is unrecognized\n",
                  moduleString,
                  token);
          goto ERROR;
        }

        /* FALLTHROUGH */

      case (1):
        /* got global level+flush */
        /* split level+flush */
        assert(L_MAX_LOG_LEVEL_LENGTH > 127);
        retValLF = sscanf(levelFlushString,
                          "%127[^/]/%1c",
                          levelString,
                          &flushChar);

        switch (retValLF) {
          case (2):
            /* got level and flush */
            if (('f' == flushChar) || ('F' == flushChar)) {
              flush = TRUE;
            } else {
              fprintf(stderr,
                      "flush \"%c\" in level+flush specification \"%s\" in log "
                      "specification \"%s\" is unrecognized\n",
                      flushChar,
                      levelFlushString,
                      token);
              goto ERROR;
            }

            /* FALLTHROUGH */

          case (1):
            /* got level only */
            if (!stringToLogLevel(&level, levelString)) {
              fprintf(stderr,
                      "level \"%s\" in level+flush specification \"%s\" in log "
                      "specification \"%s\" is unrecognized\n",
                      levelString,
                      levelFlushString,
                      token);
              goto ERROR;
            }

            break;

          default:
            fprintf(stderr,
                    "level+flush specification \"%s\" in log specification "
                    "\"%s\" is invalid\n",
                    levelFlushString,
                    token);
            goto ERROR;
        };

        break;

      default:
        fprintf(stderr, "log specification \"%s\" is invalid\n", token);
        goto ERROR;
    };

    /* at this point, 'module', 'level', and 'flush' are all set properly */
    if (NUM_LOG_MODULES == module) {
      /* global level and flush to set to everything */
      for(size_t i = 0; i < NUM_LOG_MODULES; i++) {
        L_logLevels[i] = level;
        L_flushLog[i] = flush;
      }
    } else {
      /* single module specification */
      L_logLevels[module] = level;
      L_flushLog[module] = flush;
    }

    token = strtok(NULL, " ");
  }

  retVal = TRUE;
  goto END;

ERROR:
  retVal = FALSE;

END:
  free(argCopy);

  return retVal;
}

/******************************/
/* Static Function Defintions */
/******************************/

bool stringToLogLevel(enum LogLevel* level, const char* levelString) {
  struct Conversion {
    const char* string;
    enum LogLevel level;
  };

  struct Conversion conversions[] =
      {{.string = "error", .level = LL_ERROR},
       {.string = "warning", .level = LL_WARNING},
       {.string = "info", .level = LL_INFO},
       {.string = "debug", .level = LL_DEBUG},
       {.string = "debugmore", .level = LL_DEBUGMORE}};
  size_t numConversions = sizeof(conversions) / sizeof(*conversions);

  for (size_t i = 0; i < numConversions; i++) {
    if (0 == strcasecmp(conversions[i].string, levelString)) {
      *level = conversions[i].level;
      return TRUE;
    }
  }

  return FALSE;
}

bool stringToLogModule(enum LogModule* module, const char* moduleString) {
  struct Conversion {
    const char* string;
    enum LogModule module;
  };

  struct Conversion conversions[] =
      {{.string = "allocation", .module = LM_ALLOCATION},
       {.string = "chunk", .module = LM_CHUNK},
       {.string = "chunk-pool", .module = LM_CHUNK_POOL},
       {.string = "dfs-mark", .module = LM_DFS_MARK},
       {.string = "foreach", .module = LM_FOREACH},
       {.string = "garbage-collection", .module = LM_GARBAGE_COLLECTION},
       {.string = "global-local-heap", .module = LM_GLOBAL_LOCAL_HEAP},
       {.string = "gc-state", .module = LM_GC_STATE},
       {.string = "hierarchical-heap", .module = LM_HIERARCHICAL_HEAP},
       {.string = "hh-collection", .module = LM_HH_COLLECTION},
       {.string = "parallel", .module = LM_PARALLEL},
       {.string = "thread", .module = LM_THREAD}};
  size_t numConversions = sizeof(conversions) / sizeof(*conversions);

  for (size_t i = 0; i < numConversions; i++) {
    if (0 == strcasecmp(conversions[i].string, moduleString)) {
      *module = conversions[i].module;
      return TRUE;
    }
  }

  return FALSE;
}
