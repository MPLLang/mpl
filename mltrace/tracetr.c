#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "trace.h"

static const char *EventKindStrings[] = {
  [EVENT_NIL]                   = "NIL",
  [EVENT_THREAD_CREATED]        = "THREAD_CREATED",
  [EVENT_GC_ENTER]              = "GC_ENTER",
  [EVENT_GC_LEAVE]              = "GC_LEAVE",
  [EVENT_HALT_REQ]              = "HALT_REQ",
  [EVENT_HALT_WAIT]             = "HALT_WAIT",
  [EVENT_HALT_EXIT]             = "HALT_EXIT",
};

void displayFiles(size_t filecount, char * const *filenames, FILE **files);

void usage() {
  fprintf(stderr,
          "usage: trace [options] FILE1.trace ... FILEn.trace\n"
          "options:\n"
          "  -d                 display file contents on standard output\n"
          "  -h                 display this message\n"
    );
}

int main(int argc, char **argv) {
  int opt;
  size_t fcount;
  bool display = true;
  FILE **files;

  /* Parse command line arguments. */

  while ((opt = getopt(argc, argv, "dh")) != -1) {
    switch (opt) {
    case 'd':
      display = true;
      break;
    case 'h':
      usage();
      return 0;
    default:
      fprintf(stderr, "invalid option %s\n", optarg);
      usage();
      return 1;
    }
  }

  fcount = argc - optind;

  /* Open the files and check headers. */

  if ((files = calloc(fcount, sizeof *files)) == NULL) {
    fprintf(stderr, "Could not allocate memory\n");
    return 1;
  }

  for (int i = 0; i < fcount; ++i) {
    const char *fn = argv[optind + i];

    if ((files[i] = fopen(fn, "rb")) == NULL) {
      fprintf(stderr, "%s: could not open file\n", fn);
      return 1;
    }

    struct TraceFileHeader header;
    if (fread(&header, sizeof header, 1, files[i]) != 1) {
      fprintf(stderr, "%s: could not read header\n", fn);
      return 1;
    }

    if (header.version != TraceCurrentVersion) {
      fprintf(stderr, "%s: incorrect header version (got %llx, expected %llx\n",
              fn, header.version, TraceCurrentVersion);
      return 1;
    }
  }

  /* Perform the requested actions on the files. */

  if (display)
    displayFiles(fcount, argv + optind, files);

  /* Close and free files. */

  for (int i = 0; i < fcount; i++)
    fclose(files[i]);
  free(files);

  return 0;
}

void printEventKind(int kind) {
  if (kind > 0 && (size_t)kind < EventKindCount) {
    printf(EventKindStrings[kind]);
  } else {
    printf("USER(%d)", kind);
  }
}

void printEventTime(struct Event *event) {
  double time;

  time = event->ts.tv_sec + event->ts.tv_nsec / 1E9;
  printf("%.9f", time);
}

void printEventCSV(struct Event *event) {
  printEventKind(event->kind);
  printf(",");
  printEventTime(event);
  printf(",%" PRIxPTR ",%llx,%llx\n",
         event->argptr, event->arg1, event->arg2);
}

#define BUFFER_SIZE 10000

void displayFiles(size_t filecount, char * const *filenames, FILE **files) {
  struct Event events[BUFFER_SIZE];

  for (int i = 0; i < filecount; ++i) {
    size_t evcount = 0, evbatchsize;

    printf("# events for %s\n", filenames[i]);

    do {
      evbatchsize = fread(events, sizeof *events, BUFFER_SIZE,
                          files[i]);
      evcount += evbatchsize;

      for (int j = 0; j < evbatchsize; j++)
        printEventCSV(&events[j]);

    } while (evbatchsize == BUFFER_SIZE);

      printf("# %s finished, read %zd events\n", filenames[i], evcount);
  }
}

