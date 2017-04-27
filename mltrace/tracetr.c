#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "trace.h"

static const char *EventKindStrings[] = {
  [EVENT_NIL]                   = "NIL",
  [EVENT_INIT]                  = "INIT",
  [EVENT_FINISH]                = "FINISH",

  [EVENT_GC_ENTER]              = "GC_ENTER",
  [EVENT_GC_LEAVE]              = "GC_LEAVE",

  [EVENT_HALT_REQ]              = "HALT_REQ",
  [EVENT_HALT_WAIT]             = "HALT_WAIT",
};

void displayFiles(size_t filecount, FILE **files);

void usage() {
  fprintf(stderr,
          "usage: trace [options] [input files]\n"
          "options:\n"
          "  -d                 display file contents on standard output\n"
          "  -h                 display this message\n"
    );
}

int main(int argc, char *argv[]) {
  int opt;
  size_t fcount;
  bool display = false, read_stdin = false;
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
      fprintf(stderr, "invalid option '%c' (%d)\n", opt, opt);
      usage();
      return 1;
    }
  }

  fcount = argc - optind;

  /* Open the files, if any. */

  if (fcount == 0) {
    read_stdin = true;
    fcount = 1;
  }

  if ((files = calloc(fcount, sizeof *files)) == NULL) {
    fprintf(stderr, "Could not allocate memory\n");
    return 1;
  }

  if (read_stdin)
    files[0] = stdin;
  else
    for (int i = 0; i < fcount; ++i) {
      const char *fn = argv[optind + i];

      if ((files[i] = fopen(fn, "rb")) == NULL) {
        fprintf(stderr, "%s: could not open file\n", fn);
        return 1;
      }
    }

  /* Perform the requested actions. */

  if (display)
    displayFiles(fcount, files);

  /* Close and free files. */

  if (!read_stdin)
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

void displayFiles(size_t filecount, FILE **files) {
  struct Event events[BUFFER_SIZE];

  for (size_t i = 0; i < filecount; ++i) {
    size_t evcount = 0, evbatchsize;

    printf("# events for file %zu\n", i);

    do {
      evbatchsize = fread(events, sizeof *events, BUFFER_SIZE,
                          files[i]);
      evcount += evbatchsize;

      for (int j = 0; j < evbatchsize; j++)
        printEventCSV(&events[j]);

    } while (evbatchsize == BUFFER_SIZE);

      printf("# file %zu finished, read %zd events\n", i, evcount);
  }
}

