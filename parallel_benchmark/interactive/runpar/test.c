#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int main() {
  void *handle = dlopen("./fib1.so", RTLD_LAZY);
  int (*fib)(int);
  void (*fib_open)(void);
  void (*fib_close)(void);
  if (!handle) {
    fprintf(stderr, "%s\n", dlerror());
    exit(EXIT_FAILURE);
  }

  dlerror();

  *(void **) (&fib) = dlsym(handle, "fib");
  *(void **) (&fib_open) = dlsym(handle, "fib1_open");
  *(void **) (&fib_close) = dlsym(handle, "fib1_close");

  fib_open();
  printf("fib(40) = %d\n", (*fib)(40));
  fib_close();
  dlclose(handle);
}
