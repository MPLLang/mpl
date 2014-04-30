
#include <stdlib.h>

int fib (int n) {
  if (n <= 1) return n;
  return fib(n-2) + fib(n-1);
}


void main (int argc, char *argv) {

  int x = fib (atoi (argv[0]));
  printf ("finished with %d\n", x);

}
