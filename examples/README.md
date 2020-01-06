# MPL Examples

This directory contains examples of parallel programming with MPL. To build
all examples, run `make` or `make -j`. Each example program is defined by
a `.mlb` file in `src/`, typically with a corresponding `.sml` file of the same
name. The directory `src/lib` contains common functions used across all
examples.

## Fibonacci

Calculate Fibonacci numbers with the standard recursive formula.
For example, Fib(39) using 4 processors:
```
$ make fib
$ ./fib @mpl procs 4 -- -N 39
```
This is not very practical but is a good demonstration of the basics of using MPL.

## Random Bits

Generate an array of pseudo-random 64-bit words. For example, 1 billion
words using 4 processors:
```
$ make random
$ ./random @mpl procs 8 -- -N 1000000000
```

## Primes

Generate all primes up to a threshold. For example, all primes less than or
equal to 100 million using 32 processors:
```
$ make random
$ ./random @mpl procs 32 -- -N 1000000000
```
