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

## Tabulate

Generate an array of pseudo-random characters. For example, 1 billion
characters using 8 processors:
```
$ make tabulate
$ ./tabulate @mpl procs 4 -- -N 1000000000
```
