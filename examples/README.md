# MPL Examples

This directory contains examples of parallel programming using MPL.
Each example program is defined by a `.mlb` file in `src/`, typically with a
corresponding `.sml` file of the same name. The directory `src/lib/` contains
common functions used across all examples.

To build everything, run `make` or `make -j`.

## Fibonacci

Calculate Fibonacci numbers with the standard recursive formula.
For example, Fib(39) using 4 processors:
```
$ make fib
$ ./fib @mpl procs 4 -- -N 39
```
This is not very practical but is a good demonstration of the basics of using MPL.

## Random Data

Generate an array of pseudo-random 64-bit words. Seed the randomness with
`-seed X`. For example, 1 billion words using 4 processors:
```
$ make random
$ ./random @mpl procs 4 -- -N 1000000000 -seed 15210
```

## Primes

Generate all primes up to a threshold. For example, all primes less than or
equal to 100 million:
```
$ make primes
$ ./primes @mpl procs 4 -- -N 100000000
```

## Mergesort

Mergesort on an array of random integers. For example:
```
$ make msort
$ ./msort @mpl procs 4 -- -N 100000000
```

## Dense Matrix Multiplication

Multiply two square matrices of size N*N. The sidelength N must be a
power-of-two. For example:
```
$ make dmm
$ ./dmm @mpl procs 4 -- -N 1024
```

## Ray Tracing

A simple ray tracer that generates MxN images in PPM format.  Use
`-m`/`-n` for output size, `-s` to select a scene, and `-f` to specify
the output file. By default, this outputs in P3 format (human readable);
use `--ppm6` for P6 instead (smaller file size but not easily readable).
For example:
```
$ make ray
$ ./ray -f out.ppm -m 400 -n 400 -s irreg
$ ./ray @mpl procs 4 -- -f out.ppm -m 1000 -n 1000 -s rgbbox --ppm6
```

## Tokenization

Parse a file into tokens identified by whitespace, writing the tokens to stdout
separated by newlines. Pass `--verbose` to print timing info and
`--no-output` to not dump the result to stdout.
```
$ make tokens
$ ./tokens FILE
$ ./tokens @mpl procs 4 -- FILE --verbose --no-output
```
