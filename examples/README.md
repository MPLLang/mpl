# MPL Examples

This directory contains examples of parallel programming using MPL.
Each example program has a corresponding subdirectory in `src/`.
The `lib/` directory contains common functions used across all examples.

To build everything, run `make` or `make -j`. Compiled programs are
put into a `bin/`.

## Fibonacci

Calculate Fibonacci numbers with the standard recursive formula.
For example, Fib(39) using 4 processors:
```
$ make fib
$ bin/fib @mpl procs 4 -- -N 39
```
This is not very practical but is a good demonstration of the basics of using MPL.

## N Queens

Calculate the number of unique solutions to the
[N Queens problem](https://en.wikipedia.org/wiki/Eight_queens_puzzle).
For example, the number of solutions on a board of size 13x13 using 4
processors:
```
$ make nqueens
$ bin/nqueens @mpl procs 4 -- -N 13
```

## Random Data

Generate an array of pseudo-random 64-bit words. Seed the randomness with
`-seed X`. For example, 1 billion words using 4 processors:
```
$ make random
$ bin/random @mpl procs 4 -- -N 1000000000 -seed 15210
```

## Primes

Generate all primes up to a threshold. For example, all primes less than or
equal to 100 million:
```
$ make primes
$ bin/primes @mpl procs 4 -- -N 100000000
```

## Mergesort

Mergesort on an array of random integers. For example:
```
$ make msort
$ bin/msort @mpl procs 4 -- -N 100000000
```

## Dense Matrix Multiplication

Multiply two square matrices of size N*N. The sidelength N must be a
power-of-two. For example:
```
$ make dmm
$ bin/dmm @mpl procs 4 -- -N 1024
```

## Ray Tracing

A simple ray tracer that generates MxN images in PPM format.  Use
`-m`/`-n` for output size, `-s` to select a scene, and `-f` to specify
the output file. For example:
```
$ make ray
$ bin/ray -f out.ppm -m 400 -n 400 -s irreg
$ bin/ray @mpl procs 4 -- -f out.ppm -m 1000 -n 1000 -s rgbbox
```

## Tokenization

Parse a file into tokens identified by whitespace, writing the tokens to stdout
separated by newlines. Pass `--benchmark` to print timing info and
not dump the result to stdout.
```
$ make tokens
$ bin/tokens FILE
$ bin/tokens @mpl procs 4 -- FILE --benchmark
```

## Deduplication

Parse a file into tokens (identified by whitespace), deduplicate the tokens,
and write to stdout separated by newlines. Pass `--benchmark` to print timing
info and not dump the result to stdout.
```
$ make dedup
$ bin/dedup @mpl procs 4 -- FILE --benchmark
```

## Nearest Neighbors

For a set of 2d points, calculate the nearest neighbor of each point. This
example generates random points in the unit square. Select the number of
points with `-N`, and use `-output` to visualize the results (PPM format).
For visualization, use `-resolution` to indicate the width (in pixels) of the
output image. The resolution needs to be a similar order of magnitude as the
number of points in order to see anything interesting.
```
$ make nn
$ bin/nn @mpl procs 4 -- -N 10000 -output result.ppm -resolution 1000
```

## Reverb

Applies an artificial reverberation effect to a `.wav` sound file. At the
moment, it only supports either 8-bit or 16-bit PCM, and if there
are multiple channels, these will be mixed into a mono-channel output.
```
$ make reverb
$ bin/reverb @mpl procs 4 -- INPUT.wav -output OUTPUT.wav
```

## Seam-carving

Seam-carving is a content-aware image resizing algorithm that
rescales an image while attempting to preserve the interesting
features of the image. This implementation removes vertical seams
from a `.ppm` given as input. Use `-num-seams` to specify
how many seams to remove, and `-output` to see a visualization of
the process.
```
$ make seam-carve
$ bin/seam-carve @mpl procs 4 -- INPUT.ppm -output OUTPUT.gif -num-seams 100
```

## Coins

A port of the `coins` benchmark from the Haskell NoFib benchmark suite.
Use `-N` for the input size.
```
$ make coins
$ bin/coins @mpl procs 4 -- -N 999
```
