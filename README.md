# MaPLe (MPL)

MaPLe (MPL) is a functional language for provably efficient and safe multicore
parallelism, developed at Carnegie Mellon University.

Features:
  * Support for the full Standard ML programming language, extended with
    primitives for task-based parallelism as well as data-parallel computations.
  * Whole-program compilation based on [MLton](http://www.mlton.org),
    with aggressive optimizations to achieve performance competitive
    with languages such as C/C++.
  * Efficient memory representations, including untagged and unboxed native
    integers (signed and unsigned), reals (32-bit and 64-bit floating point),
    characters, and native arrays with unboxed elements.
  * Simple and fast foreign-function calls into C, based on
    [MLtonFFI](http://mlton.org/ForeignFunctionInterface).
  * Efficient support for both regular and irregular fine-grained parallelism,
    with provably efficient **automatic parallelism management** [[7](#wfra24)]
    to control the overheads of task creation.
  * Provably efficient parallel garbage collection based on
    **hierarchical memory management** and **disentanglement**
    [[1](#rmab16),[2](#gwraf18),[3](#wyfa20),[4](#awa21),[5](#waa22),[6](#awa23)].
  * Support for large memory sizes and large core counts. MPL efficiently
    handles heap sizes of as much as 1TB, and scales to hundreds of cores.

MPL is being actively developed. If you are interested in contributing to
the project, PRs are welcome!

If you are you interested in using MPL, consider checking
out the [tutorial](https://github.com/MPLLang/mpl-tutorial).
You might also be interested in exploring
[`mpllib`](https://github.com/MPLLang/mpllib)
(a library for MPL) and the
[Parallel ML benchmark suite](https://github.com/MPLLang/parallel-ml-bench).


## References

[<a name="wfra24">7</a>]
[Automatic Parallelism Management](https://www.cs.cmu.edu/~swestric/24/popl24-par-manage.pdf).
Sam Westrick, Matthew Fluet, Mike Rainey, and Umut A. Acar.
POPL 2024.

[<a name="awa23">6</a>]
[Efficient Parallel Functional Programming with Effects](https://www.cs.cmu.edu/~swestric/23/epfpe.pdf).
Jatin Arora, Sam Westrick, and Umut A. Acar.
PLDI 2023.

[<a name="waa22">5</a>]
[Entanglement Detection with Near-Zero Cost](http://www.cs.cmu.edu/~swestric/22/icfp-detect.pdf).
Sam Westrick, Jatin Arora, and Umut A. Acar.
ICFP 2022.

[<a name="awa21">4</a>]
[Provably Space-Efficient Parallel Functional Programming](http://www.cs.cmu.edu/~swestric/21/popl.pdf).
Jatin Arora, Sam Westrick, and Umut A. Acar.
POPL 2021.

[<a name="wyfa20">3</a>]
[Disentanglement in Nested-Parallel Programs](http://www.cs.cmu.edu/~swestric/20/popl-disentangled.pdf).
Sam Westrick, Rohan Yadav, Matthew Fluet, and Umut A. Acar.
POPL 2020.

[<a name="gwraf18">2</a>]
[Hierarchical Memory Management for Mutable State](http://www.cs.cmu.edu/~swestric/18/ppopp.pdf).
Adrien Guatto, Sam Westrick, Ram Raghunathan, Umut Acar, and Matthew Fluet.
PPoPP 2018.

[<a name="rmab16">1</a>]
[Hierarchical Memory Management for Parallel Programs](http://cs.iit.edu/~smuller/papers/icfp16-preprint.pdf).
Ram Raghunathan, Stefan K. Muller, Umut A. Acar, and Guy Blelloch.
ICFP 2016.

## Try It Out

If you want to quickly try out using MPL, you can download the Docker image and
run one of the [examples](examples/).
```
$ docker pull shwestrick/mpl
$ docker run -it shwestrick/mpl /bin/bash
...# examples/bin/primes @mpl procs 4 --
```

To write and compile your own code, we recommend
mounting a local directory inside the container. For example, here's how you
can use MPL to compile and run your own `main.mlb` in the current directory.
(To mount some other directory, replace `$(pwd -P)` with a different path.)
```
$ ls
main.mlb
$ docker run -it -v $(pwd -P):/root/mycode shwestrick/mpl /bin/bash
...# cd /root/mycode
...# mpl main.mlb
...# ./main @mpl procs 4 --
```

## Benchmark Suite

The [Parallel ML benchmark suite](https://github.com/MPLLang/parallel-ml-bench)
provides many examples of sophisticated parallel algorithms and
applications in MPL, as well as cross-language performance comparisons with
C++, Go, Java,
and multicore OCaml.

## Libraries

We recommend using the [smlpkg](https://github.com/diku-dk/smlpkg) package
manager. MPL supports the full SML language, so existing libraries for
SML can be used.

In addition, here are a few libraries that make use of MPL for parallelism:
  * [`github.com/MPLLang/mpllib`](https://github.com/MPLLang/mpllib): implements
  a variety of data structures (sequences, sets, dictionaries, graphs, matrices, meshes,
  images, etc.) and parallel algorithms (map, reduce, scan, filter, sorting,
  search, tokenization, graph processing, computational geometry, etc.). Also
  includes basic utilies (e.g. parsing command-line arguments) and
  benchmarking infrastructure.
  * [`github.com/shwestrick/sml-audio`](https://github.com/shwestrick/sml-audio):
  a library for audio processing with I/O support for `.wav` files.


## Parallel and Concurrent Extensions

MPL extends SML with a number of primitives for parallelism and concurrency.
Take a look at `examples/` to see these primitives in action.

### The `ForkJoin` Structure
```
val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
val parfor: int -> (int * int) -> (int -> unit) -> unit
val alloc: int -> 'a array
```
The `par` primitive takes two functions to execute in parallel and
returns their results.

The `parfor` primitive is a "parallel for loop". It takes a grain-size `g`, a
range `(i, j)`, and a function `f`, and executes `f(k)` in parallel for each
`i <= k < j`. The grain-size `g` is for manual granularity
control: `parfor` splits the input range into approximately `(j-i)/g` subranges,
each of size at most `g`, and each subrange is processed sequentially. The
grain-size must be at least 1, in which case the loop is "fully parallel".

The `alloc` primitive takes a length and returns a fresh, uninitialized array
of that size. **Warning**: To guarantee no errors, the programmer must be
careful to initialize the array before reading from it. `alloc` is intended to
be used as a low-level primitive in the efficient implementation of
high-performance libraries. It is integrated with the scheduler and memory
management system to perform allocation in parallel and be safe-for-GC.

### The `MLton.Parallel` Structure
```
val compareAndSwap: 'a ref -> ('a * 'a) -> 'a
val arrayCompareAndSwap: ('a array * int) -> ('a * 'a) -> 'a
```

`compareAndSwap r (x, y)` performs an atomic
[CAS](https://en.wikipedia.org/wiki/Compare-and-swap)
which attempts to atomically swap the contents of `r` from `x` to `y`,
returning the original value stored in `r` before the CAS.
Polymorphic equality is determined
in the same way as [MLton.eq](http://mlton.org/MLtonStructure), which is a
standard equality check for simple types (`char`, `int`, `word`, etc.) and
a pointer equality check for other types (`array`, `string`, tuples, datatypes,
etc.). The semantics are a bit murky.

`arrayCompareAndSwap (a, i) (x, y)` behaves the same as `compareAndSwap` but
on arrays instead of references. This performs a CAS at index `i` of array
`a`, and does not read or write at any other locations of the array.

## Using MPL

MPL uses `.mlb` files ([ML Basis](http://mlton.org/MLBasis)) to describe
source files for compilation. A typical `.mlb` file for MPL is shown
below. The first three lines of this file respectively load:
* The [SML Basis Library](http://sml-family.org/Basis/)
* The `ForkJoin` structure, as described above
* The `MLton` structure, which includes the MPL extension
  `MLton.Parallel` as described above, as well as various
  [MLton-specific features](http://mlton.org/MLtonStructure). Not all MLton
  features are supported (see "Unsupported MLton Features" below).
```
(* libraries *)
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/basis/fork-join.mlb
$(SML_LIB)/basis/mlton.mlb

(* your source files... *)
A.sml
B.sml
```

### Compiling a Program

The command to compile a `.mlb` is as follows. By default, MPL
produces an executable with the same base name as the source file, i.e.
this would create an executable named `foo`:
```
$ mpl [compile-time options...] foo.mlb
```

MPL has a number of compile-time options derived from MLton, which are
documented [here](http://mlton.org/CompileTimeOptions). Note that MPL only
supports C codegen and does not support profiling.

Some useful compile-time options are
* `-output <NAME>` Give a specific name to the produced executable.
* `-default-type int64 -default-type word64` Use 64-bit integers and words
by default.
* `-debug true -debug-runtime true -keep g` For debugging, keeps the generated
C files and uses the debug version of the runtime (with assertions enabled).
The resulting executable is somewhat peruse-able with tools like `gdb`.

For example:
```
$ mpl -default-type -int64 -output foo sources.mlb
```

### Running a Program

MPL executables can take options at the command line that control the run-time
system. The syntax is
```
$ <program> [@mpl [run-time options...] --] [program args...]
```
The runtime arguments must begin with `@mpl` and end with `--`, and these are
not visible to the program via
[CommandLine.arguments](http://sml-family.org/Basis/command-line.html).

Some useful run-time options are
* `procs <N>` Use `N` worker threads to run the program.
* `set-affinity` Pin worker threads to processors. Can be used in combination
with `affinity-base <B>` and `affinity-stride <S>` to pin thread `i` to
processor number `B + S*i`.
* `block-size <X>` Set the heap block size to `X` bytes. This can be
written with suffixes K, M, and G, e.g. `64K` is 64 kilobytes. The block-size
must be a multiple of the system page size (typically 4K). By default it is
set to one page.

For example, the following runs a program `foo` with a single command-line
argument `bar` using 4 pinned processors.
```
$ foo @mpl procs 4 set-affinity -- bar
```


## Bugs and Known Issues

### Basis Library
The basis library is inherited from (sequential) SML. It has not yet been 
thoroughly scrubbed, and some functions may not be safe for parallelism
([#41](https://github.com/MPLLang/mpl/issues/41)).

### Garbage Collection
* ([#115](https://github.com/MPLLang/mpl/issues/115)) The GC is currently
disabled at the "top level" (outside any calls to `ForkJoin.par`).
For highly parallel programs, this has generally not been a problem so far,
but it can cause a memory explosion for programs that are mostly (or entirely)
sequential.

## Unsupported MLton Features
Many [MLton-specific features](http://mlton.org/MLtonStructure) are
unsupported, including (but not limited to):
* `share`
* `shareAll`
* `size`
* `Finalizable`
* `Profile`
* `Signal`
* `Thread` (partially supported but not documented)
* `Cont` (partially supported but not documented)
* `Weak`
* `World`


## Build and Install (from source)

### Requirements

MPL has only been tested on Linux with x86-64. The following software is
required.
 * [GCC](http://gcc.gnu.org)
 * [GMP](http://gmplib.org) (GNU Multiple Precision arithmetic library)
 * [GNU Make](http://savannah.gnu.org/projects/make), [GNU Bash](http://www.gnu.org/software/bash/)
 * binutils (`ar`, `ranlib`, `strip`, ...)
 * miscellaneous Unix utilities (`diff`, `find`, `grep`, `gzip`, `patch`, `sed`, `tar`, `xargs`, ...)
 * Standard ML compiler and tools:
   - Recommended: [MLton](http://mlton.org) (`mlton`, `mllex`, and `mlyacc`).  Pre-built binary packages for MLton can be installed via an OS package manager or (for select platforms) obtained from http://mlton.org.
   - Supported but not recommended: [SML/NJ](http://www.smlnj.org) (`sml`, `ml-lex`, `ml-yacc`).
 * (If using [`mpl-switch`](https://github.com/mpllang/mpl-switch)): Python 3, and `git`.

### Installation with `mpl-switch`

The [`mpl-switch`](https://github.com/mpllang/mpl-switch) utility makes it
easy to install multiple versions of MPL on the same system and switch
between them. After setting up `mpl-switch`, you can install MPL as follows:
```
$ mpl-switch install v0.4
$ mpl-switch select v0.4
```

You can use any commit hash or tag name from the MPL repo to pick a
particular version of MPL. Installed versions are stored in `~/.mpl/`; this
folder is safe to delete at any moment, as it can always be regenerated. To
see what versions of MPL are currently installed, do:
```
$ mpl-switch list
```

### Manual Instructions

Alternatively, you can manually build `mpl` by cloning this repo and then
performing the following.

**Build the executable**. This produces an executable at `build/bin/mpl`:
```
$ make
```

**Put it where you want it**. After building, MPL can then be installed to
`/usr/local`:
```
$ make install
```
or to a custom directory with the `PREFIX` option:
```
$ make PREFIX=/opt/mpl install
```
