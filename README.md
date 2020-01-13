# MPL

MaPLe (MPL) is an extension of the [MLton](http://mlton.org)
compiler for Standard ML which implements support for nested parallelism.

MPL is research software and is being actively developed.

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
   - [MLton](http://mlton.org) (`mlton`, `mllex`, and `mlyacc`) recommended.  Pre-built binary packages for MLton can be installed via an OS package manager or (for select platforms) obtained from http://mlton.org.
   - [SML/NJ](http://www.smlnj.org) (`sml`, `ml-lex`, `ml-yacc`) supported, but not recommended.

### Instructions

The following builds the compiler at `build/bin/mpl`.
```
$ make all
```

After building, MPL can then be installed to `/usr/local`:
```
$ make install
```
or to a custom directory with the `PREFIX` option:
```
$ make PREFIX=/opt/mpl install
```

## Parallel and Concurrent Extensions

MPL extends SML with a number of primitives for parallelism and concurrency.
Take a look at `examples/` to see these primitives in action.

**Note**: Before writing any of your own code, make sure to read the section
"Disentanglement" below.

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

## Disentanglement

Currently, MPL only supports programs that are **disentangled**, which
(roughly speaking) is the property that concurrent threads remain oblivious
to each other's allocations.

**MPL does not yet have a checker to verify
disentanglement, so the programmer must manually check their own
code**.

Here are a number of different ways to guarantee that your code is
disentangled.
- (Option 1) Use only purely functional data (no `ref`s or `array`s). This is
the simplest but most restrictive approach.
- (Option 2) If using mutable data, use only non-pointer data. MPL guarantees
that simple types (`int`, `word`, `char`, `real`, etc.) are never
indirected through a
pointer, so for example it is safe to use `int array`. Other types such as
`int list array` and `int array array` should be avoided. This approach
is very easy to check and is surprisingly general. Data races are fine!
- (Option 3) Make sure that your program is race-free. This can be
tricky to check but allows you to use any type of data. Many of our example
programs are race-free.

For more information about disentanglement, see the POPL 2020 paper
"Disentanglement in Nested-Parallel Programs" by Westrick et al.

## Bugs and Known Issues

### Basis Library
In general, the basis library has not yet been thoroughly scrubbed, and many
functions may not be safe for parallelism. Some known issues:
* `Int.toString` is racy when called in parallel.
* `Real.fromString` may throw an error when called in parallel.

### Other
* Some programs that use `compareAndSwap` and `arrayCompareAndSwap` primitives
may fail to compile.

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

