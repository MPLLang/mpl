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
make all
```

After building, MPL can then be installed to `/usr/local`:
```
make install
```
or to a custom directory with the `PREFIX` option:
```
make PREFIX=/opt/mpl install
```

## Parallel and Concurrent Extensions

MPL extends SML with a number of primitives for parallelism and concurrency.
For concrete examples, see the `examples/` subdirectory.

### The `ForkJoin` Structure
MPL provides a structure, `ForkJoin`, which implements the following primitives.
```
val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
val alloc: int -> 'a array
```
The `fork` primitive takes two functions to execute in parallel and
returns their results.

The `alloc` primitive takes a length and returns a fresh, uninitialized array
of that size. This is integrated with the scheduler and memory management
system to perform allocation in parallel and be safe-for-GC.

**Warning**: To guarantee no errors, the programmer must be careful to
fully initialize the array (write at each index) before reading from it.
The `alloc` function is intended to be used as a low-level primitive in the
efficient implementation of high-performance libraries.

### The `MLton.Parallel` Structure
The `MLton.Parallel` structure provides the following concurrency primitives:
```
val compareAndSwap : 'a ref -> ('a * 'a) -> 'a
val arrayCompareAndSwap : ('a array * int) -> ('a * 'a) -> 'a
```

## Compilation

MPL uses `.mlb` files ([ML Basis](http://mlton.org/MLBasis)) to describe
source files for compilation. A typical compilation file with MPL begins like
this:
```
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/basis/mlton.mlb
$(SML_LIB)/basis/schedulers/shh.mlb
...
```
These respectively load:
* The [SML Basis Library](http://sml-family.org/Basis/)
* The `MLton.Parallel` as described above, as well as other
  [MLton-specific features](http://mlton.org/MLtonStructure) (although many
  of these are unsupported, see "Known Issues" below)
* The `ForkJoin` structure, as described above

### Compile-time Options

(TODO)

## Execution

### Run-time Options

(TODO)

## Disentanglement

Currently, MPL only supports programs that are **disentangled**, which
(roughly speaking) is the property that concurrent threads remain oblivious
to each other's allocations. For more information, see the POPL 2020 paper
"Disentanglement in Nested-Parallel Programs" by Westrick et al.

MPL does not yet have a checker to verify
disentanglement, so the programmer must manually verify their own code.
This is typically not difficult, as disentanglement is fairly general.
In particular, all race-free programs are disentangled, as are many
programs that use shared memory to communicate between concurrent threads.

## Known Issues

### Bugs
* `Int.toString` is racy when called in parallel.
* `Real.fromString` may throw an error when called in parallel.
* Some programs that use `compareAndSwap` and `arrayCompareAndSwap` primitives
may fail to compile.

###

### Unsupported Features
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

