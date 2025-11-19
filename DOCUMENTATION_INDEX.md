# MPL Documentation Index

This file provides a comprehensive index to all documentation for the MaPLe (MPL) parallel functional programming language implementation.

## Runtime System Documentation

The MPL runtime system provides the core infrastructure for executing compiled MPL programs.

### Main Runtime Documentation

📂 **[runtime/README.md](runtime/README.md)** - **START HERE for runtime system**
- Complete overview of the runtime architecture
- Build process and configuration options
- Memory model (hierarchical heaps, chunks, disentanglement)
- Garbage collection overview
- Parallel primitives and scheduler support
- FFI and basis library integration
- Platform abstraction layer
- Debugging and performance tuning

### Garbage Collector (151 files)

📂 **[runtime/gc/README.md](runtime/gc/README.md)** - **Core GC and parallel runtime**

The heart of MPL's parallel memory management system:

**Key Topics**:
- Hierarchical heap management with union-find
- Concurrent local garbage collection
- Disentanglement checking (nearly-zero-cost entanglement detection)
- Work-stealing scheduler support (ABP deques)
- Epoch-based reclamation (EBR) for safe concurrent memory management

**Major Components**:
- `hierarchical-heap.{c,h}` - Hierarchical heap data structure
- `chunk.{c,h}` - Memory chunk management
- `hierarchical-heap-collection.{c,h}` - Concurrent local GC algorithm
- `decheck.{c,h}` - Disentanglement checking implementation
- `ebr.{c,h}`, `hierarchical-heap-ebr.{c,h}`, `entangled-ebr.{c,h}` - EBR systems
- `abp-deque.{c,h}` - Work-stealing deques
- `concurrent-collection.{c,h}` - Concurrent GC coordination

**See Also**: Detailed file-by-file documentation organized by category in the README.

### Basis Library FFI (~118 files)

📂 **[runtime/basis/README.md](runtime/basis/README.md)** - **C implementations of Standard ML basis library**

Foreign function interface bindings for the basis library:

**Modules**:
- **IntInf/** - Arbitrary-precision integer operations (GMP integration)
- **Real/** - Floating-point operations and math functions
- **Word/** - Unsigned integer (word) operations
- **Posix/** - POSIX API bindings (FileSys, IO, ProcEnv, Process, SysDB)
- **Net/Socket/** - Network socket operations
- **System/OS/** - OS-specific I/O operations
- **MLton/** - MLton/MPL-specific extensions
  - `Itimer/` - Interval timers
  - `Process/` - Process management
  - `Rlimit/` - Resource limits
  - `Rusage/` - Resource usage statistics
  - `Syslog/` - System logging

**Key Files**:
- `coerce.{c,h}` - Type coercion between ML and C
- `cpointer.{c,h}` - C pointer operations from ML
- `Stdio.c` - Standard I/O wrappers

### Platform Abstraction

📂 **[runtime/platform/README.md](runtime/platform/README.md)** - **Platform and architecture-specific code**

Abstracts differences between operating systems and CPU architectures:

**Operating Systems**:
- Linux, macOS (Darwin), Windows, Cygwin, MinGW
- BSD family: FreeBSD, NetBSD, OpenBSD
- Unix variants: Solaris, AIX, HP-UX, GNU/Hurd
- WebAssembly (WASI)

**Architectures** (header-only definitions):
- x86 family: x86 (IA-32), AMD64 (x86-64)
- ARM family: ARM (32-bit), ARM64 (AArch64)
- PowerPC: 32-bit, 64-bit
- RISC-V, MIPS, SPARC, IA-64, S/390
- Others: Alpha, PA-RISC, M68K, LoongArch64, WebAssembly

**Key Components**:
- Atomic operations: `atomics-gcc-gte48.h`, `atomics-gcc-lt48.h`
- Memory mapping: `mmap.c`, `mmap-protect.c`, `mremap.c`
- System utilities: `sysconf.c`, `sysctl.c`, `diskBack.unix.c`

### Utility Functions (15 files)

📂 **[runtime/util/README.md](runtime/util/README.md)** - **Low-level utility functions and macros**

Essential utilities used throughout the runtime:

- **Error Handling**: `die.{c,h}` - Fatal error reporting
- **Logging**: `log.{c,h}` - Debug and diagnostic output
- **Memory Utilities**:
  - `align.h` - Alignment operations
  - `pointer.h` - Pointer arithmetic helpers
  - `safe.h` - Safe wrappers for malloc, file I/O, etc.
- **Synchronization**: `spinlock.{c,h}` - Simple spinlock implementation
- **Type Conversion**: `to-string.{c,h}` - Formatted type-to-string conversion
- **Endianness**: `endian.h` - Byte order conversions
- **Debugging**: `valgrind.h` - Valgrind integration for custom allocators

### Floating-Point Conversion Library

📂 **[runtime/gdtoa/README.md](runtime/gdtoa/README.md)** - **David M. Gay's gdtoa library**

High-quality floating-point conversion routines:

**Purpose**:
- Correctly-rounded binary ↔ decimal conversion
- Shortest decimal representation that rounds back to original value
- Deterministic behavior across platforms

**Key Features**:
- Multiple precisions: single, double, extended, quad, double-double
- Thread-safe implementation
- Patches applied for MLton integration
- Used by `Real.toString`, `Real.fromString`, etc.

**Core Files**:
- `dtoa.c` - Binary → decimal (classic dtoa algorithm)
- `gdtoa.c` - Generalized binary → decimal
- `strtodg.c` - Generalized decimal → binary
- `strtod.c` - String to double (optimized)

## Basis Library (SML)

The Standard ML basis library implementation with MPL extensions.

### Main Basis Library

📂 **[basis-library/](basis-library/)** - SML basis library implementation
- Standard ML Basis Library
- MPL parallel extensions

### Parallel Scheduler

📂 **[basis-library/schedulers/spork/](basis-library/schedulers/spork/)** - Work-stealing scheduler

The core parallel execution engine for MPL:

**Key Files**:
- `Scheduler.sml` - Main scheduler implementation
- `ForkJoin.sml` - ForkJoin structure implementation
- `DequeABP.sml` - ABP work-stealing deque implementation

**Core Primitives**:
- `ForkJoin.par` - Fork-join parallelism
- `ForkJoin.parfor` - Parallel for loop (manual grain size)
- `ForkJoin.parform` - Parallel for loop (automatic grain size)
- `ForkJoin.reducem` - Parallel reduction (automatic grain size)
- `ForkJoin.alloc` - Allocate uninitialized array

## Compiler (mlton/)

The MPL compiler is based on MLton with extensions for parallelism.

### Main Compiler Documentation

📂 **[mlton/README.md](mlton/README.md)** - **START HERE for compiler architecture**

Complete compiler overview:
- Compilation pipeline (Source → CoreML → XML → SSA → SSA2 → Machine → C)
- Intermediate representation progression
- Pass organization and ordering
- Parallel extensions (fork-join, compile-time decisions)
- Development guide (adding passes, primitives, flags)
- Debugging compilation
- ~400+ compiler source files

**Quick Summary**:
The compiler performs whole-program optimization through multiple stages:
1. **Front-end**: Parse and elaborate to CoreML
2. **Defunctorize**: CoreML → XML (eliminate functors)
3. **Monomorphise**: XML → SXML (specialize polymorphism)
4. **Closure Convert**: SXML → SSA (first-order + closures)
5. **SSA Optimize**: Main optimization stage (20+ passes)
6. **SSA2 Optimize**: Final cleanup and backend prep
7. **Backend**: SSA2 → RSSA → Machine → C code

### SSA Optimization (Main Compiler Stage)

📂 **[mlton/ssa/README.md](mlton/ssa/README.md)** - **SSA IR and optimization passes** ⭐ Most important

The heart of compiler optimization (~81 source files):

**Key Topics**:
- SSA structure (blocks, statements, transfers, expressions)
- 20+ optimization passes in carefully chosen order
- Pass ordering rationale and dependencies
- Shrink cycles (optimize → shrink → optimize)
- Type system and type checking
- Parallel-specific passes (direct-exp, analyze, drop-spork)
- Compile-time optimization based on program analysis

**Major Pass Categories**:
- **Inlining**: inline.fun, contify.fun, known-case.fun
- **Constants**: constant-propagation.fun, common-subexp.fun
- **Data Rep**: flatten.fun, deep-flatten.fun, local-flatten.fun, ref-flatten.fun
- **Loops**: introduce-loops.fun, loop-invariant.fun, loop-unroll.fun, loop-unswitch.fun
- **Redundancy**: redundant.fun, redundant-tests.fun, useless.fun, remove-unused.fun
- **Types**: simplify-types.fun, poly-equal.fun, poly-hash.fun
- **Parallel**: direct-exp.fun, analyze.fun, drop-spork.fun
- **Cleanup**: shrink.fun, simplify.fun

**Development Guide**:
- How to add new optimization passes
- Pass patterns and common transformations
- Debugging passes with diagnostics
- Understanding pass interactions

**See Also**: SSA2 (simplified SSA), conversion to backend

### Compiler Control System

📂 **[mlton/control/README.md](mlton/control/README.md)** - **Compiler flags and configuration**

Central control and configuration (~4 key files):

**Topics**:
- Flag categories (optimization, diagnostics, codegen, parallel)
- Pass management (enable/disable, diagnostics)
- Diagnostic system (`-diag-pass`)
- Keeping intermediate files (`-keep-xml`, `-keep-ssa`, etc.)
- Error handling and type checking

**Adding a New Flag** (complete workflow):
1. Declare in control-flags.sig
2. Implement in control-flags.sml
3. Add CLI parsing in main/main.fun
4. Use in compiler passes

**Common Flags**:
- `-diag-pass <regex>` - View pass diagnostics
- `-keep-ssa`, `-keep-ssa2` - Keep intermediate representations
- `-disable-pass <name>`, `-enable-pass <name>` - Control passes
- `-spork-choose-threshold <n>` - Compile-time decision threshold
- `-stop {xml|ssa|ssa2|g}` - Stop after stage

**Example**: Complete walkthrough of `-spork-choose-threshold` flag from declaration to usage.

### Primitives and Atoms

📂 **[mlton/atoms/README.md](mlton/atoms/README.md)** - **Primitive operations and compiler atoms**

Fundamental compiler building blocks (~90 source files):

**Core Structures**:
- **Prim.t**: All primitive operations (~150+ primitives)
- **Var, Func, Label**: Variables, functions, block labels
- **Con, Tycon, Tyvar**: Constructors, type constructors, type variables
- **Const**: Constant values (int, real, word, string)
- **FFI**: C function interface (CFunction, CType, CSymbol)

**Primitive Categories**:
- Array operations (alloc, sub, update, CAS)
- Reference operations (ref, deref, assign, CAS)
- Word operations (arithmetic, bitwise, shifts - all sized)
- Real operations (arithmetic, math functions - sized)
- IntInf operations (arbitrary precision via GMP)
- Thread operations (atomic, switch, fork)
- GC operations (collect, state)
- Parallel operations (Spork, Spork_forkThreadAndSetData)

**Adding a New Primitive** (complete workflow):
1. Add constructor to Prim.t (prim.sig, prim.fun)
2. Add toString mapping
3. Implement checkApp (type checking)
4. Implement extractTargs, map (if polymorphic)
5. Add basis library declaration (`_prim "PrimName"`)
6. Implement in compiler passes (closure conversion, SSA, backend, or codegen)
7. Test and debug

**Example**: Complete example of adding a new primitive with all required steps.

### Front-End (Parsing and Lexing)

📂 **[mlton/front-end/README.md](mlton/front-end/README.md)** - **Lexing and parsing ML/MLB files**

The entry point to the compilation pipeline (~10 source files + generated code):

**Topics**:
- Standard ML lexing and parsing (ML-Lex + ML-Yacc)
- ML Basis (MLB) file parsing and path resolution
- Abstract Syntax Tree (AST) generation
- Source location tracking for error messages
- Line directive handling (`(*#line ...*)`)

**Key Files**:
- `ml.lex` (~800 lines) - ML-Lex specification for Standard ML
- `ml.grm` (~1,700 lines) - ML-Yacc grammar for Standard ML
- `mlb.lex` (~400 lines) - ML-Lex specification for ML Basis
- `mlb.grm` (~200 lines) - ML-Yacc grammar for ML Basis
- `front-end.fun` - ML source front-end driver
- `mlb-front-end.fun` - MLB front-end with path resolution

**MLB Path Resolution**:
- Path variables: `$(SML_LIB)`, `$(LIB_MLTON_DIR)`, etc.
- Configured via `-mlb-path-var` and `-mlb-path-map`
- Recursive file parsing with cycle detection
- Promise-based lazy evaluation

**Elaboration Flags**:
- `-allow-line-comments` - Enable `//` comments
- `-allow-extended-num-consts` - Binary literals `0b...`
- `-allow-extended-text-consts` - Unicode escapes `\u...`
- `-allow-opt-bar` - Optional `|` before first pattern
- `-allow-record-pun-exps` - Record punning `{x}` for `{x=x}`

**Development Guide**:
- How to modify the lexer (adding new tokens)
- How to modify the parser (adding new syntax)
- Debugging lexer/parser issues
- Managing shift/reduce conflicts

**See Also**: AST structures ([mlton/ast/](mlton/ast/)), Elaboration ([mlton/elaborate/](mlton/elaborate/))

### Compiler Architecture

The compiler uses a multi-stage pipeline with multiple intermediate representations:

**Compilation Pipeline**:
```
Source ML
    ↓
  AST (Abstract Syntax Tree)
    ↓ [Elaboration + Type Inference]
CoreML (Explicitly-typed ML)
    ↓ [Defunctorization]
  XML (Polymorphic, typed, first-order)
    ↓ [Monomorphisation]
 SXML (Monomorphic XML)
    ↓ [Closure Conversion + Compile-time decisions]
  SSA (Static Single Assignment - main optimization)
    ↓ [20+ optimization passes]
  SSA2 (Simplified SSA)
    ↓ [Backend preparation]
  RSSA (Explicit stack/runtime)
    ↓ [Backend transformations]
Machine (Low-level representation)
    ↓ [Code generation]
   C code
```

**Key Stages**:
1. **Front-end** (`mlton/front-end/`, `mlton/elaborate/`)
   - Parse source code to AST
   - Elaborate modules, type inference
   - Produces CoreML

2. **Defunctorize** (`mlton/defunctorize/`)
   - Eliminate functors and modules
   - Produces XML (polymorphic, first-order)

3. **Monomorphise** (`mlton/xml/monomorphise.fun`)
   - Specialize polymorphic functions
   - Produces SXML (monomorphic)

4. **Closure Convert** (`mlton/closure-convert/`)
   - Convert to first-order with explicit closures
   - Can perform compile-time optimizations based on program analysis
   - Produces SSA

5. **SSA Optimize** (`mlton/ssa/`) ⭐ **Main stage**
   - 20+ optimization passes
   - Inlining, contification, constant propagation
   - Data representation optimization (flattening)
   - Loop optimization
   - Parallel-specific transformations

6. **SSA2 Optimize** (`mlton/ssa/`)
   - Simplified SSA with restricted constructs
   - Final cleanup and backend preparation

7. **Backend** (`mlton/backend/`, `mlton/codegen/`)
   - SSA2 → RSSA → Machine → C code
   - Register allocation, stack management
   - C code generation (only supported backend for MPL)

### Directory Structure

```
mlton/
├── README.md                 (main compiler docs)
│
├── atoms/                    Primitive operations and basic atoms
│   ├── README.md             (primitives documentation)
│   ├── prim.sig/fun          Primitive operations (Prim.t)
│   ├── var.sig/fun           Variables
│   ├── func.sig              Functions
│   ├── con-.sig/fun          Constructors
│   ├── tycon.sig/fun         Type constructors
│   └── ...                   (~90 files)
│
├── control/                  Compiler flags and configuration
│   ├── README.md             (control system docs)
│   ├── control-flags.sig/sml Flag definitions
│   └── control.sig/sml       Control structure
│
├── front-end/                Lexing and parsing
│   ├── README.md             (front-end documentation)
│   ├── ml.lex                ML-Lex specification (Standard ML)
│   ├── ml.grm                ML-Yacc grammar (Standard ML)
│   ├── mlb.lex               ML-Lex specification (ML Basis)
│   ├── mlb.grm               ML-Yacc grammar (ML Basis)
│   ├── front-end.fun         ML source parser driver
│   └── mlb-front-end.fun     MLB parser with path resolution
│
├── ssa/                      SSA IR and optimization passes
│   ├── README.md             (SSA documentation) ⭐
│   ├── ssa.sig/fun           SSA data structures
│   ├── ssa2.sig/fun          SSA2 data structures
│   ├── simplify.fun          Main optimization driver
│   ├── simplify2.fun         SSA2 optimization driver
│   │
│   ├── inline.fun            Inlining
│   ├── contify.fun           Contification
│   ├── constant-propagation.fun
│   ├── common-subexp.fun     CSE
│   ├── flatten.fun           Data representation
│   ├── loop-invariant.fun    Loop optimization
│   │
│   ├── direct-exp.fun        Parallel primitives
│   ├── analyze.fun           Parallel analysis
│   ├── drop-spork.fun        Parallel optimization
│   └── ...                   (~81 files)
│
├── front-end/                Parsing and lexing
├── elaborate/                Type inference and modules
├── ast/                      Abstract syntax tree
├── core-ml/                  CoreML IR
├── defunctorize/             Functor elimination
├── xml/                      XML IR (polymorphic)
├── closure-convert/          Closure conversion
├── backend/                  Backend transformations
├── codegen/                  Code generation
└── main/                     Compiler driver
    ├── compile.fun           Main compilation pipeline
    └── main.fun              Entry point, CLI parsing
```

### Compiler Development

**Common tasks**:
- **Adding optimization passes**: See [mlton/ssa/README.md](mlton/ssa/README.md)
- **Adding primitives**: See [mlton/atoms/README.md](mlton/atoms/README.md)
- **Adding compiler flags**: See [mlton/control/README.md](mlton/control/README.md)
- **Understanding compilation**: See [mlton/README.md](mlton/README.md)

**Debugging compilation**:
```bash
# View IR at stages
mpl -stop ssa program.mlb        # Stop after SSA
mpl -keep-ssa program.mlb        # Keep SSA IR

# Pass diagnostics
mpl -diag-pass inline program.mlb         # See inlining
mpl -diag-pass closureConvert program.mlb # See closure conversion

# Keep IR after specific pass
mpl -keep-pass constantPropagation program.mlb
```

## Examples

📂 **[examples/](examples/)** - Example MPL programs

Demonstrates real parallel algorithms and MPL programming patterns.

**Build**:
```bash
cd examples/
make              # Build all examples
make fib          # Build specific example
bin/fib @mpl procs 4 -- -N 39
```

See `examples/README.md` for details on each example.

## Testing

📂 **[bin/regression](bin/regression)** - Regression test suite

```bash
./bin/regression       # Run all tests
make check             # Same as above
```

## Build System

**Main Makefile**: [Makefile](Makefile)

**Build Commands**:
```bash
make                    # Full build
make -j                 # Parallel build
make runtime            # Runtime only
make compiler           # Compiler only
make basis              # Basis library only
make examples           # Example programs
make clean              # Clean build artifacts
```

**Build Order**:
1. Runtime (C libraries)
2. Compiler (MLton → MPL compiler)
3. Basis library (SML sources)
4. Libraries (smlnj-lib, mllpt-lib, etc.)
5. Examples (optional)

## Configuration and Usage

### Compilation Options

Compile an MLB file:
```bash
mpl [options] foo.mlb
```

**Common compile-time options**:
- `-output foo` - Name the output executable
- `-default-type int64` - Use 64-bit integers by default
- `-default-type word64` - Use 64-bit words by default
- `-debug true` - Debug build
- `-debug-runtime true` - Use debug runtime library
- `-keep g` - Keep generated C files

### Runtime Options

Runtime arguments via `@mpl ... --`:

```bash
./program @mpl procs 4 set-affinity block-size 64K gc-summary -- <program-args>
```

**Common runtime options**:
- `procs <N>` - Number of processors (default: all cores)
- `set-affinity` - Pin threads to cores
- `block-size <size>` - Heap block size (default: 64K)
- `gc-summary` - Print GC statistics on exit
- `heartbeat-rate <N>` - Heartbeat interval for granularity control

See [runtime/README.md](runtime/README.md) for complete list.

## Development Guides

### Adding Runtime Features

See respective module documentation:
- [runtime/gc/README.md](runtime/gc/README.md) - GC modifications
- [runtime/basis/README.md](runtime/basis/README.md) - New FFI functions
- [runtime/platform/README.md](runtime/platform/README.md) - Platform support

### Modifying the Runtime

**Adding GC features**:
1. Declare in appropriate `gc/*.h` file
2. Implement in `gc/*.c` file
3. Export to basis library via `basis-ffi.h` if needed
4. Rebuild: `make runtime`

**Adding FFI functions**:
1. Create C implementation in `runtime/basis/`
2. Declare in `basis-ffi.h`
3. Add `_import` declaration in basis library `.sml` file
4. Rebuild: `make runtime && make basis`

### Compiler Development

**Key directories**:
- `mlton/control/` - Compiler flags and controls
- `mlton/atoms/` - Primitives and basic atoms
- `mlton/ssa/` - SSA optimization passes
- `mlton/backend/` - Code generation

**Common tasks**:
- Adding optimization passes
- Adding primitives
- Modifying code generation
- Type system modifications

## Debugging

### Runtime Debugging

See [runtime/README.md § "Debugging and Tracing"](runtime/README.md):

```bash
# Build with debug symbols
make clean && make DEBUG=true

# Run with GC debugging
./program @mpl gc-summary --

# Use gdb
gdb ./program
```

**GC debugging flags** (in `runtime/gc/debug.h`):
- `DEBUG` - General debugging output
- `DEBUG_DETAILED` - Verbose GC logging
- `DEBUG_THREADS` - Thread creation/destruction
- `DEBUG_DECHECK` - Disentanglement checking

### Compiler Debugging

```bash
# Keep intermediate files
mpl -debug true -keep g foo.mlb

# View IR at different stages
mpl -stop ssa foo.mlb        # Stop after SSA
mpl -stop ssa2 foo.mlb       # Stop after SSA2

# View pass diagnostics
mpl -diag-pass <passName> foo.mlb
```

**Useful diagnostic passes**:
- `closureConvert` - See closure conversion decisions
- `directExp` - See parallel primitive handling
- `inline` - See inlining decisions

## Performance Tuning

### Runtime Performance

See [runtime/README.md § "Performance Considerations"](runtime/README.md) and [runtime/gc/README.md § "Performance Tuning"](runtime/gc/README.md):

**Key tuning parameters**:
- Chunk size: `@mpl block-size <size>` (default: 64K)
  - Larger: Less metadata overhead, more memory waste
  - Smaller: Better granularity, more overhead
- Processor count: `@mpl procs <N>`
- Thread affinity: `@mpl set-affinity`
- Heartbeat rate: `@mpl heartbeat-rate <N>`

### Parallel Granularity

**Automatic granularity control** (recommended):
- Use `ForkJoin.parform` for parallel loops
- Use `ForkJoin.reducem` for parallel reductions
- Runtime automatically manages grain size

**Manual granularity control**:
- Use `ForkJoin.parfor` only when you have specific knowledge
- Requires careful tuning of grain size parameter

## Common Issues and Troubleshooting

See module-specific documentation:
- **GC issues**: [runtime/gc/README.md § "Common Issues and Debugging"](runtime/gc/README.md)
- **Platform issues**: [runtime/platform/README.md § "Common Issues"](runtime/platform/README.md)
- **FFI issues**: [runtime/basis/README.md § "Common Issues"](runtime/basis/README.md)

**Most common issues**:

### Entanglement Failures
**Symptom**: Program crashes or hangs, "entanglement detected" messages

**Cause**: Shared mutable state between parallel tasks

**Fix**: Avoid sharing mutable references across `ForkJoin.par`:
```sml
(* BAD: shared ref *)
val r = ref 0
val (a, b) = ForkJoin.par (fn () => r := 1, fn () => r := 2)

(* GOOD: no sharing *)
val (a, b) = ForkJoin.par (fn () => ref 1, fn () => ref 2)
```

### Memory Leaks
**Symptom**: Memory usage grows without bound

**Cause**: Root heap accumulation (GC disabled at top level by design)

**Debug**: Use `@mpl gc-summary` to see collection statistics

### Build Failures
**Symptom**: Compilation errors on specific platforms

**Cause**: Platform compatibility issues

**Fix**: Check [runtime/platform/README.md](runtime/platform/README.md) for platform-specific requirements

## Academic References

MPL implements algorithms described in academic papers on:
- Hierarchical memory management
- Disentanglement checking
- Concurrent local garbage collection
- Provably efficient automatic memory management

See the main repository README for links to academic papers.

## Getting Started

### New to MPL Runtime Development

1. Start with [runtime/README.md](runtime/README.md) - Overview
2. Browse [examples/](examples/) - Example programs
3. Deep dive into specific modules as needed:
   - GC internals → [runtime/gc/README.md](runtime/gc/README.md)
   - FFI → [runtime/basis/README.md](runtime/basis/README.md)
   - Platform code → [runtime/platform/README.md](runtime/platform/README.md)

### New to MPL Compiler Development

1. Explore compiler source in [mlton/](mlton/)
2. Read signature files (`.sig`) for module interfaces
3. Examine existing optimization passes for examples
4. Study the compilation pipeline from front-end to backend

### Writing MPL Programs

1. Study [examples/](examples/) for parallel programming patterns
2. Learn the `ForkJoin` structure primitives
3. Understand hierarchical memory management
4. Practice with small parallel programs
5. Use `@mpl gc-summary` to understand memory behavior

## Documentation Style

All documentation follows these conventions:
- **Markdown format** (GitHub-flavored)
- **Clear hierarchy** with headers
- **Practical focus** - what developers need to know
- **Cross-linking** - link to related docs
- **Examples** - show concrete usage
- **File references** - link to actual source files

## Contributing Documentation

When adding documentation:
1. Update relevant module README.md
2. Update this index
3. Cross-link related documentation
4. Follow existing style and format
5. Include practical examples
6. Document common issues and fixes

## Documentation Maintenance

- **Last updated**: 2025-11-18
- **Status**: Runtime documentation complete; compiler documentation in progress
- **Coverage**: ~350 source files documented across runtime modules

---

**Quick Navigation**: [Top](#mpl-documentation-index) | [Runtime](#runtime-system-documentation) | [Basis Library](#basis-library-sml) | [Compiler](#compiler-mlton) | [Examples](#examples)
