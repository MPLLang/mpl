# MPL Runtime System

The MPL runtime system provides the core infrastructure for running compiled MPL programs, including the hierarchical garbage collector, parallel scheduling support, and platform-specific interfaces.

## Overview

The runtime is based on the MLton runtime with significant extensions for:
- **Hierarchical Memory Management**: Parent-child heap organization with union-find for merging
- **Concurrent Local GC**: Per-thread local collection with disentanglement checking
- **Parallel Primitives**: Support for fork-join parallelism and work-stealing
- **Entanglement Detection**: Nearly-zero-cost detection of shared mutable references between heaps

## Directory Structure

- **[gc/](gc/README.md)** - Core garbage collector and parallel runtime (151 files)
  - Hierarchical heap management
  - Concurrent collection algorithms
  - Disentanglement checking
  - Work-stealing scheduler support (ABP deques)
  - Epoch-based reclamation (EBR)

- **[basis/](basis/README.md)** - FFI implementations for basis library structures
  - MLton extensions (parallel, GC interface)
  - Posix APIs
  - Network I/O
  - Integer/Real/Word operations

- **[platform/](platform/README.md)** - Platform-specific code
  - OS abstractions (Linux, Darwin, Windows, etc.)
  - Architecture-specific definitions (x86, ARM, RISC-V, etc.)
  - Memory mapping and protection
  - Atomics implementations

- **[util/](util/README.md)** - Utility functions
  - Logging and debugging
  - Safe allocation wrappers
  - String conversion
  - Spinlocks

- **[gdtoa/](gdtoa/README.md)** - Floating-point to/from string conversion
  - Based on David M. Gay's gdtoa library
  - Patched for MLton/MPL integration

- **gen/** - Generated files (build artifacts)

## Key Header Files

- **gc.h** - Main GC interface, includes all GC subsystem headers
- **platform.h** - Platform detection and basic types
- **basis-ffi.h** - FFI declarations for basis library
- **gc.c** - Main GC entry point
- **ml-types.h** - Generated ML type mappings
- **c-types.h** - Generated C type mappings

## Build Process

The runtime is built as static libraries with multiple configurations:

```bash
make runtime              # Build all runtime configurations
```

### Build Configurations

Libraries are built in multiple variants (examples: `libmlton.a`, `libmlton-dbg.a`, etc.):

- **Standard** - Optimized build
- **dbg** - Debug symbols, assertions enabled
- **detect** - Entanglement detection enabled
- **trace** - Tracing/profiling enabled
- **dbgdetect** - Both debug and detection
- **tracedetect** - Both tracing and detection
- **npi** - "No position independent" (non-PIC code)

The final executable links against one of these variants based on compile-time flags.

## Memory Model

### Hierarchical Heaps

The runtime organizes memory in a tree of heaps:

```
        Root Heap (shared)
              |
     +--------+--------+
     |                 |
  Thread 1 Heap    Thread 2 Heap
     |                 |
   Join Heap         Join Heap
     |                 |
  Nested Par        Nested Par
```

- Each `ForkJoin.par` creates child heaps
- Heaps are merged at join points using union-find
- Local GC can collect thread-local heaps independently
- Entanglement tracking prevents unsafe local GCs

### Chunks

Memory is allocated in chunks (configurable size, default ~1MB):
- Chunks contain a metadata header + user objects
- Organized into level lists (one per heap depth)
- All objects begin in the first block of a chunk
- Chunk metadata can be found by masking any object pointer

### Disentanglement

The runtime tracks whether heaps share mutable references:
- Uses `decheck_tid_t` (depth + path encoding)
- Nearly zero-cost: single 64-bit load per object access
- Detects "entanglement" and prevents unsafe local GCs
- See [gc/decheck.c](gc/decheck.c) for implementation

## Garbage Collection

### Local Collection
- Per-thread, concurrent collection of thread-local heaps
- Uses Cheney-style copying collection within a level
- Remembered sets track cross-level pointers
- Disabled when entanglement detected

### Root Collection
- Global stop-the-world collection (fallback)
- Collects entire heap hierarchy
- Used when local collections are insufficient

### Epoch-Based Reclamation (EBR)
- Safe concurrent memory reclamation
- Three separate EBR systems:
  - `ebr.{c,h}` - General-purpose EBR
  - `hierarchical-heap-ebr.{c,h}` - For heap metadata
  - `entangled-ebr.{c,h}` - For entanglement suspects

## Parallel Scheduler Support

The runtime provides low-level primitives for the user-level scheduler:

- **ABP Deques** ([gc/abp-deque.{c,h}](gc/abp-deque.c)): Arora-Blumofe-Plaxton work-stealing deques
- **Atomic Operations** ([gc/atomic.{c,h}](gc/atomic.c)): CAS, fetch-and-add
- **Processor Management** ([gc/processor.{c,h}](gc/processor.c)): Thread affinity, core counts
- **Thread Primitives** ([gc/thread.{c,h}](gc/thread.c)): Thread objects and state

The actual scheduler is implemented in ML: [basis-library/schedulers/spork/](../basis-library/schedulers/spork/)

## FFI and Basis Library

C implementations of basis library functions are in `basis/`:

- **MLton extensions**: Parallel primitives, GC interface, process control
- **Standard basis**: File I/O, network sockets, POSIX APIs
- **Coercion functions**: Type conversions between ML and C

All FFI functions are declared in `basis-ffi.h`.

## Platform Abstraction

Platform-specific code in `platform/` provides:

- Memory mapping (`mmap`, `mmap-protect`, `mremap`)
- OS-specific initialization (Linux, macOS, Windows, etc.)
- Architecture-specific definitions (word size, alignment, byte order)
- Atomic operation implementations (GCC >= 4.8, GCC < 4.8)

The build system automatically selects the correct platform files.

## Debugging and Tracing

### Compile-Time Options
- `ASSERT` - Enable runtime assertions
- `DETECT_ENTANGLEMENT` - Enable disentanglement checking
- `MPL_TRACING` - Enable event tracing

### Runtime Options
MPL programs accept runtime arguments via `@mpl ... --`:

```bash
./program @mpl procs 4 set-affinity gc-summary -- <program args>
```

Key options:
- `procs <N>` - Number of processors
- `set-affinity` - Pin threads to cores
- `block-size <size>` - Heap block size (e.g., `64K`, `1M`)
- `gc-summary` - Print GC statistics on exit
- `heartbeat-rate <N>` - Heartbeat interval for granularity control

See `gc/controls.{c,h}` for all runtime options.

## Key Data Structures

### GC_state
Central GC state structure, one per program execution:
- Global heap metadata
- Processor management
- Signal handlers
- Runtime controls

### GC_thread
Per-thread state:
- Current heap hierarchy path
- Stack information
- Decheck TID (for entanglement)
- Hierarchical heap pointer

### HM_HierarchicalHeap
Heap metadata for one level:
- Union-find node for merging
- Chunk list
- Remembered set
- Concurrent collection state
- Entanglement suspects

### HM_chunk
Chunk metadata:
- Level head pointer
- Frontier/limit pointers
- Next/prev in level list
- Decheck state
- Pin status

## Development Workflow

### Adding Runtime Features

1. **Declare in header** - Add to appropriate `gc/*.h` file
2. **Implement in C** - Add to `gc/*.c` file
3. **Export to basis** - Add to `basis-ffi.h` if needed
4. **Rebuild runtime** - `make runtime`

### Debugging Runtime Issues

```bash
# Build with debug symbols
make clean
make -j runtime DEBUG=true

# Run with GC debugging
./program @mpl gc-summary --

# Use gdb
gdb ./program
(gdb) run @mpl procs 1 --
```

### Common Debugging Flags

In `gc/debug.h`:
- `DEBUG` - General debugging output
- `DEBUG_DETAILED` - Verbose GC logging
- `DEBUG_THREADS` - Thread creation/destruction
- `DEBUG_DECHECK` - Disentanglement checking

## Performance Considerations

### Chunk Size
- Larger chunks: Less metadata overhead, more memory waste
- Smaller chunks: Better granularity, more overhead
- Default: 512KB (blocks of 64KB × 8)
- Tune with `@mpl block-size ...`

### Processor Count
- More processors: Better parallelism, more GC overhead
- Use `procs` to limit parallelism
- `set-affinity` can improve cache locality

### Heartbeat Rate
- Controls granularity of automatic work-stealing decisions
- Higher rate: More responsive, more overhead
- Lower rate: Less overhead, coarser granularity

## Related Documentation

- **Compiler**: [../mlton/](../mlton/) - MPL compiler (based on MLton)
- **Basis Library**: [../basis-library/](../basis-library/) - Standard ML basis + MPL extensions
- **Scheduler**: [../basis-library/schedulers/spork/](../basis-library/schedulers/spork/) - Work-stealing scheduler
- **Examples**: [../examples/](../examples/) - Example MPL programs

## References

- **MPL Papers**: See main repository README for academic papers
- **MLton**: http://mlton.org - Original MLton compiler
- **GC Algorithms**: Disentangled memory management and concurrent collection
