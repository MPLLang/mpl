# MPL Garbage Collector

The `runtime/gc/` directory contains the core garbage collection and parallel runtime system for MPL. This includes hierarchical memory management, concurrent local GC, disentanglement checking, and work-stealing scheduler support.

**Files**: 74 C source files, 77 headers (151 total)

## Architecture Overview

The MPL GC is a **hierarchical, concurrent garbage collector** with:

1. **Hierarchical Heaps**: Tree-structured heap organization matching program parallelism structure
2. **Local Collection**: Per-thread concurrent collection of thread-local heaps
3. **Disentanglement**: Nearly-zero-cost detection of shared mutable state between heaps
4. **Union-Find**: Efficient heap merging at parallel join points
5. **Epoch-Based Reclamation**: Safe concurrent memory reclamation for metadata

## Module Organization

### Core Data Structures

#### Hierarchical Heap Management
- **[hierarchical-heap.{c,h}](hierarchical-heap.c)** - Core hierarchical heap structure
  - `HM_HierarchicalHeap`: Heap metadata for one level in the hierarchy
  - `HM_UnionFindNode`: Union-find structure for heap merging
  - Tree of heaps matching program parallelism structure
  - Path-copied per-thread views merged at join points

- **[chunk.{c,h}](chunk.c)** - Memory chunk management
  - `HM_chunk`: Contiguous memory regions containing objects
  - Organized into level lists (one per heap depth)
  - Crucial invariant: All objects begin in first block of chunk
  - Chunk metadata lookup via pointer masking

- **[local-heap.{c,h}](local-heap.c)** - Per-thread local allocation state
  - Thread-local allocation frontier
  - Integration with hierarchical heap
  - Fast-path allocation

- **[local-scope.{c,h}](local-scope.c)** - Scope-based heap management
  - Managing heap scopes for parallel constructs
  - Scope entry/exit operations

#### Garbage Collection Algorithms

- **[hierarchical-heap-collection.{c,h}](hierarchical-heap-collection.c)** - Local GC implementation
  - Concurrent local collection algorithm
  - Cheney-style copying within a heap level
  - Promotion to ancestor heaps
  - Pinning for objects escaping local scope
  - Integration with disentanglement checking

- **[concurrent-collection.{c,h}](concurrent-collection.c)** - Concurrent GC coordination
  - `ConcurrentPackage`: State for concurrent collections
  - Coordination between concurrent collectors
  - Work-stealing for collection work

- **[garbage-collection.{c,h}](garbage-collection.c)** - Main GC entry points
  - Top-level GC invocation
  - Root collection (stop-the-world fallback)
  - GC policy decisions

- **[remembered-set.{c,h}](remembered-set.c)** - Remembered set for cross-level pointers
  - Tracks pointers from ancestor heaps to descendant heaps
  - Used during local collections
  - Prevents missed live objects

#### Disentanglement and Entanglement Detection

- **[decheck.{c,h}](decheck.c)** - Disentanglement checking
  - `decheck_tid_t`: 64-bit thread ID (depth + path encoding)
  - Nearly-zero-cost: single load per object access
  - Detects when heaps share mutable references
  - `decheck()`: Check if object is disentangled from current thread
  - `lcaHeapDepth()`: Compute lowest common ancestor depth

- **[entanglement-suspects.{c,h}](entanglement-suspects.c)** - Tracking potential entanglement
  - Maintains list of objects that might be entangled
  - Used to defer handling of entanglement
  - Integration with local GC

#### Epoch-Based Reclamation (EBR)

- **[ebr.{c,h}](ebr.c)** - General-purpose EBR
  - Safe concurrent memory reclamation
  - Grace periods for deferred frees
  - Lock-free read-side critical sections

- **[hierarchical-heap-ebr.{c,h}](hierarchical-heap-ebr.c)** - EBR for heap metadata
  - Specialized EBR for `HM_HierarchicalHeap` objects
  - Ensures safe reclamation after heap merges

- **[entangled-ebr.{c,h}](entangled-ebr.c)** - EBR for entanglement suspects
  - Specialized EBR for entanglement suspect lists
  - Safe concurrent access to suspect lists

### Object Model

#### Object Representation
- **[object.{c,h}](object.c)** - Object header and layout
  - Object header format (tag, metadata)
  - Object type queries (array, normal, weak, etc.)
  - Mutable vs immutable objects

- **[object-size.{c,h}](object-size.c)** - Object size computation
  - Size of objects including headers
  - Payload size calculations
  - Alignment requirements

- **[objptr.{c,h}](objptr.c)** - Object pointer operations
  - `objptr`: Typed object pointer
  - Pointer validation
  - Conversions to/from C pointers

- **[pointer.{c,h}](pointer.c)** - Raw pointer utilities
  - Pointer arithmetic
  - Alignment operations
  - Pointer comparisons

#### Type-Specific Objects

- **[sequence.{c,h}](sequence.c)** - Sequences (arrays, vectors, strings)
  - Sequence header format
  - Length encoding
  - Element access

- **[sequence-allocate.{c,h}](sequence-allocate.c)** - Sequence allocation
  - Allocation of arrays, vectors, strings
  - Initialization strategies

- **[string.{c,h}](string.c)** - String operations
  - String utilities
  - Integration with GC

- **[int-inf.{c,h}](int-inf.c)** - Arbitrary-precision integers
  - IntInf (big integer) representation
  - GMP integration
  - Allocation and conversion

- **[weak.{c,h}](weak.c)** - Weak pointers
  - Weak reference support
  - Finalization

#### Special Objects

- **[thread.{c,h}](thread.c)** - Thread objects
  - `GC_thread`: Per-thread GC state
  - Thread creation and destruction
  - Stack management
  - Hierarchical heap path

- **[stack.{c,h}](stack.c)** - Stack representation
  - Stack layout and metadata
  - Stack scanning for GC roots
  - Stack resizing

- **[frame.{c,h}](frame.c)** - Stack frames
  - Frame layout and traversal
  - Return address handling
  - Frame offsets

- **[call-stack.{c,h}](call-stack.c)** - Call stack management
  - Stack walking
  - Exception handling support

### Memory Management

#### Allocation

- **[new-object.{c,h}](new-object.c)** - Object allocation
  - Main allocation entry point
  - Small object allocation (bump-pointer)
  - Large object allocation
  - Integration with local heap

- **[block-allocator.{c,h}](block-allocator.c)** - Block-level memory allocator
  - Allocates memory blocks (fixed-size chunks)
  - SuperBlock management
  - Free block lists
  - Integration with OS memory mapping

- **[fixed-size-allocator.{c,h}](fixed-size-allocator.c)** - Fixed-size object allocator
  - Fast allocation for fixed-size objects
  - Free list management

- **[heap.{c,h}](heap.c)** - Heap structure (legacy MLton compatibility)
  - Global heap state
  - Heap resizing
  - Heap limits

#### GC Operations

- **[forward.{c,h}](forward.c)** - Object forwarding
  - Forwarding pointers for copying GC
  - Forward object during collection
  - Check if object is forwarded

- **[foreach.{c,h}](foreach.c)** - Heap traversal
  - Traverse all objects in heap
  - Apply function to each object
  - Used for collection, statistics, etc.

- **[pin.{c,h}](pin.c)** - Object pinning
  - Pin objects to prevent collection
  - Unpin after local GC
  - Integration with chunk metadata

- **[copy-thread.{c,h}](copy-thread.c)** - Thread copying
  - Copy thread object during GC
  - Update thread references

- **[assign.{c,h}](assign.c)** - Assignment operations
  - Write barrier for mutable updates
  - Remembered set updates
  - Entanglement detection on write

- **[deferred-promote.{c,h}](deferred-promote.c)** - Deferred promotion
  - Delay promotion of objects during GC
  - Batching promotions for efficiency

### Concurrent Data Structures

- **[abp-deque.{c,h}](abp-deque.c)** - ABP work-stealing deque
  - Arora-Blumofe-Plaxton algorithm
  - Lock-free work stealing
  - Used by parallel scheduler (in basis library)

- **[concurrent-list.{c,h}](concurrent-list.c)** - Concurrent linked list
  - Lock-free list operations
  - Used for various metadata structures

- **[concurrent-stack.{c,h}](concurrent-stack.c)** - Concurrent stack
  - Lock-free stack operations
  - Used for free lists, work lists

- **[cc-work-list.{c,h}](cc-work-list.c)** - Concurrent collection work list
  - Work list for concurrent GC
  - Load balancing for collection work

### Synchronization and Atomics

- **[atomic.{c,h}](atomic.c)** - Atomic operations
  - Compare-and-swap (CAS)
  - Fetch-and-add
  - Atomic loads/stores
  - Memory barriers

- **[processor.{c,h}](processor.c)** - Processor management
  - Number of processors
  - Thread affinity
  - Processor ID queries

- **[parallel.{c,h}](parallel.c)** - Parallel runtime support
  - Parallel primitive implementations
  - Integration with scheduler

### System Integration

#### Initialization and Lifecycle

- **[init.{c,h}](init.c)** - GC initialization
  - Initialize GC state
  - Setup initial heap
  - Platform-specific init

- **[init-world.{c,h}](init-world.c)** - World initialization
  - Initialize complete GC state
  - Load heap from binary
  - Setup static data

- **[done.{c,h}](done.c)** - GC finalization
  - Cleanup on program exit
  - Print GC statistics
  - Free resources

- **[gc_state.{c,h}](gc_state.c)** - Global GC state
  - `GC_state` structure
  - Global GC parameters
  - State initialization

#### Runtime Controls

- **[controls.{c,h}](controls.c)** - Runtime control parameters
  - Command-line argument parsing (`@mpl ... --`)
  - GC tuning parameters
  - Debugging flags

- **[sysvals.{c,h}](sysvals.c)** - System values
  - Page size, cache sizes
  - Platform capabilities

#### Signals and Interrupts

- **[signals.{c,h}](signals.c)** - Signal handling
  - Unix signal integration
  - GC-safe signal delivery

- **[handler.{c,h}](handler.c)** - Signal handlers
  - Handler installation
  - Handler invocation

- **[enter_leave.{c,h}](enter_leave.c)** - GC entry/exit
  - Enter/leave GC from mutator
  - Synchronization for concurrent GC

### Utilities and Helpers

#### Memory Operations

- **[align.{c,h}](align.c)** - Alignment operations
  - Align pointers to boundaries
  - Check alignment

- **[model.{c,h}](model.c)** - Memory model
  - Word size, pointer size
  - Platform-specific sizes

- **[gap.{c,h}](gap.c)** - Gap management in chunks
  - Manage gaps in chunk allocation
  - Used for metadata at chunk front

- **[pack.{c,h}](pack.c)** - Heap packing
  - Compact heap representation
  - Reduce memory fragmentation

- **[share.{c,h}](share.c)** - Heap sharing
  - Share immutable data between heaps
  - Deduplication

- **[size.{c,h}](size.c)** - Size computations
  - Heap size queries
  - Memory usage statistics

#### Debugging and Diagnostics

- **[debug.h](debug.h)** - Debugging macros
  - Conditional compilation for debug code
  - Debug output macros

- **[logger.{c,h}](logger.c)** - Event logging
  - Structured event logging
  - Performance analysis

- **[sampler.{c,h}](sampler.c)** - Statistical sampling
  - Sample heap state
  - Profiling support

- **[time-histogram.{c,h}](time-histogram.c)** - Time measurements
  - Histogram of GC pause times
  - Latency analysis

- **[tracing-hooks.{c,h}](tracing-hooks.c)** - Event tracing
  - Compile-time tracing hooks
  - Integration with external tracers

- **[profiling.{c,h}](profiling.c)** - Profiling support
  - Allocation profiling
  - Time profiling

#### Statistics and Monitoring

- **[statistics.{c,h}](statistics.c)** - GC statistics
  - Collection counts
  - Memory usage
  - Pause times
  - Runtime statistics reporting

- **[rusage.{c,h}](rusage.c)** - Resource usage
  - CPU time, memory usage (via getrusage)
  - Integration with statistics

- **[invariant.{c,h}](invariant.c)** - Invariant checking
  - Check GC data structure invariants
  - Debugging aid

#### Miscellaneous

- **[current.{c,h}](current.c)** - Current thread queries
  - Get current thread object
  - Thread-local state access

- **[switch-thread.{c,h}](switch-thread.c)** - Thread switching
  - Switch execution to different thread
  - Stack switching

- **[termination.{c,h}](termination.c)** - Termination detection
  - Detect when parallel computation completes
  - Scheduler integration

- **[world.{c,h}](world.c)** - World save/restore
  - Heap snapshots (limited support)
  - State serialization

- **[sources.{c,h}](sources.c)** - Source-level info
  - Source code location tracking
  - Debugging information

- **[static-heaps.{c,h}](static-heaps.c)** - Static data management
  - Global/static SML values
  - Embedded in binary

- **[tls-objects.{c,h}](tls-objects.c)** - Thread-local storage
  - TLS for GC state
  - Platform-specific TLS

- **[gdtoa-multiple-threads-defs.{c,h}](gdtoa-multiple-threads-defs.c)** - gdtoa threading support
  - Thread-safe gdtoa integration

- **[major.{c,h}](major.c)** - Major GC (legacy)
  - Major collection (mostly superseded by hierarchical GC)

## Key Algorithms

### Hierarchical Heap Management

The heap hierarchy is a tree where:
- **Nodes** are `HM_HierarchicalHeap` objects representing heap levels
- **Edges** represent parent-child relationships from `ForkJoin.par`
- **Union-Find** merges sibling heaps at join points
- **Path Copying** gives each thread its own leaf-to-root view

```
Initial:           After fork:          After join (union):
   Root               Root                    Root
                     /    \                  /    |
                Thread1  Thread2   ==>   Merged  |
```

Union-find nodes (`HM_UnionFindNode`) support:
- `find()`: Get representative (path compression)
- `union()`: Merge two heaps (union by rank)
- Dependant tracking: Tree of dead union-find nodes for GC

### Local Garbage Collection

Local GC collects a single heap level concurrently:

1. **Check disentanglement**: Ensure no shared mutable state with other threads
2. **Scan roots**: Thread stack, remembered set from ancestors
3. **Copy live objects**: Cheney-style breadth-first copy
4. **Promote escaping objects**: Move to ancestor heap or pin in place
5. **Update pointers**: Forward all references
6. **Reclaim memory**: Return chunks to free list

Key property: **Disentangled** heaps can be collected independently without synchronization.

### Disentanglement Checking

Each chunk has a `decheck_tid_t` (64-bit):
```c
typedef union {
  struct {
    uint32_t path;   // Unique path in fork-join tree
    uint32_t depth;  // Heap depth
  } internal;
  uint64_t bits;
} decheck_tid_t;
```

Check if object is disentangled:
```c
bool decheck(GC_state s, objptr op) {
  decheck_tid_t objTid = getChunkOf(op)->decheckState;
  decheck_tid_t myTid = getThreadCurrent(s)->decheckState;
  return decheckIsOrdered(myTid, objTid); // Check depth + path compatibility
}
```

Nearly zero cost: Single 64-bit load, comparison, and depth check.

### Concurrent Collection

Multiple threads can collect their local heaps concurrently:
- Each thread collects its own leaf heap
- Disentanglement ensures no conflicts
- Remembered sets track cross-heap pointers
- EBR reclaims metadata safely after collection

## Key Invariants

1. **Chunk Object Invariant**: All objects begin in the first block of their chunk
2. **Levelhead Invariant**: Each level list has exactly one levelhead chunk
3. **Disentanglement Invariant**: If `decheck(obj)` returns true, obj is not shared with other threads
4. **Union-Find Invariant**: Each union-find tree has exactly one representative
5. **Remembered Set Invariant**: All pointers from ancestors to descendants are in remembered sets
6. **EBR Invariant**: No thread accesses memory after it enters EBR retire queue

## Build Configuration

The GC is compiled with multiple configuration flags:

### Debug Flags
- `ASSERT` - Enable runtime assertions
- `DEBUG` - Enable debug output
- `DEBUG_DETAILED` - Verbose GC logging

### Feature Flags
- `DETECT_ENTANGLEMENT` - Enable disentanglement checking (recommended)
- `MPL_TRACING` - Enable event tracing hooks
- `PROFILE` - Enable profiling support

### Platform Flags
- Set automatically by build system based on target platform
- Control memory model, atomics, etc.

## Usage from Compiler

The compiler generates C code that calls GC functions:

```c
// Allocation
objptr obj = GC_allocate(s, size, tag);

// Thread operations
GC_HH_decheckFork(s, &leftTid, &rightTid);  // Before fork
GC_HH_decheckJoin(s, leftTid, rightTid);    // After join

// Explicit GC (rare)
GC_collect(s, bytesRequested, force);
```

Most GC operations are automatic (allocation triggers collection when needed).

## Performance Tuning

### Runtime Parameters

Key `@mpl` parameters (see [controls.h](controls.h)):

- `procs <N>` - Number of processors (default: all cores)
- `block-size <size>` - Heap block size (default: 64K)
  - Larger: Less metadata overhead, more waste
  - Smaller: Finer granularity, more overhead
- `gc-summary` - Print GC stats on exit
- `heartbeat-rate <N>` - Scheduler heartbeat interval

### Compilation Flags

MPL compiler flags affecting GC:
- `-debug-runtime true` - Use debug runtime library
- `-detect-entanglement true` - Enable disentanglement (recommended)

## Common Issues and Debugging

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

**Cause**:
- Root heap accumulating objects (no GC at top level by design)
- Retained references preventing collection

**Debug**: Use `@mpl gc-summary` to see collection statistics

### Slow GC
**Symptom**: Long GC pauses

**Cause**:
- Large heap levels
- Frequent entanglement (falling back to global GC)
- Inefficient chunk size

**Fix**:
- Tune `block-size`
- Reduce heap retention
- Increase granularity of parallelism

### Assertion Failures
**Symptom**: Runtime assertion failure (in debug builds)

**Cause**: GC invariant violated (bug in GC or generated code)

**Debug**:
1. Build with `DEBUG=true`
2. Run under `gdb`
3. Check `invariant.c` for which invariant failed
4. Report bug with backtrace

## References

See main repository README and academic papers for:
- Hierarchical memory management algorithm
- Disentanglement checking algorithm
- Concurrent local GC algorithm
- Union-find for heap merging

## See Also

- [../README.md](../README.md) - Runtime system overview
- [../basis/README.md](../basis/README.md) - Basis library FFI
- [../../basis-library/schedulers/spork/](../../basis-library/schedulers/spork/) - Work-stealing scheduler
- [../../mlton/](../../mlton/) - MPL compiler
