# Documentation Progress Checkpoint

This file tracks progress on documenting the MPL codebase. Use this to resume work if context is lost.

## Overall Goal
Create comprehensive markdown documentation for each major subsystem in the MPL runtime and compiler.

## Completed

### Runtime Documentation
- ✅ **runtime/README.md** - Overview of entire runtime system
  - Architecture overview
  - Build process and configurations
  - Memory model (hierarchical heaps, chunks, disentanglement)
  - GC overview
  - Parallel scheduler support
  - FFI and basis library
  - Platform abstraction
  - Debugging and performance tuning

- ✅ **runtime/gc/README.md** - Complete GC subsystem documentation (151 files)
  - Organized into categories: Core data structures, GC algorithms, object model, memory management, concurrent data structures, synchronization, system integration, utilities
  - Each file documented with purpose and key functions
  - Key algorithms explained (hierarchical heap, local GC, disentanglement)
  - Invariants documented
  - Performance tuning guide
  - Common issues and debugging

- ✅ **runtime/basis/README.md** - Basis library FFI implementations (~118 files)
  - FFI conventions and type conversions
  - Organized by module: IntInf, Real, Word, Posix, Net, System, MLton
  - POSIX APIs (FileSys, IO, ProcEnv, Process, SysDB)
  - Network sockets
  - MLton extensions (Itimer, Process, Rlimit, Rusage, Syslog)
  - Error handling patterns
  - How to add new FFI functions

- ✅ **runtime/platform/README.md** - Platform-specific code
  - OS abstraction (Linux, Darwin, BSD, Windows, etc.)
  - Architecture support (x86, ARM, RISC-V, PowerPC, MIPS, etc.)
  - Atomic operations (GCC >=4.8 and <4.8)
  - Memory mapping (mmap, mprotect, mremap)
  - System utilities
  - Platform detection and configuration

- ✅ **runtime/util/README.md** - Utility functions (15 files)
  - Error handling (die.c)
  - Logging (log.c)
  - Memory utilities (align.h, pointer.h, safe.h)
  - Synchronization (spinlock.c)
  - Type conversions (to-string.c)
  - Endianness (endian.h)
  - Valgrind integration

- ✅ **runtime/gdtoa/README.md** - Floating-point conversion library
  - David M. Gay's gdtoa library
  - Correctly-rounded binary↔decimal conversion
  - Support for multiple precisions
  - Integration with MLton/MPL
  - Patches applied for thread-safety
  - Algorithms and references

- ✅ **DOCUMENTATION_INDEX.md** - Comprehensive documentation index
  - Quick links to all documentation
  - Organized by subsystem (runtime, basis library, compiler)
  - Getting started guides for runtime and compiler development
  - Cross-references and navigation aids
  - Common issues and troubleshooting
  - Development workflow guides

## Summary

**RUNTIME DOCUMENTATION COMPLETE!** ✅

All major runtime subsystems have been documented:
- 6 module-specific READMEs (gc, basis, platform, util, gdtoa, main runtime)
- 1 comprehensive documentation index
- ~350 source files covered across all modules
- Developer-focused with practical examples and troubleshooting

## In Progress

None - Runtime documentation complete!

## TODO - Runtime

### Optional Future Enhancements
- ⬜ **runtime/gen/README.md** (very brief, low priority)
  - Generated files directory (build artifacts)

- ⬜ **runtime/gdtoa/README.md** - Floating-point conversion
  - Based on David M. Gay's gdtoa library
  - Patches applied for MLton integration
  - Thread-safety modifications

- ⬜ **runtime/gen/README.md** - Generated files (brief, just explain purpose)

## TODO - Basis Library

- ⬜ **basis-library/README.md** - Overview of basis library structure
  - Standard ML basis library implementation
  - MPL extensions (parallel primitives)
  - Organization by module

- ⬜ **basis-library/schedulers/spork/README.md** - Spork scheduler documentation
  - Work-stealing scheduler
  - ForkJoin implementation
  - ABP deques
  - Heartbeat-based granularity control

## TODO - Compiler (mlton/)

Need to create a comprehensive plan for documenting the compiler. The compiler is large and complex, with multiple IRs and many passes.

### Suggested Structure for mlton/ Documentation

1. **mlton/README.md** - Compiler overview
   - Compilation pipeline
   - IR progression (Front-end → XML → SSA → SSA2 → Backend)
   - Pass ordering
   - How to add new passes
   - How to add primitives

2. **mlton/front-end/README.md** - Front-end (parsing, elaboration)
   - ML source → CoreML
   - Type checking and inference
   - Module system

3. **mlton/elaborate/README.md** - Elaboration
   - Type inference
   - Module elaboration

4. **mlton/core-ml/README.md** - CoreML IR

5. **mlton/xml/README.md** - XML IR
   - First typed intermediate language
   - Defunctorization

6. **mlton/ssa/README.md** - SSA IR (main optimization passes)
   - SSA form
   - All optimization passes (inlining, contification, known-case, etc.)
   - Parallel-specific passes (direct-exp, analyze, drop-spork, etc.)
   - Closure conversion (handles loop_choose, spork_choose)

7. **mlton/backend/README.md** - Backend and codegen
   - Machine representation
   - C codegen
   - Register allocation

8. **mlton/control/README.md** - Compiler flags and controls
   - How to add compile-time flags
   - Control flow

9. **mlton/atoms/README.md** - Basic compiler atoms (primitives, types, etc.)
   - Prim.t - primitive operations
   - How to add new primitives

### Key Areas for Compiler Documentation

Focus on developer-facing documentation:
- How to add new optimization passes
- How to add new primitives (like loop_choose, spork_choose)
- How to add compile-time flags
- How the parallel primitives are handled through the pipeline
- IR transformations at each stage
- Type system and type checking

## Documentation Style Guidelines

Each README.md should include:
1. **Overview** - What this module does, why it exists
2. **Key Files** - List of important files with brief descriptions
3. **Key Concepts** - Main algorithms, data structures, invariants
4. **Usage** - How other parts of the system use this module
5. **Development** - How to modify/extend this module
6. **See Also** - Links to related documentation

Keep documentation:
- **Practical** - Focus on what developers need to know
- **Well-organized** - Clear hierarchical structure
- **Cross-linked** - Link between related docs
- **Up-to-date** - Reference actual file names and line numbers where helpful

## Notes for Resumption

If you need to resume this work:

1. Check `DOCUMENTATION_CHECKPOINT.md` for current status
2. Look at completed docs for style/structure examples
3. Use `ls -la` to explore directory structure
4. Use `Read` tool to examine key files before documenting
5. Focus on one module at a time
6. Update checkpoint after completing each module
7. Mark todos as completed in TodoWrite

## File Locations

All runtime docs: `runtime/*/README.md`
Main runtime overview: `runtime/README.md`
Checkpoint: `DOCUMENTATION_CHECKPOINT.md`

## Quick Commands for Exploration

```bash
# List subdirectories
find <dir> -type d -maxdepth 2 | sort

# Count files
ls <dir>/*.{c,h} 2>/dev/null | wc -l

# List files
ls -la <dir>/

# View key headers
cat <dir>/<module>.h
```
