# MLton Compiler Documentation Plan

This document outlines a strategy for documenting the MPL compiler (based on MLton with parallel extensions).

## Overview

The MLton compiler is a whole-program optimizing compiler for Standard ML with multiple intermediate representations and extensive optimization passes. MPL extends it with parallel primitives and optimizations.

## Current Status

✅ **Available**: [CLAUDE.md](CLAUDE.md) contains:
- Compilation pipeline overview
- How to add compile-time flags
- How to add parallel primitives
- How to add loop optimization primitives (loop_choose, spork_choose)
- Key file locations and workflows

⬜ **Needed**: Comprehensive documentation for compiler internals and development

## Proposed Documentation Structure

### 1. Main Compiler Overview
**File**: `mlton/README.md`

**Contents**:
- Compilation pipeline overview
- IR progression: Source → CoreML → XML → SSA → SSA2 → Machine → C
- Pass organization and ordering
- How passes communicate (shrink cycles, type checking)
- Key design principles
- How to extend the compiler

**Audience**: Developers new to the MLton codebase

**Length**: ~500-800 lines (similar to runtime/README.md)

### 2. Front-End Documentation
**File**: `mlton/front-end/README.md`

**Contents**:
- Parsing Standard ML source
- AST structure
- Elaboration to CoreML
- Module system handling
- Type inference
- Key files: ml.lex, ml.grm, elaborate/

**Key Topics**:
- How ML source becomes CoreML
- Type checking and inference algorithm
- Module elaboration (functors, signatures)
- Error reporting

### 3. Elaboration
**File**: `mlton/elaborate/README.md`

**Contents**:
- Type inference engine
- Module elaboration (functors, structures, signatures)
- Overload resolution
- Type generalization
- Key invariants

**Key Files**:
- `elaborate-core.fun` - Core expression elaboration
- `elaborate-modules.fun` - Module system elaboration
- `type-env.fun` - Type environment
- `scope.fun` - Scoping rules

### 4. CoreML IR
**File**: `mlton/core-ml/README.md`

**Contents**:
- CoreML intermediate representation
- Explicitly-typed ML after elaboration
- Still has modules (not yet defunctorized)
- Data structures and invariants
- Transformations applied

### 5. XML IR
**File**: `mlton/xml/README.md`

**Contents**:
- XML (eXplicit intermediate language)
- First typed intermediate language
- Polymorphic with explicit type passing
- Defunctorization happens here
- Key transformations: monomorphise, implement-suffix, etc.

**Key Topics**:
- Type representation in XML
- Polymorphism handling
- Defunctorization algorithm
- Closure conversion preparation

### 6. SSA IR (Most Important)
**File**: `mlton/ssa/README.md`

**Contents**:
- Static Single Assignment form
- Main optimization passes (20+ passes)
- Parallel-specific passes
- Pass ordering and rationale
- How to add new passes
- Type system in SSA

**Optimization Passes** (document each):
- **Common**: constant-propagation, common-subexp, contify, flatten, inline, known-case, local-flatten, loop-invariant, redundant, redundant-tests, remove-unused, simplify, useless
- **Parallel-specific**: analyze, analyze2, direct-exp, drop-spork, drop-spork2
- **Special**: closure-convert (handles loop_choose, spork_choose decisions)

**Key Files by Category**:

*Parallel Primitives*:
- `direct-exp.fun` - Handle ForkJoin.par and parallel constructs
- `analyze.fun`, `analyze2.fun` - Analyze parallel structure
- `drop-spork.fun`, `drop-spork2.fun` - Remove unnecessary parallelism

*Optimization*:
- `inline.fun` - Function inlining
- `contify.fun` - Contification (CPS-like optimization)
- `known-case.fun` - Specialize case expressions with known constructors
- `constant-propagation.fun` - Constant propagation and folding
- `common-subexp.fun` - Common subexpression elimination
- `loop-invariant.fun` - Loop-invariant code motion
- `flatten.fun`, `deep-flatten.fun`, `local-flatten.fun` - Data representation optimization

*Control Flow*:
- `simplify.fun` - Simplify control flow
- `useless.fun` - Remove useless code
- `remove-unused.fun` - Remove unused declarations

*Other*:
- `closure-convert.fun` - **CRITICAL** - Closure conversion and compile-time decisions
- `polyvariance.fun` - Polyvariant specialization
- `ref-flatten.fun` - Reference flattening
- `poly-equal.fun`, `poly-hash.fun` - Polymorphic equality/hash specialization

**Closure Conversion Details** (needs thorough documentation):
- Makes compile-time decisions for loop_choose, spork_choose
- Analyzes loop body size using `sxmlLambdaSize`
- Threshold-based decision (e.g., size ≤ 100 → unrolled)
- How to add new compile-time choice primitives
- Diagnostics output (`-diag-pass closureConvert`)

### 7. SSA2 IR
**File**: `mlton/ssa/ssa2/README.md` (or section in ssa/README.md)

**Contents**:
- Simplified SSA after major optimizations
- Cleanup passes
- Preparation for backend
- Final type checking

### 8. Backend and Codegen
**File**: `mlton/backend/README.md`

**Contents**:
- Machine representation (lower-level than SSA2)
- C code generation (only supported backend for MPL)
- Runtime interface
- Object layout decisions
- Stack management
- Calling conventions

**Key Topics**:
- SSA2 → Machine lowering
- Machine → C code generation
- Integration with runtime system
- FFI handling
- Allocation and GC integration

**Key Files**:
- `machine.sig/fun` - Machine IR
- `c-codegen.fun` - C code generation
- `allocate-registers.fun` - Register allocation
- `limit-check.fun` - Heap limit checks

### 9. Control System
**File**: `mlton/control/README.md`

**Contents**:
- Compiler flags and controls
- Control flow through passes
- How to add new flags (with examples)
- Expert vs normal flags
- Diagnostic controls

**Key Files**:
- `control-flags.sig/sml` - Flag definitions
- `control.sig/sml` - Central control structure

**Examples**:
- Adding `-spork-choose-threshold` (recently added)
- Adding `-loop-choose-threshold`
- Controlling optimization levels
- Debug and diagnostic flags

### 10. Atoms and Primitives
**File**: `mlton/atoms/README.md`

**Contents**:
- Basic compiler building blocks
- Primitive operations (Prim.t)
- Constants, types, variables, labels
- How to add new primitives (with detailed example)

**Primitive Addition Workflow**:
1. Add to `Prim.t` datatype in `prim.sig`
2. Add to implementation in `prim.fun`
3. Add string name in `toString`
4. Handle in `checkApp`, `map`, `extractTargs`
5. Add basis primitive declaration (in basis-library)
6. Implement in closure conversion or other passes
7. Test compilation and runtime

**Example**: Document loop_choose / spork_choose as case study

### 11. Type System
**File**: `mlton/type-system/README.md` or section in main README

**Contents**:
- Type representations across IRs
- Type checking at each stage
- Polymorphism handling
- Type-directed optimizations

## Documentation Priorities

### Phase 1: High-Level Overview (Most Important)
1. **mlton/README.md** - Compiler overview
2. **mlton/ssa/README.md** - SSA IR and optimization passes
3. **mlton/control/README.md** - Adding flags and controls
4. **mlton/atoms/README.md** - Adding primitives

**Rationale**: These cover the most common development tasks:
- Understanding compilation pipeline
- Adding optimization passes
- Adding primitives (loop_choose, spork_choose, etc.)
- Adding compile-time flags

### Phase 2: IR-Specific Details
5. **mlton/front-end/README.md** - Front-end
6. **mlton/xml/README.md** - XML IR
7. **mlton/backend/README.md** - Backend and codegen

**Rationale**: Needed for deeper compiler work:
- Modifying parsing/elaboration
- Understanding defunctorization
- Backend modifications

### Phase 3: Specialized Topics
8. **mlton/elaborate/README.md** - Type inference
9. **mlton/core-ml/README.md** - CoreML IR
10. **mlton/closure-convert/README.md** - Detailed closure conversion doc

**Rationale**: Advanced topics for specialized work

## Key Cross-Cutting Concerns to Document

### 1. Parallel Primitives Pipeline
Document how parallel primitives flow through compilation:

**Source SML**:
```sml
ForkJoin.par (fn () => left, fn () => right)
```

**XML**:
- Still high-level parallel primitive
- Polymorphic

**SSA** (direct-exp.fun):
- Lower to fork/join/ccall primitives
- Insert heap checks, limit checks
- Create thread objects

**SSA** (analyze.fun):
- Analyze parallel structure
- Detect nested parallelism
- Mark fork-join regions

**SSA** (drop-spork.fun):
- Remove unnecessary parallelism
- Based on analysis results

**SSA2 → Backend**:
- Generate C code calling runtime functions
- GC integration (decheck, heap management)

### 2. Compile-Time Decisions (loop_choose, spork_choose)
Document the full workflow:

**Basis Library**:
- Declare `_prim` function with type
- Wrapper with `__inline_always__`
- Two implementations (managed vs unrolled / parallel vs sequential)

**Compiler (Prim.t)**:
- Add primitive constructor
- String name mapping
- Type checking

**Closure Conversion**:
- Pattern match on primitive
- Extract and analyze loop body
- Size-based decision using threshold
- Apply chosen implementation
- Emit diagnostics

**Example**: Complete walkthrough of adding new compile-time choice primitive

### 3. Type Checking and Invariants
Document invariants maintained at each IR:

- **CoreML**: Explicitly typed, modules intact
- **XML**: Polymorphic with type passing, defunctorized
- **SSA**: Monomorphic, SSA form, complex control flow
- **SSA2**: Simplified SSA, limited constructs
- **Machine**: Low-level, close to C

How type checking is performed after each pass, what to check when adding passes.

### 4. Pass Ordering Rationale
Document why passes run in specific order:

Example: Why inline before contify? Why shrink between optimization passes?

Understanding pass dependencies and interaction.

## Style and Format

Each README should follow the pattern established in runtime documentation:

1. **Overview** - What this module does
2. **Key Concepts** - Main algorithms and data structures
3. **File Organization** - Directory structure
4. **Key Files** - Important files with descriptions
5. **Development Guide** - How to modify/extend
6. **Examples** - Concrete examples of common tasks
7. **Common Issues** - Troubleshooting
8. **See Also** - Cross-references

**Length**:
- Main overview: ~800-1000 lines
- Module-specific: ~300-600 lines
- Specialized topics: ~200-400 lines

## Integration with Existing Documentation

Link from:
- **DOCUMENTATION_INDEX.md** - Add compiler section with links
- **CLAUDE.md** - Reference detailed compiler docs
- **Each compiler README** - Cross-link to related modules

## Estimated Effort

**Phase 1** (highest priority):
- mlton/README.md: ~6-8 hours
- mlton/ssa/README.md: ~8-10 hours
- mlton/control/README.md: ~3-4 hours
- mlton/atoms/README.md: ~3-4 hours
**Total: ~20-26 hours**

**Phase 2**:
- ~15-20 hours

**Phase 3**:
- ~10-15 hours

**Grand Total**: ~45-61 hours for comprehensive compiler documentation

## Next Steps

1. Review this plan with team
2. Prioritize which sections are most urgently needed
3. Begin with Phase 1 (high-level overview and SSA)
4. Iterate based on feedback
5. Expand to Phase 2 and 3 as time permits

## Resources for Documentation Writers

To document the compiler, consult:

1. **Existing code** - Best source of truth
2. **MLton website** - http://mlton.org (some documentation exists)
3. **CLAUDE.md** - Current workflows and examples
4. **Papers** - MLton papers on whole-program compilation
5. **Comments** - Many files have good header comments
6. **Types** - Signature files (.sig) document interfaces

## Questions to Answer in Documentation

For each module, answer:
- **What** - What does this module do?
- **Why** - Why does it exist? What problem does it solve?
- **How** - How does it work (algorithm/approach)?
- **When** - When is it invoked in the compilation pipeline?
- **Who** - What other modules does it interact with?
- **Where** - Where are the key files?
- **Extend** - How do I modify/extend this module?
- **Debug** - How do I debug issues in this module?

## Maintenance

Once created:
- Update when adding new passes
- Update when adding new primitives
- Update when changing pass ordering
- Update when adding compile-time flags
- Keep examples current with actual code

---

**Status**: Plan complete, awaiting implementation
**Last Updated**: 2025-11-18
**Contact**: MPL development team
