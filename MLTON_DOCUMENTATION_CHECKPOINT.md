# MLton Compiler Documentation - Checkpoint for Resumption

**Status**: Ready to begin compiler documentation
**Last Updated**: 2025-11-18
**Context**: Runtime documentation is complete and scrubbed. Now focusing on compiler documentation.

## What's Been Completed

### Runtime Documentation (DONE ✅)
All runtime documentation is complete and ready for separate branch:

1. **runtime/README.md** - Main runtime overview
2. **runtime/gc/README.md** - GC subsystem (151 files documented)
3. **runtime/basis/README.md** - FFI implementations (~118 files)
4. **runtime/platform/README.md** - Platform abstraction
5. **runtime/util/README.md** - Utility functions (15 files)
6. **runtime/gdtoa/README.md** - Floating-point conversion library
7. **DOCUMENTATION_INDEX.md** - Master index (scrubbed, standalone)

**Total**: ~3500+ lines of documentation, ~350 source files covered

**Important**: All docs are scrubbed of CLAUDE.md references and development-specific content. They are standalone reference documentation ready for a docs branch.

## What's Next: Compiler Documentation

### Strategy Document Created
**File**: [MLTON_DOCUMENTATION_PLAN.md](MLTON_DOCUMENTATION_PLAN.md)

This plan document outlines the complete compiler documentation strategy. **READ THIS FIRST** when resuming.

### Phased Approach (from plan)

#### Phase 1: High-Level Overview (HIGHEST PRIORITY - Start Here)
Estimated: 20-26 hours

1. **mlton/README.md** (~6-8 hours)
   - Compiler overview and architecture
   - Compilation pipeline: Source → CoreML → XML → SSA → SSA2 → Machine → C
   - IR progression and transformations
   - Pass organization and ordering
   - How to extend the compiler
   - **Template**: Follow runtime/README.md structure

2. **mlton/ssa/README.md** (~8-10 hours)
   - SSA IR structure and invariants
   - All optimization passes (20+ passes) documented
   - Parallel-specific passes (direct-exp, analyze, drop-spork)
   - Closure conversion (handles loop_choose, spork_choose)
   - How to add new passes
   - Pass ordering rationale
   - **This is the most critical doc** - SSA is where most work happens

3. **mlton/control/README.md** (~3-4 hours)
   - Compiler flags and controls
   - How to add new flags (with examples)
   - Control flow through compiler
   - Expert vs normal flags
   - **Use spork-choose-threshold as case study**

4. **mlton/atoms/README.md** (~3-4 hours)
   - Primitive operations (Prim.t)
   - Basic compiler atoms
   - How to add new primitives (with full workflow)
   - **Use loop_choose/spork_choose as case study**

#### Phase 2: IR-Specific Details (~15-20 hours)

5. **mlton/front-end/README.md**
6. **mlton/xml/README.md**
7. **mlton/backend/README.md**

#### Phase 3: Specialized Topics (~10-15 hours)

8. **mlton/elaborate/README.md**
9. **mlton/core-ml/README.md**
10. **mlton/closure-convert/README.md** (detailed deep-dive)

## Key Resources for Documentation

### Files to Study

**Compilation Pipeline**:
- `mlton/main/compile.fun` - Main compilation driver
- `mlton/main/main.fun` - Entry point, command-line parsing

**SSA Passes** (most important):
- `mlton/ssa/direct-exp.fun` - Parallel primitives handling
- `mlton/ssa/closure-convert.fun` - Closure conversion + compile-time decisions
- `mlton/ssa/analyze.fun`, `mlton/ssa/analyze2.fun` - Parallel analysis
- `mlton/ssa/drop-spork.fun`, `mlton/ssa/drop-spork2.fun` - Remove parallelism
- `mlton/ssa/inline.fun` - Inlining
- `mlton/ssa/contify.fun` - Contification
- `mlton/ssa/known-case.fun` - Case optimization

**Control and Primitives**:
- `mlton/control/control-flags.sig` - Flag signatures
- `mlton/control/control-flags.sml` - Flag implementations
- `mlton/atoms/prim.sig` - Primitive type signatures
- `mlton/atoms/prim.fun` - Primitive implementations

**Type System**:
- Various `type-check.fun` files in each IR directory

### Key Concepts to Document

1. **Compilation Pipeline Flow**:
   - How source ML becomes C code
   - What each IR transformation does
   - Why each IR exists (what problems it solves)

2. **Pass Ordering**:
   - Why passes run in specific order
   - Dependencies between passes
   - Shrink cycles and their purpose

3. **Parallel Primitives Pipeline**:
   - How `ForkJoin.par` flows through compilation
   - XML → SSA lowering (direct-exp)
   - Analysis passes (analyze, analyze2)
   - Optimization (drop-spork)
   - Backend code generation

4. **Compile-Time Decisions** (loop_choose, spork_choose):
   - Basis library primitive declarations
   - Compiler primitive handling
   - Closure conversion analysis and decision
   - Size thresholds and configuration
   - How to add new choice primitives

5. **Type Checking**:
   - Type representations in each IR
   - Invariants maintained
   - How type checking works after each pass

## Documentation Style Guide

Follow the established pattern from runtime docs:

### Structure Template
```markdown
# Module Name

Brief overview (2-3 sentences)

## Overview

Detailed description of purpose and role

## Key Concepts

Main algorithms, data structures, ideas

## File Organization / Directory Structure

List of key files with descriptions

## [Category-Specific Sections]

Organized by functionality

## Development Guide

How to modify/extend this module

## Examples

Concrete examples of common tasks

## Common Issues

Troubleshooting and debugging

## See Also

Cross-references to related docs
```

### Writing Guidelines

1. **Length**: Main docs ~500-1000 lines, module docs ~300-600 lines
2. **Tone**: Technical, precise, developer-focused
3. **Examples**: Include code snippets and concrete examples
4. **Cross-linking**: Link to related files and docs
5. **File references**: Use `[filename.ext](path/to/file.ext)` format
6. **No emojis**: Keep it professional (unlike this checkpoint)
7. **No CLAUDE.md references**: Standalone documentation only
8. **No development-specific language**: Reference material, not WIP notes

## Important: What NOT to Include

Based on scrubbing experience:

❌ **Do NOT include**:
- References to CLAUDE.md
- "Current development" or "work in progress"
- Temporary implementation notes
- Personal development workflows
- Context about recent changes or current branch

✅ **DO include**:
- Reference documentation
- How-to guides for modification
- Architecture and design
- Debugging and troubleshooting
- Examples of adding features

## Starting Point: mlton/README.md

When you resume, start with `mlton/README.md`. Here's the outline:

```markdown
# MPL Compiler (MLton-based)

## Overview
- Based on MLton compiler
- Extensions for parallelism
- Whole-program optimization

## Compilation Pipeline
- Source → CoreML → XML → SSA → SSA2 → Machine → C
- Purpose of each IR
- Transformation overview

## Directory Structure
- mlton/front-end/
- mlton/elaborate/
- mlton/core-ml/
- mlton/xml/
- mlton/ssa/
- mlton/backend/
- mlton/control/
- mlton/atoms/

## Key Intermediate Representations
- CoreML: After elaboration
- XML: Polymorphic, defunctorized
- SSA: Main optimizations
- SSA2: Simplified
- Machine: Low-level
- C: Target code

## Optimization Passes
- Overview of major passes
- Pass ordering
- Shrink cycles

## Parallel Extensions
- Parallel primitives handling
- Fork-join transformations
- Compile-time decisions

## Type System
- Type checking at each stage
- Type representations
- Invariants

## Build Process
- How compiler is built
- Bootstrap process

## Development Guide
- Adding passes
- Adding primitives
- Adding flags
- Debugging compilation

## See Also
- Links to other docs
```

## Exploration Commands

Use these to explore the compiler when documenting:

```bash
# List compilation passes
grep -r "structure.*Pass" mlton/ssa/*.fun | head -20

# Find all SSA passes
ls mlton/ssa/*.fun | grep -v "ssa-" | wc -l

# Count compiler source files
find mlton -name "*.sml" -o -name "*.fun" -o -name "*.sig" | wc -l

# View compilation pipeline
grep -A 50 "fun compile" mlton/main/compile.fun | head -60

# Find primitive definitions
grep "datatype.*Prim" mlton/atoms/prim.fun

# Find control flags
grep "val.*control" mlton/control/control-flags.sml | head -30
```

## Files Created for Tracking

- **MLTON_DOCUMENTATION_PLAN.md** - Overall strategy (keep for reference)
- **MLTON_DOCUMENTATION_CHECKPOINT.md** - This file (resumption guide)
- **DOCUMENTATION_CHECKPOINT.md** - Overall progress (can archive after compiler docs done)

## Resumption Checklist

When you resume compiler documentation:

1. ✅ Read MLTON_DOCUMENTATION_PLAN.md thoroughly
2. ✅ Read this checkpoint file
3. ✅ Explore mlton/ directory structure
4. ✅ Read mlton/main/compile.fun to understand pipeline
5. ✅ Start writing mlton/README.md
6. ✅ Follow Phase 1 priorities
7. ✅ Update DOCUMENTATION_INDEX.md as you add compiler docs
8. ✅ Keep docs scrubbed (no CLAUDE.md refs)

## Quick Reference: What's Where

**Runtime Docs** (complete):
- runtime/README.md
- runtime/gc/README.md
- runtime/basis/README.md
- runtime/platform/README.md
- runtime/util/README.md
- runtime/gdtoa/README.md

**Master Index**:
- DOCUMENTATION_INDEX.md (update with compiler docs as you create them)

**Planning Docs** (reference only):
- MLTON_DOCUMENTATION_PLAN.md - Comprehensive strategy
- MLTON_DOCUMENTATION_CHECKPOINT.md - This file
- DOCUMENTATION_CHECKPOINT.md - Overall progress tracking

**Do NOT document** (keep in main branch):
- CLAUDE.md - Development instructions for current work

## Success Criteria

Compiler documentation is complete when:

1. All Phase 1 docs created (mlton/README.md, ssa/README.md, control/README.md, atoms/README.md)
2. DOCUMENTATION_INDEX.md updated with compiler sections
3. All docs are scrubbed and standalone
4. Cross-references are working
5. Examples and how-tos are included
6. Common issues documented
7. Developer workflows explained

Estimated total effort: 45-61 hours for all three phases.

## Notes on Context Limits

If you run out of context:
1. This checkpoint file has everything needed to resume
2. MLTON_DOCUMENTATION_PLAN.md has the detailed strategy
3. Runtime docs serve as templates
4. Start fresh with Phase 1, prioritize mlton/README.md and mlton/ssa/README.md

---

**Ready to resume compiler documentation!** Start with Phase 1, Item 1: mlton/README.md
