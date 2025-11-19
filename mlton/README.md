# MPL Compiler (MLton-based)

The MPL compiler is a whole-program optimizing compiler for Standard ML with extensions for parallelism. It is based on the MLton compiler and uses a multi-stage compilation pipeline with multiple intermediate representations (IRs).

## Overview

**Key Features**:
- Whole-program optimization with extensive optimization passes
- Multiple intermediate representations for different optimization stages
- Parallel primitive support (fork-join, parallel loops)
- Compile-time decision making for performance-critical constructs
- C code generation backend
- Type checking at every stage

**Design Principles**:
- **Whole-program compilation**: Analyzes entire program for aggressive optimization
- **Multiple IRs**: Each IR is designed for specific transformation and optimization tasks
- **Type preservation**: Programs remain well-typed throughout compilation
- **Simplify cycles**: Iterative optimization with shrinking between passes

## Compilation Pipeline

The compiler transforms Standard ML source code through multiple intermediate representations:

```
Source ML
    ↓
  AST (Abstract Syntax Tree)
    ↓
  [Elaboration + Type Inference]
    ↓
CoreML (Explicitly-typed ML)
    ↓
  [Defunctorization]
    ↓
  XML (Polymorphic, typed, first-order)
    ↓
  [Monomorphisation]
    ↓
  SXML (Monomorphic XML)
    ↓
  [Closure Conversion]
    ↓
  SSA (Static Single Assignment - main optimization stage)
    ↓
  [Major optimizations]
    ↓
  SSA2 (Simplified SSA)
    ↓
  [More optimizations + preparation for backend]
    ↓
  RSSA (Explicit stack representation)
    ↓
  [Backend transformations]
    ↓
Machine (Low-level representation)
    ↓
  [Code generation]
    ↓
   C code
```

### Pipeline Stages

The compilation progresses through these major stages:

1. **Frontend** ([front-end/](front-end/), [elaborate/](elaborate/))
   - Parses Standard ML source into AST
   - Elaborates modules and performs type inference
   - Produces CoreML

2. **Defunctorization** ([defunctorize/](defunctorize/))
   - Eliminates functors and higher-order modules
   - Produces XML (polymorphic, typed, first-order)

3. **Monomorphisation** (xml/monomorphise.fun)
   - Specializes polymorphic functions
   - Produces SXML (monomorphic)

4. **Closure Conversion** ([closure-convert/](closure-convert/))
   - Converts to first-order representation with explicit closures
   - Can perform compile-time optimizations based on program analysis
   - Produces SSA

5. **SSA Optimization** ([ssa/](ssa/))
   - Main optimization stage with 20+ passes
   - Inlining, contification, constant propagation
   - Data representation optimization (flattening)
   - Parallel-specific transformations

6. **SSA2 Optimization** ([ssa/](ssa/))
   - Simplified SSA with final optimization passes
   - Preparation for backend

7. **Backend** ([backend/](backend/), [codegen/](codegen/))
   - Converts to machine representation (RSSA → Machine)
   - Register allocation, stack management
   - C code generation

## Key Intermediate Representations

### CoreML ([core-ml/](core-ml/))
**Purpose**: Explicitly-typed ML after elaboration
- Still has module structure (functors, structures)
- Explicitly typed with type annotations
- Output of elaboration and type inference

### XML ([xml/](xml/))
**Purpose**: First typed intermediate language
- Polymorphic with explicit type passing
- Defunctorized (no more functors)
- First-order (no nested functions yet)
- Simple expression language

**Key transformations**:
- xmlSimplify: Basic XML-level optimizations
- Preparation for monomorphisation

### SXML (Simplified XML)
**Purpose**: Monomorphic version of XML
- All polymorphism specialized away
- Ready for closure conversion
- Uses same data structures as XML but monomorphic

### SSA ([ssa/](ssa/))
**Purpose**: Main optimization IR
- Static Single Assignment form
- Each variable assigned exactly once
- Explicit control flow with basic blocks
- Complex optimization passes

**Key properties**:
- Monomorphic (types specialized)
- First-order with explicit closures
- Suitable for data-flow analysis
- Most optimization happens here

**Major pass categories**:
- Inlining and contification
- Constant propagation and folding
- Common subexpression elimination
- Data representation (flattening, unboxing)
- Loop optimization (invariant motion, unrolling, unswitching)
- Parallel-specific (direct-exp, analyze, drop-spork)
- Redundancy elimination

See [ssa/README.md](ssa/README.md) for detailed SSA documentation.

### SSA2 ([ssa/](ssa/))
**Purpose**: Simplified SSA for backend preparation
- Restricted subset of SSA constructs
- Final cleanup optimizations
- Explicit representations for backend

### RSSA (Runtime SSA)
**Purpose**: SSA with explicit runtime representations
- Explicit stack allocation and management
- Object representations finalized
- GC interaction points explicit

### Machine ([backend/](backend/))
**Purpose**: Low-level representation close to C
- Explicit memory layout
- Register and stack management
- Calling conventions
- Ready for code generation

## Directory Structure

```
mlton/
├── README.md                 (this file)
│
├── atoms/                    Primitive operations and basic compiler atoms
│   ├── prim.sig/fun          Primitive operations (Prim.t)
│   ├── con.sig/fun           Datatype constructors
│   ├── tycon.sig/fun         Type constructors
│   └── ...                   Variables, labels, constants
│
├── front-end/                Parsing and lexing
│   ├── ml.lex                ML lexer
│   ├── ml.grm                ML grammar
│   ├── mlb.lex/grm           MLB (ML Basis) lexer/parser
│   └── ...
│
├── elaborate/                Type inference and module elaboration
│   ├── elaborate-core.fun    Core expression elaboration
│   ├── elaborate-modules.fun Module system elaboration
│   ├── type-env.fun          Type environment
│   └── ...
│
├── ast/                      Abstract syntax tree
│   └── ast.sig/fun           AST data structures
│
├── core-ml/                  CoreML IR
│   ├── core-ml.sig/fun       CoreML data structures
│   └── dead-code.fun         Dead code elimination
│
├── defunctorize/             Functor elimination
│   └── defunctorize.fun      Defunctorization pass
│
├── xml/                      XML IR (polymorphic, typed)
│   ├── xml.sig/fun           XML data structures
│   ├── xml-simplify.fun      XML-level optimizations
│   ├── monomorphise.fun      Specialization of polymorphic code
│   └── ...
│
├── closure-convert/          Closure conversion
│   └── closure-convert.fun   Convert to first-order + compile-time decisions
│
├── ssa/                      SSA IR and main optimization passes
│   ├── ssa.sig/fun           SSA data structures
│   ├── ssa2.sig/fun          SSA2 data structures
│   ├── simplify.fun          Main optimization driver
│   ├── simplify2.fun         SSA2 optimization driver
│   │
│   ├── inline.fun            Function inlining
│   ├── contify.fun           Contification (CPS-like optimization)
│   ├── constant-propagation.fun
│   ├── common-subexp.fun     Common subexpression elimination
│   ├── known-case.fun        Case optimization with known constructors
│   ├── flatten.fun           Data representation optimization
│   ├── loop-invariant.fun    Loop-invariant code motion
│   │
│   ├── direct-exp.fun        Parallel primitives handling
│   ├── analyze.fun           Parallel structure analysis
│   ├── drop-spork.fun        Remove unnecessary parallelism
│   │
│   └── ...                   (20+ more optimization passes)
│
├── backend/                  Backend transformations
│   ├── backend.sig/fun       Main backend driver
│   ├── rssa.sig/fun          RSSA IR
│   ├── machine.sig/fun       Machine IR
│   ├── allocate-registers.fun
│   ├── limit-check.fun       Heap limit checks
│   └── ...
│
├── codegen/                  Code generation
│   ├── c-codegen/            C code generator
│   └── ...
│
├── control/                  Compiler flags and controls
│   ├── control-flags.sig/sml Compiler flag definitions
│   └── control.sig/sml       Central control structure
│
└── main/                     Compiler driver
    ├── compile.fun           Main compilation pipeline
    ├── main.fun              Entry point, command-line parsing
    └── ...
```

## Optimization Passes

### Pass Organization

Optimizations are organized into **simplify cycles**:

```
SSA stage:
  ssaSimplify = multiple optimization passes + type checking

Typical pattern:
  removeUnused → ... optimizations ... → removeUnused
```

Each simplify cycle:
1. Runs multiple optimization passes
2. Shrinks the program (removes dead code, simplifies)
3. Type-checks the result
4. Iterates until fixed point

### Major Optimization Categories

**Inlining and Specialization**:
- `inline.fun`: Function inlining (leaf, non-recursive)
- `contify.fun`: Contification (convert to continuation-passing style)
- `known-case.fun`: Specialize case statements with known constructors
- `poly-equal.fun`, `poly-hash.fun`: Specialize polymorphic equality/hash

**Constant and Expression Optimization**:
- `constant-propagation.fun`: Propagate and fold constants
- `common-subexp.fun`: Eliminate common subexpressions
- `common-arg.fun`, `common-block.fun`: Common argument/block elimination
- `redundant.fun`, `redundant-tests.fun`: Remove redundant code/tests

**Data Representation**:
- `flatten.fun`: Flatten tuple/record representations
- `deep-flatten.fun`: Aggressive flattening
- `local-flatten.fun`: Local tuple flattening
- `ref-flatten.fun`: Flatten mutable references when safe
- `split-types.fun`: Split sum types for better representation

**Loop Optimization**:
- `introduce-loops.fun`: Recognize loop patterns
- `loop-invariant.fun`: Move loop-invariant code
- `loop-unroll.fun`: Unroll small loops
- `loop-unswitch.fun`: Hoist loop conditions

**Control Flow**:
- `useless.fun`: Remove useless code
- `remove-unused.fun`: Remove unused declarations
- `local-ref.fun`: Optimize local references

**Parallel-Specific Passes**:
- `direct-exp.fun`: Lower `ForkJoin.par` and parallel primitives
- `analyze.fun`, `analyze2.fun`: Analyze parallel structure
- `drop-spork.fun`, `drop-spork2.fun`: Remove unnecessary parallelism

**Cleanup and Utilities**:
- `shrink.fun`, `shrink2.fun`: Shrink program size
- `simplify-types.fun`: Simplify type representations
- `combine-conversions.fun`: Combine type conversions

### Pass Ordering Rationale

Passes run in carefully chosen order based on dependencies:

**Early passes** (after closure conversion):
1. `removeUnused`: Clean up dead code
2. `introduceLoops`: Recognize loops for optimization
3. `loopInvariant`: Move invariant code before inlining makes it harder
4. `inlineLeaf`: Inline simple leaf functions
5. `contify`: Convert to continuation style

**Middle passes** (main optimization):
1. `constantPropagation`: Enable many other optimizations
2. `useless`: Remove tuple slots that became constant
3. `simplifyTypes`: Simplify type representations
4. `polyEqual`, `polyHash`: Specialize before inlining
5. `inline` (non-recursive): Major inlining pass
6. `localFlatten`, `flatten`: Optimize data layout
7. `commonSubexp`, `commonBlock`: Eliminate redundancy

**Late passes** (cleanup):
1. `redundantTests`: Remove redundant comparisons
2. `knownCase`: Optimize case statements
3. `removeUnused`: Final cleanup

**Why this order?**:
- Constant propagation early: Enables other opts
- Inline after type simplification: More opportunities
- Flatten after inlining: See more data flow
- Redundancy elimination late: After other opts create redundancy
- Cleanup (removeUnused) at beginning and end

## Parallel Extensions

MPL adds parallel programming support on top of MLton:

### Parallel Primitives

Defined in basis library (`basis-library/schedulers/spork/ForkJoin.sml`):
- `ForkJoin.par`: Fork-join parallelism
- `ForkJoin.parfor`: Parallel for loops
- `ForkJoin.parform`: Parallel for with auto-granularity
- `ForkJoin.alloc`: Allocate uninitialized arrays

### Compiler Support for Parallelism

**SSA passes handle parallel constructs**:

1. **direct-exp.fun**: Lower parallel primitives
   - Converts `ForkJoin.par` to low-level fork/join operations
   - Inserts heap checks and GC interaction points
   - Creates thread objects

2. **analyze.fun**, **analyze2.fun**: Analyze parallel structure
   - Detect nested parallelism
   - Mark fork-join regions
   - Gather information for optimization

3. **drop-spork.fun**, **drop-spork2.fun**: Optimize parallelism
   - Remove unnecessary parallelism based on analysis
   - Sequential execution when beneficial

### Compile-Time Optimizations

The compiler can make optimization decisions during compilation based on program analysis:

**General approach**:
- Analyze program structure (e.g., function size, call patterns)
- Choose between different implementation strategies
- Apply transformations based on heuristics and thresholds

**Examples of compile-time decisions**:
- Inlining: Decide whether to inline based on function size
- Contification: Convert tail calls to jumps
- Loop unrolling: Decide whether to unroll based on loop structure
- Data representation: Choose between boxed/unboxed representations

**Implementation points**:
- Closure conversion can analyze code structure
- SSA passes can make optimization decisions
- Backend can choose low-level representations

See [ssa/README.md](ssa/README.md) for optimization pass details.

## Type System

### Type Checking at Each Stage

The compiler type-checks after every major transformation:

**CoreML**: Full ML type system with polymorphism
- Type inference produces explicitly-typed programs
- Module system types (functors, signatures)

**XML**: Explicit polymorphism
- Types passed explicitly as arguments
- Type variables tracked through program

**SXML/SSA**: Monomorphic
- All types fully specialized
- No polymorphism remains
- Simple type checking

**SSA2, RSSA, Machine**: Increasingly low-level
- Type checking ensures invariants
- Verifies memory layouts
- Checks GC interaction points

### Type Representations

Different IRs use different type representations:

- **CoreML**: Standard ML types with variables and constraints
- **XML**: Types with explicit quantification
- **SSA**: Simple monomorphic types (tuples, arrays, constructors)
- **Machine**: Low-level types (words, pointers, stack slots)

## Build Process

### Building the Compiler

The MPL compiler is built by compiling its SML source with an existing MLton compiler:

```bash
# From repository root
make compiler
```

This produces `mlton/mlton-mpl-compile`, the MPL compiler executable.

### Bootstrap Process

1. **Base compiler**: Use existing MLton (or MPL) compiler
2. **Compile mlton sources**: Compile `mlton/sources.mlb`
3. **Result**: `mlton-mpl-compile` executable

### Compiler Source Organization

**Main MLB file**: [mlton.mlb](mlton.mlb)
- Lists all compiler modules in dependency order
- Includes basis library and supporting libraries

**Key source files**:
- ~407 SML source files (`.sml`, `.fun`, `.sig`)
- Organized by compiler stage and functionality

## Development Guide

### Adding a New Optimization Pass

1. **Create the pass**: `mlton/ssa/my-pass.fun`
   ```sml
   functor MyPass (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
   struct
      open S

      fun transform (program: Program.t): Program.t =
         (* Implement optimization *)
         program
   end
   ```

2. **Add to simplify**: Edit `mlton/ssa/simplify.fun`
   ```sml
   structure MyPass = MyPass (S)

   (* Add to pass list *)
   {name = "myPass", doit = MyPass.transform, execute = true} ::
   ```

3. **Test**: Rebuild compiler and test with example programs

### Adding a Primitive

See [atoms/README.md](atoms/README.md) for detailed workflow.

**Quick summary**:
1. Add constructor to `Prim.t` in [atoms/prim.sig](atoms/prim.sig) and [atoms/prim.fun](atoms/prim.fun)
2. Add string name in `toString`
3. Handle in `checkApp`, `map`, `extractTargs`
4. Add basis library declaration (in `basis-library/`)
5. Implement in compiler passes (e.g., closure conversion, SSA passes)

### Adding a Compiler Flag

See [control/README.md](control/README.md) for detailed workflow.

**Quick summary**:
1. Declare in [control/control-flags.sig](control/control-flags.sig)
2. Implement in [control/control-flags.sml](control/control-flags.sml)
3. Add CLI parsing in [main/main.fun](main/main.fun)
4. Use via `!Control.flagName` in passes

### Debugging Compilation

**View intermediate representations**:
```bash
# Stop after SSA
mpl -stop ssa program.mlb

# Stop after SSA2
mpl -stop ssa2 program.mlb

# Keep generated files
mpl -keep g program.mlb
```

**Pass diagnostics**:
```bash
# See closure conversion analysis
mpl -diag-pass closureConvert program.mlb

# See inlining decisions
mpl -diag-pass inline program.mlb

# General diagnostics
mpl -diag-pass <passName> program.mlb
```

**Type checking**:
```bash
# Enable extra type checking
mpl -type-check true program.mlb
```

**Keep intermediate files**:
```bash
# Keep IR after specific pass
mpl -keep-pass <passName> program.mlb
```

### Understanding a Pass

To understand what a pass does:

1. **Read the signature**: Look at source file header comments
2. **Check simplify.fun**: See where pass runs in the pipeline
3. **Examine transform function**: Main entry point for the pass
4. **Look at comments**: Many passes have good documentation
5. **Run with diagnostics**: Use `-diag-pass` to see what it does

### Common Compilation Issues

**Problem**: Type error after a pass
**Solution**: Check that types are preserved in transformation

**Problem**: Performance regression
**Solution**: Use `-diag-pass` to see what optimizations fired

**Problem**: Segfault in generated code
**Solution**: Check backend passes, GC interaction points

## Examples

### Viewing the Compilation Pipeline

```bash
# Compile with all IRs saved
mpl -keep-xml -keep-sxml -keep-ssa -keep-ssa2 -keep-rssa -keep-machine program.mlb

# Results in program.{xml,sxml,ssa,ssa2,rssa,machine}
```

### Adding a Debug Print to a Pass

```sml
(* In your optimization pass *)
val () = Control.diagnostic
         (fn () => Layout.str "Running my optimization")
```

Use `-diag-pass myPass` to see output.

### Testing a New Primitive

```sml
(* 1. Add to prim.fun *)
datatype 'a t = ...
              | MyPrimitive

fun toString p =
   case p of
      ...
    | MyPrimitive => "my_primitive"

(* 2. Add basis library binding *)
val myFunc = _prim "MyPrimitive": int -> int;

(* 3. Handle in passes *)
(* In closure-convert.fun or other pass *)
case Prim.name prim of
   Prim.MyPrimitive => (* implement *)
```

## Common Issues

**Issue**: Compiler takes too long
- Check if optimization level is appropriate
- Some passes are expensive (deep-flatten, ref-flatten)
- Use `-opt-passes minimal` for faster compilation

**Issue**: Generated code is slow
- Check inlining decisions (`-diag-pass inline`)
- Verify contification is working (`-diag-pass contify`)
- Look at data representation (flattening passes)

**Issue**: Compilation fails with type error
- Type checking is strict
- Check that all passes preserve types
- Use `-type-check true` for more checking

**Issue**: Pass ordering affects results
- Passes have dependencies
- Check `simplify.fun` for rationale comments
- Some optimizations enable others

## See Also

- [ssa/README.md](ssa/README.md) - Detailed SSA IR and optimization passes
- [control/README.md](control/README.md) - Compiler flags and controls
- [atoms/README.md](atoms/README.md) - Primitives and atoms
- [front-end/README.md](front-end/README.md) - Parsing and elaboration
- [backend/README.md](backend/README.md) - Backend and code generation
- [../runtime/README.md](../runtime/README.md) - Runtime system
- [../DOCUMENTATION_INDEX.md](../DOCUMENTATION_INDEX.md) - Master documentation index

## Resources

**MLton website**: http://mlton.org
- Original MLton documentation
- Some concepts carry over to MPL

**Source code**:
- Many files have good header comments
- Signature files (`.sig`) document interfaces
- Look at existing passes as examples

**Academic papers**:
- MLton papers on whole-program compilation
- SSA-based optimization techniques
- Papers on specific passes (contification, flattening)
