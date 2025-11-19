# SSA (Static Single Assignment) Intermediate Representation

The SSA IR is the main optimization stage of the MPL compiler where most optimization passes run. It uses Static Single Assignment form for precise data-flow analysis and transformations.

## Overview

**Purpose**: Primary intermediate representation for optimization
- Static Single Assignment: Each variable assigned exactly once
- Explicit control flow with basic blocks
- Monomorphic (all types specialized)
- First-order with explicit closures
- Suitable for data-flow analysis and aggressive optimization

**Key Features**:
- 20+ optimization passes
- Parallel primitive support (fork-join, work-stealing)
- Type checking after every pass
- Iterative optimization with shrink cycles
- Compile-time optimization decisions based on program analysis

## SSA Structure

### Program Organization

```sml
Program = list of Datatypes × list of Globals × list of Functions

Function = name × arguments × list of Blocks

Block = label × arguments × list of Statements × Transfer

Statement = var × type × Expression

Expression =
  | Var(x)                      (* variable reference *)
  | Const(c)                    (* constant *)
  | Tuple(vars)                 (* tuple construction *)
  | Select(tuple, offset)       (* tuple projection *)
  | ConApp(con, args)           (* datatype constructor application *)
  | PrimApp(prim, targs, args)  (* primitive operation *)
  | Profile(exp)                (* profiling expression *)

Transfer =
  | Return(vars)                (* return from function *)
  | Raise(vars)                 (* raise exception *)
  | Call(func, args, return)    (* function call *)
  | Case(test, cases, default)  (* case/switch *)
  | Goto(label, args)           (* jump to block *)
  | Bug                         (* unreachable code *)
  | Runtime(prim, args, return) (* runtime system call *)
  | Spork(spid, cont, spwn)     (* parallel fork *)
  | Spoin(spid, seq, sync)      (* parallel join *)
```

### Key Data Structures

**Type** ([ssa-tree.sig](ssa-tree.sig)):
```sml
datatype Type.dest =
   Array of Type.t
 | CPointer
 | Datatype of Tycon.t
 | IntInf
 | Real of RealSize.t
 | Ref of Type.t
 | Thread
 | Tuple of Type.t vector
 | Vector of Type.t
 | Weak of Type.t
 | Word of WordSize.t
```

**Expression** ([ssa-tree.sig](ssa-tree.sig)):
- Simple expressions (variables, constants, tuples, selects)
- Constructor applications
- Primitive applications
- No control flow (handled by Transfer)

**Statement**:
- Optional variable binding
- Type annotation
- Expression to evaluate

**Transfer** (control flow):
- Terminates basic blocks
- Specifies control flow (calls, jumps, cases, returns)
- Includes parallel primitives (Spork, Spoin)

**Block**:
- Label for identification
- Formal parameters (with types)
- Sequence of statements
- Transfer (control flow)

**Function**:
- Name
- Formal arguments
- Entry block
- Collection of blocks
- Return type

## Optimization Passes

### Pass List (in order)

The main SSA simplification runs passes in this carefully chosen order:

#### Initial Cleanup (1-6)
1. **removeUnused1** - Remove dead code from closure conversion
2. **introduceLoops1** - Recognize loop patterns for optimization
3. **loopInvariant1** - Move loop-invariant code (before inlining obscures it)
4. **inlineLeaf1**, **inlineLeaf2** - Inline simple leaf functions
5. **contify1** - Convert to continuation-passing style
6. **localFlatten1** - Flatten local tuples

#### Main Optimizations (7-25)
7. **constantPropagation** - Propagate and fold constants
8. **useless** - Remove useless tuple slots made constant
9. **simplifyTypes** - Simplify type representations
10. **polyEqual**, **polyHash** - Specialize polymorphic operations
11. **introduceLoops2**, **loopInvariant2** - More loop optimization
12. **contify2** - Second contification pass
13. **inlineNonRecursive** - Major inlining pass
14. **localFlatten2** - More local flattening
15. **removeUnused3** - Cleanup after inlining
16. **contify3** - Third contification pass
17. **introduceLoops3**, **loopInvariant3** - Final loop passes
18. **localRef** - Optimize local references
19. **flatten** - Main data representation optimization
20. **localFlatten3** - Final local flattening
21. **combineConversions** - Combine type conversions
22. **commonArg** - Common argument elimination
23. **commonSubexp1** - Common subexpression elimination
24. **commonBlock** - Common block elimination
25. **shareZeroVec** - Share zero-length arrays

#### Late Optimizations (26-29)
26. **redundantTests** - Remove redundant comparisons
27. **redundant** - Remove redundant computations
28. **knownCase2** - Specialize case expressions
29. **removeUnused4** - Final cleanup

### Pass Details

#### Inlining and Specialization

**inline.fun** - Function Inlining
- **Purpose**: Replace function calls with function body
- **Modes**:
  - `inlineLeaf`: Inline simple leaf functions (no calls)
  - `inlineNonRecursive`: Inline non-recursive functions
- **Controlled by**: `-inline` size threshold flags
- **Why**: Enables other optimizations, reduces call overhead
- **When**: Multiple times - leaf early, non-recursive after contify
- **Diagnostics**: `-diag-pass inline`

**contify.fun** - Contification
- **Purpose**: Convert function calls to continuation-passing style
- **What it does**: Identifies functions used in tail position and converts calls to jumps
- **Benefits**: Eliminates function call overhead, enables more optimization
- **Why multiple times**: After inlining exposes more opportunities
- **Based on**: Fluet and Weeks paper on contification

**known-case.fun** - Known Case Optimization
- **Purpose**: Specialize case expressions when constructor is known
- **Example**: `case (C x) of C y => ... → let y = x in ...`
- **Why**: Eliminates case dispatch, exposes more optimization
- **When**: After loop unswitching and late in pipeline

#### Constant and Expression Optimization

**constant-propagation.fun** - Constant Propagation
- **Purpose**: Propagate constants and fold constant expressions
- **Example**: `x = 5; y = x + 3` → `x = 5; y = 8`
- **Why early**: Enables many other optimizations
- **Includes**: Constant folding, algebraic simplification

**common-subexp.fun** - Common Subexpression Elimination (CSE)
- **Purpose**: Eliminate redundant computations
- **Example**: `x = a + b; y = a + b` → `x = a + b; y = x`
- **Uses**: Value numbering and dominator tree
- **Runs**: Twice (early and late) to catch redundancy

**common-arg.fun** - Common Argument Elimination
- **Purpose**: Merge function calls with same arguments
- **Example**: `f(x, y); ...; f(x, y)` → `f(x, y); ...; <reuse result>`

**common-block.fun** - Common Block Elimination
- **Purpose**: Merge identical basic blocks
- **Reduces**: Code size and improves cache locality

#### Data Representation Optimization

**flatten.fun** - Flattening
- **Purpose**: Optimize tuple/record representations
- **What it does**: Unpacks nested tuples, removes boxing
- **Example**: `((a, b), c)` → `(a, b, c)` where beneficial
- **Benefits**: Reduces indirection, improves memory layout
- **Tradeoff**: Code size vs performance

**deep-flatten.fun** - Deep Flattening (not in default pipeline)
- **Purpose**: Aggressive flattening across function boundaries
- **More expensive**: Compilation time and potential code bloat
- **Use when**: Maximum performance needed

**local-flatten.fun** - Local Flattening
- **Purpose**: Flatten tuples with local scope
- **Less aggressive**: Only within basic blocks
- **Runs**: Three times (after various passes create opportunities)

**ref-flatten.fun** - Reference Flattening
- **Purpose**: Unbox mutable references when safe
- **Example**: `ref (x, y)` → `(ref x, ref y)` when possible
- **Safety**: Requires escape analysis
- **Benefits**: Reduces allocation and indirection

**split-types.fun** - Type Splitting (disabled by default)
- **Purpose**: Split sum types for better representation
- **Currently**: Disabled due to CAS primitive incompatibility
- **Future**: Needs update for primitive polymorphic CAS

#### Loop Optimization

**introduce-loops.fun** - Loop Recognition
- **Purpose**: Identify loop patterns in control flow
- **Required by**: Loop optimization passes
- **Runs**: Three times as structure becomes clearer

**loop-invariant.fun** - Loop-Invariant Code Motion
- **Purpose**: Move loop-invariant computations outside loops
- **Example**: `for i in 0..n: x = a + b` → `x = a + b; for i in 0..n: ...`
- **Why early**: Before inlining obscures invariants
- **Runs**: Three times after introducing loops

**loop-unroll.fun** - Loop Unrolling (disabled by default)
- **Purpose**: Unroll small loops
- **Benefits**: Reduces loop overhead, enables more optimization
- **Tradeoff**: Code size
- **Disabled**: Can cause code bloat without careful heuristics

**loop-unswitch.fun** - Loop Unswitching (disabled by default)
- **Purpose**: Hoist loop-invariant conditionals
- **Example**: `for i: if c then A else B` → `if c then for i: A else for i: B`
- **Why after**: Loop-invariant code motion
- **Why before**: knownCase to cleanup

#### Redundancy Elimination

**redundant.fun** - Redundant Code Elimination
- **Purpose**: Remove computations whose results are unused
- **Uses**: Liveness analysis
- **When**: Late in pipeline after other opts

**redundant-tests.fun** - Redundant Test Elimination
- **Purpose**: Remove redundant comparisons and branches
- **Example**: `if x < 10 then if x < 10 then A` → `if x < 10 then A`
- **Uses**: Dominator information

**useless.fun** - Useless Code Elimination
- **Purpose**: Remove useless tuple components
- **Example**: After constant prop makes tuple slots constant
- **When**: After constant propagation

**remove-unused.fun** - Remove Unused Declarations
- **Purpose**: Remove dead declarations
- **Runs**: Four times (beginning, middle twice, end)
- **Why multiple**: Other passes create dead code

#### Type Optimization

**simplify-types.fun** - Type Simplification
- **Purpose**: Simplify type representations
- **What it does**: Merge equivalent types, simplify constructors
- **Why**: Enables equality/hash specialization and code sharing

**poly-equal.fun**, **poly-hash.fun** - Polymorphic Specialization
- **Purpose**: Specialize polymorphic equality and hash functions
- **Cannot omit**: Implements `MLton_equal` and `MLton_hash`
- **Why before inlining**: Specialized versions can be inlined

#### Control Flow Optimization

**simplify.fun** (overall driver) - Simplification
- **Purpose**: Coordinate optimization passes
- **Does**: Run passes in order, shrink, type-check
- **Pattern**: optimize → shrink → optimize → shrink ...

**shrink.fun** - Program Shrinking
- **Purpose**: Reduce program size between optimization passes
- **Does**: Constant folding, dead code removal, simplification
- **When**: After each major optimization
- **Why**: Keeps program size manageable

#### Miscellaneous

**combine-conversions.fun** - Conversion Combining
- **Purpose**: Combine adjacent type conversions
- **Example**: `Int32 → Int64 → Int32` → `Int32`

**duplicate-globals.fun** - Global Duplication
- **Purpose**: Duplicate small global definitions for better optimization
- **Disabled by default**: First pass (execute = false)
- **Enabled**: Second pass

**share-zero-vec.fun** - Zero-Length Vector Sharing
- **Purpose**: Share zero-length arrays/vectors
- **Why late**: After useless, simplifyTypes, inlining
- **Creates**: Comparisons with zero (for redundantTests)

**profile.fun**, **profile2.fun** - Profiling Instrumentation
- **Purpose**: Insert profiling code
- **When enabled**: `-profile` flag

**local-ref.fun** - Local Reference Optimization
- **Purpose**: Optimize locally-scoped references
- **Does**: Transform refs to SSA variables when safe

### Parallel-Specific Passes

**direct-exp.fun** - Direct Expression Lowering
- **Purpose**: Lower parallel primitives to explicit fork/join
- **Handles**:
  - `ForkJoin.par`: Binary fork-join
  - `ForkJoin.parfor`: Parallel for loops
  - `ForkJoin.alloc`: Array allocation
- **Does**:
  - Creates Spork/Spoin transfers (explicit fork/join)
  - Inserts heap limit checks
  - Creates thread objects
  - Manages GC interaction points
- **Output**: SSA with explicit Spork/Spoin control flow

**analyze.fun**, **analyze2.fun** - Parallel Analysis
- **Purpose**: Analyze parallel structure for optimization
- **Detects**:
  - Nested parallelism
  - Fork-join regions
  - Entanglement points
- **Used by**: drop-spork passes

**drop-spork.fun**, **drop-spork2.fun** - Parallel Optimization
- **Purpose**: Remove unnecessary parallelism
- **Based on**: Analysis results (analyze.fun)
- **Decides**: When sequential execution is better
- **Why**: Parallel overhead not always worth it

### Pass Ordering Rationale

**Why this specific order?**

1. **Early cleanup** (removeUnused, introduceLoops):
   - Start with clean program
   - Recognize structure before optimization obscures it

2. **Early inlining** (inlineLeaf):
   - Small functions inlined before expensive opts
   - Creates opportunities for other passes

3. **Contify early** (contify1):
   - Eliminate call overhead early
   - Enables better data flow

4. **Constant propagation early** (constantPropagation):
   - Enables most other optimizations
   - Must run early

5. **Useless after constant prop**:
   - Constant prop makes tuple components constant
   - Useless removes them

6. **Type simplification before specialization**:
   - Simplify types first
   - Then specialize equality/hash (polyEqual, polyHash)

7. **Major inlining after type ops** (inlineNonRecursive):
   - More aggressive inlining
   - After early optimizations create opportunities

8. **Flatten after inlining**:
   - Inlining exposes tuple data flow
   - Flattening can be more aggressive

9. **Common subexpression late**:
   - Other opts create redundancy
   - CSE cleans up

10. **Redundancy elimination very late**:
    - After all other opts create redundancy
    - Final cleanup phase

### Shrink Cycles

Between major optimization phases, **shrink** passes reduce program size:

```
removeUnused → opts → shrink → opts → shrink → removeUnused
```

**Why shrink between passes?**:
- Keeps program size manageable
- Removes dead code created by optimizations
- Makes subsequent passes faster
- Improves optimization effectiveness

## Type System

### Type Checking

SSA is strongly typed with type checking after every pass:

```sml
val typeCheck: Program.t -> unit
```

**Checks**:
- Variable types match usage
- Primitive applications well-typed
- Control flow preserves types
- Function calls type-correct
- Tuple projections in bounds

**When**: After every transformation pass via `Control.translatePass`

**Benefits**:
- Catch bugs early
- Maintain invariants
- Document type requirements

### Type Invariants

**SSA Type Properties**:
1. **Monomorphic**: All type variables specialized
2. **First-order**: Functions are not values (except via closures)
3. **Explicit**: All types annotated (no inference needed)
4. **Simple**: No higher-order types (closures explicit)

**Type Representation**:
- Primitives: Int, Real, Word (sized)
- Aggregates: Tuple, Array, Vector
- References: Ref, Weak
- Datatypes: Explicit tycon with constructors
- Special: Thread, CPointer, IntInf

## Development Guide

### Adding a New Pass

1. **Create pass file**: `mlton/ssa/my-pass.fun`

```sml
(* my-pass.fun *)
functor MyPass (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
   open S
   open Exp Transfer

   structure Program = Program
   structure Function = Function
   structure Block = Block

   fun transform (program: Program.t): Program.t =
      let
         (* Your optimization here *)
         val Program.T {datatypes, globals, functions, main} = program

         val functions =
            Vector.map (functions, fn f =>
               let
                  val {name, args, blocks, ...} = Function.dest f
                  (* Transform blocks *)
                  val blocks = Vector.map (blocks, transformBlock)
               in
                  Function.new {name = name, args = args,
                                blocks = blocks, ...}
               end)
      in
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      end
end
```

2. **Add to simplify.fun**:

```sml
(* In mlton/ssa/simplify.fun *)
structure MyPass = MyPass (S)

(* Add to ssaPassesDefault list *)
{name = "myPass", doit = MyPass.transform, execute = true} ::
```

3. **Choose position**: Consider pass dependencies
   - Does it need constant propagation first?
   - Should it run before or after inlining?
   - Does it create dead code (needs removeUnused after)?

4. **Add diagnostics** (optional):

```sml
val () = Control.diagnostic
         (fn () => Layout.str "MyPass: optimized N functions")
```

5. **Test**:
```bash
# Rebuild compiler
make compiler

# Test with diagnostics
mpl -diag-pass myPass test.mlb

# Verify output
mpl -keep-pass myPass test.mlb
```

### Understanding a Pass

To understand what a pass does:

1. **Read header comments**: Most passes have good documentation
2. **Look at signature**: `SSA_TRANSFORM` structure
3. **Find transform function**: Main entry point
4. **Check simplify.fun**: See where it runs in pipeline
5. **Use diagnostics**: Run with `-diag-pass <passName>`

### Debugging a Pass

**View IR before/after pass**:
```bash
mpl -keep-pass myPass program.mlb
# Produces program.myPass.ssa
```

**Enable diagnostics**:
```bash
mpl -diag-pass myPass program.mlb
```

**Type check explicitly**:
```bash
mpl -type-check true program.mlb
```

**Common issues**:
- **Type error**: Transformation not preserving types
- **Infinite loop**: Pass not reaching fixed point
- **Segfault**: Generated code incorrect (check backend interaction)
- **Slowdown**: Pass too aggressive or creating code bloat

### Common Pass Patterns

**Basic traversal**:
```sml
fun transformBlock (Block.T {label, args, statements, transfer}) =
   let
      val statements = Vector.map (statements, transformStmt)
      val transfer = transformTransfer transfer
   in
      Block.T {label = label, args = args,
               statements = statements, transfer = transfer}
   end
```

**Statement transformation**:
```sml
fun transformStmt (stmt as Statement.T {var, ty, exp}) =
   case exp of
      PrimApp {prim, targs, args} =>
         (* Optimize primitive application *)
         Statement.T {var = var, ty = ty, exp = newExp}
    | _ => stmt
```

**Using property lists** (for analysis):
```sml
(* Attach info to variables *)
val {get, set, ...} =
   Property.getSetOnce (Var.plist, Property.initRaise)

(* Analysis pass *)
val () = Vector.foreach (args, fn x => set (x, analysisInfo))
```

### Example: Adding Optimization

**Task**: Optimize `x + 0` → `x`

```sml
functor SimplifyArithmetic (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
   open S

   fun transformExp exp =
      case exp of
         Exp.PrimApp {prim = Prim.Word_add _,
                      args = #[x, y], ...} =>
            (* Check if y is zero *)
            if isZero y
            then Exp.Var x
            else exp
       | _ => exp

   fun transform program =
      (* Apply transformExp to all expressions *)
      ...
end
```

## Closure Conversion

While closure conversion happens at the SXML → SSA boundary, it's worth understanding how it interacts with SSA:

### Closure Conversion Analysis

Closure conversion performs several important tasks:
- Converts higher-order functions to first-order with explicit closures
- Analyzes function and loop body sizes
- Can make optimization decisions based on code analysis
- Produces well-formed SSA as output

**Common analyses**:
- Function size estimation for inlining decisions
- Loop structure analysis
- Escape analysis for data representation

**Viewing closure conversion diagnostics**:
```bash
mpl -diag-pass closureConvert program.mlb
```

## SSA vs SSA2

### When SSA Becomes SSA2

After main SSA optimization, program converts to SSA2:

**toSsa2** ([ssa-to-ssa2.fun](ssa-to-ssa2.fun)):
- Converts SSA to simplified SSA2
- Restricts constructs for backend
- Final type specialization

### Differences

**SSA2** is more restricted:
- Simpler expression language
- Explicit runtime representations
- Preparation for backend (RSSA)

**SSA2 passes** ([simplify2.fun](simplify2.fun)):
- Similar structure to SSA passes
- Fewer optimizations (main work done)
- Focus on cleanup and backend prep

## Common Issues

**Issue**: Pass causes type error
- **Solution**: Check that transformations preserve types
- **Debug**: Use `-type-check true`, examine IR with `-keep-pass`

**Issue**: Pass doesn't optimize expected code
- **Solution**: Check pass ordering - may need other passes first
- **Debug**: Use `-diag-pass` to see what pass does

**Issue**: Optimization regresses performance
- **Solution**: May be too aggressive (code bloat) or wrong heuristic
- **Debug**: Compare IR before/after, check code size

**Issue**: Pass runs forever
- **Solution**: Check fixed-point computation, may not terminate
- **Debug**: Add iteration count limit, use diagnostics

**Issue**: Generated code segfaults
- **Solution**: Transformation may violate invariants
- **Debug**: Check GC interaction, heap limit checks, type correctness

## See Also

- [ssa-tree.sig](ssa-tree.sig) - SSA data structure definitions
- [ssa.fun](ssa.fun) - SSA implementation
- [simplify.fun](simplify.fun) - Main optimization driver
- [simplify2.fun](simplify2.fun) - SSA2 optimization driver
- [ssa-to-ssa2.fun](ssa-to-ssa2.fun) - SSA to SSA2 conversion
- [../closure-convert/closure-convert.fun](../closure-convert/closure-convert.fun) - Closure conversion (SXML → SSA)
- [../README.md](../README.md) - Compiler overview
- [../control/README.md](../control/README.md) - Compiler flags
- [../atoms/README.md](../atoms/README.md) - Primitives

## Key Files

### Core SSA Structure
- [ssa-tree.sig](ssa-tree.sig), [ssa-tree.fun](ssa-tree.fun) - SSA IR definition
- [ssa.sig](ssa.sig), [ssa.fun](ssa.fun) - SSA module
- [ssa2.sig](ssa2.sig), [ssa2.fun](ssa2.fun) - SSA2 module
- [type-check.fun](type-check.fun), [type-check2.fun](type-check2.fun) - Type checking

### Optimization Drivers
- [simplify.fun](simplify.fun) - SSA optimization passes
- [simplify2.fun](simplify2.fun) - SSA2 optimization passes
- [shrink.fun](shrink.fun), [shrink2.fun](shrink2.fun) - Shrinking
- [prepasses.fun](prepasses.fun), [prepasses2.fun](prepasses2.fun) - Pre-processing

### Inlining and Specialization
- [inline.fun](inline.fun) - Function inlining
- [contify.fun](contify.fun) - Contification
- [known-case.fun](known-case.fun) - Case specialization
- [poly-equal.fun](poly-equal.fun), [poly-hash.fun](poly-hash.fun) - Polymorphic ops

### Data Representation
- [flatten.fun](flatten.fun) - Main flattening
- [deep-flatten.fun](deep-flatten.fun) - Deep flattening
- [local-flatten.fun](local-flatten.fun) - Local flattening
- [ref-flatten.fun](ref-flatten.fun) - Reference flattening
- [split-types.fun](split-types.fun) - Type splitting

### Loop Optimization
- [introduce-loops.fun](introduce-loops.fun) - Loop recognition
- [loop-invariant.fun](loop-invariant.fun) - Invariant code motion
- [loop-unroll.fun](loop-unroll.fun) - Loop unrolling
- [loop-unswitch.fun](loop-unswitch.fun) - Loop unswitching

### Expression Optimization
- [constant-propagation.fun](constant-propagation.fun) - Constants
- [common-subexp.fun](common-subexp.fun) - CSE
- [common-arg.fun](common-arg.fun), [common-block.fun](common-block.fun) - Common code
- [redundant.fun](redundant.fun), [redundant-tests.fun](redundant-tests.fun) - Redundancy

### Parallel Passes
- [direct-exp.fun](direct-exp.fun), [direct-exp2.fun](direct-exp2.fun) - Parallel primitive lowering
- [analyze.fun](analyze.fun), [analyze2.fun](analyze2.fun) - Parallel analysis
- [drop-spork.fun](drop-spork.fun), [drop-spork2.fun](drop-spork2.fun) - Parallel optimization

### Utilities
- [remove-unused.fun](remove-unused.fun), [remove-unused2.fun](remove-unused2.fun) - Dead code
- [simplify-types.fun](simplify-types.fun) - Type simplification
- [useless.fun](useless.fun) - Useless code elimination
- [combine-conversions.fun](combine-conversions.fun) - Type conversions
- [profile.fun](profile.fun), [profile2.fun](profile2.fun) - Profiling
- [restore.fun](restore.fun), [restore2.fun](restore2.fun) - IR restoration

### Conversion
- [ssa-to-ssa2.fun](ssa-to-ssa2.fun) - SSA → SSA2
- [zone.fun](zone.fun) - Zone analysis for backend

## Statistics

- **Total SSA files**: ~81 source files (.sig, .fun, .sml)
- **Optimization passes**: 20+ in default pipeline
- **Lines of code**: ~15,000+ lines (est.)
- **Type checking**: After every transformation
- **Main bottleneck**: Most compilation time spent here
