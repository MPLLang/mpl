# MLton Elaboration

The elaboration phase transforms Abstract Syntax Trees (AST) from the front-end into explicitly-typed CoreML, performing type inference, module elaboration, and overload resolution.

## Overview

Elaboration is the semantic analysis phase of the MLton compiler. It takes the syntactically correct AST produced by the [front-end](../front-end/) and produces CoreML—an explicitly-typed intermediate representation with all types inferred, modules expanded, and overloads resolved.

**Key responsibilities**:
- **Type inference**: Infer types for all expressions using Hindley-Milner algorithm
- **Module elaboration**: Expand structures, signatures, and functors
- **Overload resolution**: Resolve polymorphic operators (`+`, `*`, etc.)
- **Scope management**: Bind variables to definitions, handle shadowing
- **Error reporting**: Provide detailed type error messages with source locations
- **Semantic checking**: Enforce Standard ML semantic rules

After elaboration, the program is ready for [defunctorization](../defunctorize/), which eliminates the module system and produces XML IR.

## Key Concepts

### Type Inference (Hindley-Milner)

MLton uses the Hindley-Milner type inference algorithm with extensions for ML features:

**Core algorithm**:
1. **Generate constraints**: Walk the AST, creating type equations
2. **Unification**: Solve equations by making types identical
3. **Generalization**: Abstract over type variables at `val` bindings
4. **Instantiation**: Create fresh type variables when using polymorphic values

**Extensions for Standard ML**:
- **Overloading**: Defer resolution of `+`, `*`, `<`, etc.
- **Equality types**: Track which type variables admit equality (`''a`)
- **Record types**: Row polymorphism for extensible records
- **Datatype generativity**: Each datatype declaration creates a fresh type
- **Type aliases**: Expand type synonyms

**Example**:
```sml
fun map f []      = []
  | map f (x::xs) = f x :: map f xs
```

Inference produces:
```sml
val map : ('a -> 'b) -> 'a list -> 'b list
```

### Module System Elaboration

The elaboration phase expands Standard ML's module system:

**Structures**:
- Resolve structure paths (`A.B.C.x`)
- Apply functors to arguments
- Handle structure bindings and sharing

**Signatures**:
- Match structures against signature specifications
- Enforce abstraction (opaque vs. transparent signatures)
- Check signature ascription (`:>` vs. `:`)

**Functors**:
- Type-check functor bodies
- Handle functor application
- Propagate type information through functor boundaries

**Signature matching**:
```sml
structure S :> SIG = struct ... end  (* opaque *)
structure S : SIG = struct ... end   (* transparent *)
```

Opaque ascription hides implementation details; transparent ascription allows type information to leak.

### Overload Resolution

Standard ML allows certain operators to be overloaded:

**Overloaded operators**:
- Arithmetic: `+`, `-`, `*`, `div`, `mod`, `/`, `~`, `abs`
- Comparison: `<`, `<=`, `>`, `>=`

**Resolution strategy**:
1. **Collect constraints**: Gather uses of overloaded operators
2. **Propagate types**: Use surrounding context to narrow possibilities
3. **Defer resolution**: Keep options open as long as possible
4. **Resolve at binding**: Choose specific type at `val` binding
5. **Default types**: Use `int` for unresolved integer operations, etc.

**Example**:
```sml
fun f x = x + x        (* Overloaded: could be int, word, real *)
val _ = f 3            (* Resolves to int -> int *)
val _ = f 3.14         (* ERROR: already resolved to int *)
```

### Type Environment

The type environment tracks bindings during elaboration:

**Environment components**:
- **Value environment**: Maps variables to type schemes
- **Type environment**: Maps type constructors to kinds
- **Constructor environment**: Maps data constructors to types
- **Structure environment**: Maps structure names to signatures
- **Signature environment**: Maps signature names to specifications
- **Functor environment**: Maps functor names to signatures

**Scoping**:
- Nested environments form a stack
- Inner bindings shadow outer bindings
- `local ... in ... end` controls visibility

**Type schemes**:
```sml
val id : 'a -> 'a       (* Scheme: ∀α. α → α *)
```

Type schemes capture polymorphism—each use of `id` gets fresh type variables.

## File Organization

### Core Elaboration Files

| File | Lines | Purpose |
|------|-------|---------|
| [elaborate.sig](elaborate.sig) | ~30 | Main elaboration signature |
| [elaborate.fun](elaborate.fun) | ~30 | Elaboration driver functor |
| [elaborate-core.sig](elaborate-core.sig) | ~30 | Core expression elaboration signature |
| [elaborate-core.fun](elaborate-core.fun) | ~5,000 | Core expression/declaration elaboration |
| [elaborate-modules.sig](elaborate-modules.sig) | ~30 | Module elaboration signature |
| [elaborate-modules.fun](elaborate-modules.fun) | ~450 | Structure/signature/functor elaboration |
| [elaborate-mlbs.sig](elaborate-mlbs.sig) | ~25 | MLB elaboration signature |
| [elaborate-mlbs.fun](elaborate-mlbs.fun) | ~350 | ML Basis file elaboration |
| [elaborate-programs.sig](elaborate-programs.sig) | ~25 | Program elaboration signature |
| [elaborate-programs.fun](elaborate-programs.fun) | ~50 | Top-level program elaboration |
| [elaborate-sigexp.sig](elaborate-sigexp.sig) | ~20 | Signature expression elaboration signature |
| [elaborate-sigexp.fun](elaborate-sigexp.fun) | ~900 | Signature expression elaboration |

### Type System Files

| File | Lines | Purpose |
|------|-------|---------|
| [type-env.sig](type-env.sig) | ~160 | Type environment signature |
| [type-env.fun](type-env.fun) | ~3,000 | Type representation, unification, schemes |
| [elaborate-env.sig](elaborate-env.sig) | ~300 | Elaboration environment signature |
| [elaborate-env.fun](elaborate-env.fun) | ~5,500 | Full elaboration environment |

### Supporting Files

| File | Lines | Purpose |
|------|-------|---------|
| [interface.sig](interface.sig) | ~250 | Module interface signature |
| [interface.fun](interface.fun) | ~2,000 | Module interface implementation |
| [scope.sig](scope.sig) | ~20 | Scope resolution signature |
| [scope.fun](scope.fun) | ~500 | Add implicit type variable scoping |
| [precedence-parse.sig](precedence-parse.sig) | ~25 | Precedence parsing signature |
| [precedence-parse.fun](precedence-parse.fun) | ~450 | Parse fixity declarations (infix/infixr) |
| [decs.sig](decs.sig) | ~30 | CoreML declaration wrapper signature |
| [decs.fun](decs.fun) | ~20 | CoreML declaration wrapper |

### Build System

- [sources.mlb](sources.mlb) - ML Basis file for building elaboration
- [sources.cm](sources.cm) - CM file (legacy)

## Elaboration Workflow

### High-Level Process

**Input**: `Ast.Basdec.t` (MLB basis declaration) or `Ast.Program.t` (SML program)

**Output**: `CoreML.Dec.t list` (explicitly-typed CoreML declarations)

**Phases**:
1. **Scope resolution** ([scope.fun](scope.fun))
   - Add implicit type variable scoping to `val` and `fun` declarations
   - Determine which type variables are bound where

2. **Precedence parsing** ([precedence-parse.fun](precedence-parse.fun))
   - Resolve infix/infixr operator applications
   - Transform flat application lists into proper trees

3. **Core elaboration** ([elaborate-core.fun](elaborate-core.fun))
   - Type inference for expressions
   - Elaborate declarations
   - Generate CoreML IR

4. **Module elaboration** ([elaborate-modules.fun](elaborate-modules.fun))
   - Expand structures, signatures, functors
   - Signature matching and ascription

5. **Overload resolution**
   - Resolve deferred overloaded operators
   - Choose concrete types (int, word, real, etc.)

6. **Semantic checking**
   - Exhaustiveness checking for pattern matches
   - Redundant pattern checking
   - Sequence non-unit warnings

### Expression Elaboration

**Expression elaboration** ([elaborate-core.fun](elaborate-core.fun) `elabExp`):

1. **Create fresh type variable** for expression result
2. **Recursively elaborate** subexpressions
3. **Generate constraints** based on expression form
4. **Unify types** to solve constraints
5. **Return** CoreML expression with inferred type

**Example: Function application**
```sml
e1 e2
```
- Elaborate `e1` with type `t1`
- Elaborate `e2` with type `t2`
- Unify `t1` with `t2 -> t_result` (where `t_result` is fresh)
- Result has type `t_result`

**Example: Case expression**
```sml
case e of p1 => e1 | p2 => e2
```
- Elaborate `e` with type `t_pat`
- Elaborate pattern `p1`, unify with `t_pat`, extend environment
- Elaborate `e1` with type `t1`
- Similar for `p2` and `e2`
- Unify `t1` and `t2` (all branches must have same type)
- Result has type `t1`

### Declaration Elaboration

**Declaration elaboration** ([elaborate-core.fun](elaborate-core.fun) `elaborateDec`):

**Value bindings** (`val`):
1. Elaborate patterns and expressions
2. Unify pattern types with expression types
3. Generalize type variables (create schemes)
4. Extend environment with new bindings

**Function bindings** (`fun`):
1. Pre-process to add implicit type variables (scope)
2. Create mutually recursive bindings
3. Elaborate function bodies with function in scope
4. Generalize type variables
5. Check for value restriction (no generalization for non-values)

**Datatype declarations**:
1. Create fresh type constructor
2. Create constructors with appropriate types
3. Extend type and constructor environments
4. Handle `withtype` clauses

**Exception declarations**:
1. Create exception constructor
2. Extend environment with exception binding

### Module Elaboration

**Structure elaboration** ([elaborate-modules.fun](elaborate-modules.fun)):

**Structure bindings**:
```sml
structure S = E
```
- Elaborate structure expression `E`
- Produce structure environment
- Bind `S` in structure environment

**Signature ascription**:
```sml
structure S :> SIG = E
```
- Elaborate `E` to get structure
- Elaborate `SIG` to get signature
- Match structure against signature
- Apply abstraction (opaque `:>` hides types)

**Functor application**:
```sml
structure S = F(A)
```
- Look up functor `F`
- Elaborate argument `A`
- Match argument against functor parameter signature
- Apply functor body with argument substituted

### Signature Elaboration

**Signature expressions** ([elaborate-sigexp.fun](elaborate-sigexp.fun)):

**Signature specifications**:
```sml
sig
  type t
  val x : int
  structure S : SUBSIG
end
```
- Elaborate each specification
- Build interface describing signature
- Track flexible type constructors (can be matched)

**Signature matching**:
- Check structure provides all required components
- Verify types match (or are more specific)
- Enforce abstraction for opaque signatures

## Type Representation

### Type Structure

Types are represented with unification:

```sml
datatype t =
   Con of Tycon.t * t vector      (* Type constructor application *)
 | Record of t SortedRecord.t     (* Record type *)
 | Var of Tyvar.t                 (* Type variable *)
```

**Unification variables**:
- Implemented with union-find data structure
- Side-effecting: unification modifies type variables
- Enables efficient constraint solving

**Type schemes**:
```sml
Scheme.t = {tyvars: Tyvar.t vector, ty: Type.t}
```

Represents polymorphic types `∀α₁...αₙ. τ`.

### Unification Algorithm

**`Type.unify`** ([type-env.fun](type-env.fun)):

```sml
fun unify (t1, t2) =
   case (t1, t2) of
      (Var α, Var β) => (* unify variables *)
    | (Var α, t) => (* bind α to t (occurs check) *)
    | (t, Var α) => (* bind α to t *)
    | (Con (c1, ts1), Con (c2, ts2)) =>
         if Tycon.equals (c1, c2)
            then Vector.foreach2 (ts1, ts2, unify)
            else error "type constructor mismatch"
    | (Record r1, Record r2) =>
         (* unify corresponding fields *)
    | _ => error "type mismatch"
```

**Occurs check**: Prevent infinite types (e.g., `α = α list`)

**Error reporting**: Produces detailed messages showing where types clash

### Type Generalization

**Generalization** ([type-env.fun](type-env.fun) `close`):

At `val` bindings, abstract over type variables:

```sml
val f = fn x => x
(* Generalize: f : ∀α. α → α *)
```

**Value restriction**:
Only generalize non-expansive expressions (values):
```sml
val f = fn x => x        (* Generalizes: 'a -> 'a *)
val r = ref NONE         (* Doesn't generalize: ?.t ref *)
```

Prevents unsoundness in presence of mutable references.

### Scheme Instantiation

**Instantiation** ([type-env.fun](type-env.fun) `Scheme.instantiate`):

When using a polymorphic value, create fresh type variables:

```sml
fun instantiate (Scheme {tyvars, ty}) =
   let
      val fresh = Vector.map (tyvars, fn _ => Type.new ())
      val ty' = Type.substitute (ty, tyvars, fresh)
   in
      ty'
   end
```

**Example**:
```sml
val id : 'a -> 'a = fn x => x
val _ = id 3       (* Instantiate: int -> int *)
val _ = id "hi"    (* Instantiate: string -> string *)
```

## Overload Resolution

### Overload Representation

**Overloaded operators** are represented as:

```sml
Vid.Overload (priority, variants)
```

Where `variants` is a vector of `(Var, Scheme)` pairs representing possible types.

**Example**: The `+` operator
```sml
[(intAdd, int * int -> int),
 (wordAdd, word * word -> word),
 (realAdd, real * real -> real)]
```

### Resolution Algorithm

**Resolution** ([elaborate-core.fun](elaborate-core.fun) `resolveOverloads`):

1. **Collect overload checks**: During elaboration, defer overload resolution
2. **Sort by priority**: Process higher priority overloads first
3. **Try each variant**: Attempt unification with each possible type
4. **Pick first success**: Use the first variant that unifies
5. **Error if none**: If no variant works, report type error

**Priority system**:
- User can specify priorities with `_overload` primitive
- Higher priority = resolved first
- Allows controlling ambiguous cases

**Example**:
```sml
val x = 3 + 4
```
- `3` has type `int` (default)
- `+` is overloaded
- Try unifying with `int * int -> int` ✓
- Resolve `+` to `intAdd`

## Environment Management

### Environment Structure

**Elaborate environment** ([elaborate-env.fun](elaborate-env.fun)):

```sml
structure Env =
   struct
      type t = {
         vals: Vid.t NameMap,           (* value identifiers *)
         tycons: TypeStr.t NameMap,     (* type constructors *)
         strs: t NameMap,               (* structures *)
         sigs: Interface.t NameMap,     (* signatures *)
         fcts: FunctorEnv NameMap       (* functors *)
      }
   end
```

**Nested scopes**:
- Environments form a stack
- `open` pulls bindings into current scope
- `local ... in ... end` creates temporary scope

### Value Environment

**Value identifiers** (`Vid.t`):

```sml
datatype Vid.t =
   Con of CoreML.Con.t                  (* data constructor *)
 | Exn of CoreML.Con.t                  (* exception constructor *)
 | Overload of ... * (Var * Scheme) vector   (* overloaded operator *)
 | Var of CoreML.Var.t                  (* variable *)
```

Distinguishes constructors from variables for pattern matching.

### Type Environment

**Type structures** (`TypeStr.t`):

```sml
datatype TypeStr.node =
   Datatype of {cons: Cons.t, tycon: Tycon.t}
 | Scheme of Scheme.t                   (* type alias *)
 | Tycon of Tycon.t                     (* abstract type *)
```

**Kind tracking**:
- `Arity n`: Type constructor takes `n` arguments
- Example: `list` has arity 1, `int` has arity 0

**Equality types**:
- Track which types admit equality (`=`, `<>`)
- `AdmitsEquality.t`: `Always`, `Never`, `Sometimes`

## Fixity and Precedence

### Infix Declarations

**Fixity** ([precedence-parse.fun](precedence-parse.fun)):

```sml
infix 6 + -
infixr 5 ::
```

- `infix`: Left-associative
- `infixr`: Right-associative
- Precedence: Higher numbers bind tighter

**Precedence parsing**:
```sml
1 + 2 * 3        (* Parses as: 1 + (2 * 3) *)
x :: y :: z      (* Parses as: x :: (y :: z) *)
```

**Flat application**:
Front-end produces flat lists: `[exp1, op, exp2, op, exp3]`

Precedence parser reconstructs proper tree based on fixity.

### Operator Fixity

**Default fixities** (Standard ML Basis):
```sml
infix 7 * / div mod
infix 6 + - ^
infixr 5 :: @
infix 4 = <> < > <= >=
infix 3 := o
```

**Custom fixity**:
```sml
infixr 6 @@
fun x @@ y = x ^ " " ^ y
val s = "hello" @@ "world"
```

## Error Reporting

### Type Errors

**Unification failures** produce detailed error messages:

```
Error: foo.sml 12.10-12.15
  Type mismatch:
    expected: int
    found:    string
  in expression: x + "hello"
```

**Type pretty-printing**:
- Show type variables as `'a`, `'b`, etc.
- Expand type aliases for clarity
- Show record types in sorted order

### Exhaustiveness Checking

**Pattern match exhaustiveness** ([elaborate-core.fun](elaborate-core.fun)):

```sml
fun f NONE = 0
  (* Warning: pattern match not exhaustive *)
```

**Redundancy checking**:
```sml
fun f NONE = 0
  | f SOME _ = 1
  | f NONE = 2    (* Warning: redundant pattern *)
```

Implemented using decision tree analysis.

### Scope Errors

**Unbound variables**:
```
Error: foo.sml 5.10
  Unbound variable: x
```

**Unbound type constructors**:
```
Error: foo.sml 3.12
  Unbound type constructor: foo
```

## CoreML Output

### CoreML Structure

Elaboration produces **CoreML IR** ([core-ml](../core-ml/)):

**Explicitly-typed expressions**:
```sml
CoreML.Exp.t
```
- Each expression annotated with inferred type
- Type schemes at value bindings
- All overloads resolved

**Example transformation**:

**Input (AST)**:
```sml
fun map f [] = []
  | map f (x::xs) = f x :: map f xs
```

**Output (CoreML)**:
```sml
val map : ∀'a 'b. ('a -> 'b) -> 'a list -> 'b list =
   fn f => fn xs =>
      case xs of
         [] => []
       | x::xs => (f x) :: (map f xs)
```

All types explicit, module system still present.

## Development Guide

### Modifying Type Inference

To modify type inference behavior:

1. **Locate elaboration** in [elaborate-core.fun](elaborate-core.fun)
   - Find `elabExp` for expressions
   - Find `elaborateDec` for declarations

2. **Modify constraint generation**
   - Add new type equations for new language features
   - Call `Type.unify` to solve constraints

3. **Update generalization**
   - Modify value restriction rules in `close` function
   - Control which type variables get generalized

4. **Test thoroughly**
   - Run regression tests: `./bin/regression`
   - Test edge cases (recursive types, polymorphism, etc.)

**Example: Adding a new primitive**:
```sml
(* In elabExp *)
case exp of
   ...
 | Ast.Exp.MyNewPrim {arg, ...} =>
      let
         val argType = elabExp arg
         val resultType = Type.new ()
         val () = unify (argType, Type.int)  (* Require int argument *)
         val () = unify (resultType, Type.string)  (* Produce string *)
      in
         (CoreML.Exp.MyNewPrim arg, resultType)
      end
```

### Adding Module System Features

To extend the module system:

1. **Modify AST** ([ast](../ast/)) for new syntax

2. **Extend interface** ([interface.fun](interface.fun))
   - Add new signature specification forms
   - Update signature matching

3. **Update module elaboration** ([elaborate-modules.fun](elaborate-modules.fun))
   - Add cases for new structure/signature forms
   - Implement elaboration logic

4. **Test signature matching**
   - Verify abstraction works correctly
   - Check sharing constraints

### Debugging Elaboration

**Diagnostics**:
```bash
# Enable elaboration tracing (if implemented)
mpl -debug-elaborate true program.mlb

# View CoreML output
mpl -keep-coreml program.mlb
cat program.mlb.coreml
```

**Common issues**:
- **Infinite type**: Occurs check failed (e.g., `'a = 'a list`)
- **Unresolved overload**: Ambiguous type, add annotation
- **Signature mismatch**: Structure doesn't match signature requirements

**Debugging techniques**:
1. Add type annotations to narrow down errors
2. Break complex expressions into smaller pieces
3. Check signature matching step-by-step
4. Examine environment state at error point

## Semantic Checks

### Exhaustiveness Checking

Pattern match exhaustiveness is checked during elaboration:

**Implementation**:
- Build decision tree for patterns
- Check if all constructors are covered
- Warning for non-exhaustive matches (controlled by `-non-exhaustive-match` flag)

**Example**:
```sml
fun f (SOME x) = x
  (* Warning: match not exhaustive *)
  (* Missing: NONE *)
```

### Redundancy Checking

Redundant patterns are detected:

```sml
fun f 0 = "zero"
  | f n = "nonzero"
  | f 1 = "one"      (* Warning: redundant *)
```

Pattern `f 1` is redundant because `f n` already covers it.

### Value Restriction

The value restriction prevents unsound generalization:

**Sound**:
```sml
val id = fn x => x               (* Generalizes to 'a -> 'a *)
```

**Unsound without restriction**:
```sml
val r = ref NONE                 (* Should NOT generalize to 'a option ref *)
val _ = r := SOME 3
val _ = case !r of SOME s => s   (* Would allow extracting 3 as a string! *)
```

**Value restriction**: Only generalize non-expansive expressions (values).

## Performance Considerations

**Unification**:
- Union-find with path compression: Nearly constant time
- Occurs check: Linear in type size (can be expensive for large types)

**Environment lookup**:
- Nested environments: Linear in depth
- Can be slow for deeply nested modules
- Consider environment compression for production

**Signature matching**:
- Can be expensive for large signatures
- Avoid redundant re-matching
- Cache results when possible

**Overload resolution**:
- Deferred resolution adds overhead
- Priority-based resolution is linear in number of variants
- Usually fast in practice (few variants)

## Examples

### Example: Type Inference

**Input**:
```sml
fun map f [] = []
  | map f (x::xs) = f x :: map f xs
```

**Elaboration process**:
1. Create type variables: `f : 'a`, `[] : 'b`, `x : 'c`, `xs : 'd`, etc.
2. From first clause: `'b = 'e list`, result type `'f list`
3. From second clause: `'g list = 'c :: 'd`, so `'g = 'c` and `'d = 'c list`
4. Unify: `f 'c` has type `'h`, and `'h :: map f xs` has type `'h list`
5. Result type is `'h list`, so `'f = 'h`
6. Solve: `f : 'c -> 'h`, input: `'c list`, output: `'h list`
7. Generalize: `('c -> 'h) -> 'c list -> 'h list`
8. Rename: `('a -> 'b) -> 'a list -> 'b list`

**Output**:
```sml
val map : ('a -> 'b) -> 'a list -> 'b list
```

### Example: Overload Resolution

**Input**:
```sml
fun double x = x + x
val n = double 5
val r = double 2.5
```

**Elaboration**:
1. In `double`: `+` is overloaded, create constraint: `'a supports +`
2. Generalize (not possible due to overload): Wait to resolve
3. At `double 5`: Unify `'a` with `int`, resolve `+` to `intAdd`
4. At `double 2.5`: **ERROR** - already resolved to `int -> int`

**Correct version**:
```sml
fun double x = x + x : real   (* Force real type *)
val r = double 2.5             (* OK *)
```

### Example: Module Elaboration

**Input**:
```sml
signature MONOID =
sig
   type t
   val zero : t
   val add : t * t -> t
end

structure IntMonoid :> MONOID =
struct
   type t = int
   val zero = 0
   fun add (x, y) = x + y
end

val x = IntMonoid.zero
```

**Elaboration**:
1. Elaborate `MONOID` signature: Create flexible type `t`
2. Elaborate `IntMonoid` structure: `t = int`, `zero = 0`, etc.
3. Match against `MONOID`: Check all specs present, types compatible
4. Apply `:>` (opaque): Hide `t = int`, make `t` abstract
5. Bind `IntMonoid` with abstract type
6. At use site: `x : IntMonoid.t` (abstract, not `int`)

## Common Issues

### Issue: Type Variable Scope

**Symptom**: "Type variable not in scope"

**Cause**: Type variable used but not bound at `val`/`fun`

**Example**:
```sml
val f : 'a -> 'a = fn x => x    (* OK: 'a bound at val *)
val g = (fn x => x) : 'a -> 'a  (* ERROR: 'a not in scope *)
```

**Fix**: Bind type variables at value binding:
```sml
val 'a g = fn x => x : 'a       (* OK *)
```

### Issue: Value Restriction

**Symptom**: Type variable not generalized

**Cause**: Expression is not a value (is expansive)

**Example**:
```sml
val id = (fn x => x) (fn y => y)   (* Not generalized! *)
```

**Fix**: Use `fun` binding or eta-expand:
```sml
fun id x = x                       (* Generalizes *)
```

### Issue: Signature Mismatch

**Symptom**: "structure does not match signature"

**Cause**: Missing components or type mismatch

**Example**:
```sml
structure S : SIG = struct end
  (* ERROR: missing required values/types *)
```

**Fix**: Implement all signature requirements:
```sml
structure S : SIG =
struct
   type t = int
   val x = 42
   (* ... all required components *)
end
```

### Issue: Overload Ambiguity

**Symptom**: "overload not resolved"

**Cause**: Insufficient type information to choose variant

**Example**:
```sml
fun f x = x + x
  (* ERROR: can't generalize, don't know which + *)
```

**Fix**: Add type annotation:
```sml
fun f (x : int) = x + x    (* Resolves to int *)
```

## See Also

- [Front-End](../front-end/) - Parsing and AST generation
- [AST Structures](../ast/) - Abstract syntax tree definitions
- [CoreML IR](../core-ml/) - Output of elaboration
- [Defunctorize](../defunctorize/) - Next stage (CoreML → XML)
- [Control System](../control/) - Elaboration flags and options
- [Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) - Algorithm background

## References

- **The Definition of Standard ML (Revised)**: Formal specification of ML semantics
- **Types and Programming Languages** (Pierce): Type inference algorithms
- **Hindley-Milner Type Inference**: Classic algorithm for ML-style type inference
- **MLton Source Code**: Reference implementation of elaborate phase
