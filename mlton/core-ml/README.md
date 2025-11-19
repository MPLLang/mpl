# MLton CoreML IR

CoreML is a typed, explicitly-typed intermediate representation produced by type elaboration and consumed by defunctorization.

## Overview

CoreML sits between the front-end elaboration and XML in the compilation pipeline. It is produced by [type elaboration](../elaborate/), which performs type inference and module elaboration, and is consumed by [defunctorization](../defunctorize/), which eliminates the module system.

**Key characteristics**:
- **Typed**: Every expression and pattern has an explicit type
- **Module-aware**: Retains structures, signatures, and functors
- **Explicitly-typed**: Type annotations are explicit (no type inference needed)
- **First-order**: Functions are first-class values
- **Pattern matching**: Full SML pattern syntax with exhaustiveness checking
- **Dead code elimination**: Optional cleanup pass to remove unused bindings

The CoreML representation preserves the structure of elaborated SML code while making all types explicit. It serves as the last IR before module elimination and the first IR where types are fully resolved.

## Key Concepts

### CoreML Structure

**Program** ([core-ml.sig](core-ml.sig)):
```sml
Program = {decs: Dec vector}
```

A CoreML program is simply a sequence of declarations.

**Declarations** ([core-ml.sig](core-ml.sig)):
```sml
datatype Dec =
   Datatype of {cons: {arg: Type option, con: Con} vector,
                tycon: Tycon,
                tyvars: Tyvar vector} vector
 | Exception of {arg: Type option, con: Con, elab: ExnDecElab}
 | Fun of {decs: {lambda: Lambda, var: Var} vector,
           tyvars: unit -> Tyvar vector}
 | Val of {matchDiags: diagnostics,
           rvbs: {lambda: Lambda, var: Var} vector,
           tyvars: unit -> Tyvar vector,
           vbs: {exp: Exp, pat: Pat, ...} vector}
```

- **Datatype**: Algebraic datatype declarations with type variables
- **Exception**: Exception constructor declarations
- **Fun**: (Mutually) recursive function declarations
- **Val**: Value bindings (both recursive `rvbs` and non-recursive `vbs`)

**Expressions** ([core-ml.sig](core-ml.sig)):
```sml
datatype ExpNode =
   App of {func: Exp, arg: Exp, inline: InlineAttr}
 | Case of {test: Exp, rules: (Pat * Exp) vector, noMatch: noMatch, ...}
 | Con of Con * Type vector
 | Const of unit -> Const
 | EnterLeave of Exp * SourceInfo
 | Handle of {try: Exp, catch: Var * Type, handler: Exp}
 | Lambda of Lambda
 | Let of Dec vector * Exp
 | List of Exp vector
 | PrimApp of {prim: Type Prim, targs: Type vector, args: Exp vector}
 | Raise of Exp
 | Record of Exp Record
 | Seq of Exp vector
 | Var of (unit -> Var) * (unit -> Type vector)
 | Vector of Exp vector

type Exp = {node: ExpNode, ty: Type}
```

Every expression carries its type explicitly.

**Patterns** ([core-ml.sig](core-ml.sig)):
```sml
datatype PatNode =
   Con of {con: Con, targs: Type vector, arg: Pat option}
 | Const of unit -> Const
 | Layered of Var * Pat
 | List of Pat vector
 | Or of Pat vector
 | Record of Pat Record
 | Var of Var
 | Vector of Pat vector
 | Wild

type Pat = {node: PatNode, ty: Type}
```

Patterns also carry explicit types.

**Lambdas** ([core-ml.sig](core-ml.sig)):
```sml
Lambda = {arg: Var, argType: Type, body: Exp, inline: InlineAttr}
```

### Type Representation

Types in CoreML are fully resolved from elaboration:

**Type interface** ([core-ml.sig](core-ml.sig)):
```sml
structure Type:
   sig
      type t
      val arrow: t * t -> t
      val bool: t
      val deConOpt: t -> (Tycon * t vector) option
      val deRecord: t -> (Field * t) vector
      val isCharX: t -> bool
      val isInt: t -> bool
      val tuple: t vector -> t
      val unit: t
   end
```

Types come from the elaboration phase and are shared with the type environment.

### Pattern Matching

CoreML preserves full SML pattern matching:

**Case expressions**:
```sml
Case {
  test: Exp,                    (* expression being matched *)
  rules: {pat: Pat, exp: Exp, regionPat: Region, ...} vector,
  noMatch: noMatch,             (* what to do if no pattern matches *)
  matchDiags: diagnostics,      (* diagnostics for exhaustiveness/redundancy *)
  ...
}
```

**noMatch values**:
- `Impossible`: Pattern matching is provably exhaustive
- `RaiseMatch`: Raise Match exception if no pattern matches
- `RaiseBind`: Raise Bind exception (for val bindings)
- `RaiseAgain`: Re-raise current exception (for handle)

**Diagnostics**:
```sml
{
  nonexhaustiveExn: DiagDI,    (* non-exhaustive exception patterns *)
  nonexhaustive: DiagEIW,       (* non-exhaustive value patterns *)
  redundant: DiagEIW            (* redundant patterns *)
}
```

These diagnostics are computed during elaboration and preserved in CoreML for later error reporting.

### Inlining Attributes

Both function declarations and function applications carry inlining attributes:

**InlineAttr** (from [atoms](../atoms/)):
- Controls whether functions should be inlined
- Set by user annotations (`_inline`) or compiler heuristics
- Propagated through the pipeline to guide optimization

**Example**:
```sml
(* User annotation *)
val _inline f = fn x => x + 1

(* CoreML representation *)
Fun {decs = [{var = f,
              lambda = {arg = x, argType = int,
                       body = x + 1,
                       inline = InlineAttr.Always}}],
     tyvars = fn () => []}
```

## File Organization

### Core Files

| File | Lines | Purpose |
|------|-------|---------|
| [core-ml.sig](core-ml.sig) | ~200 | CoreML IR signature |
| [core-ml.fun](core-ml.fun) | ~700 | CoreML IR implementation |
| [dead-code.sig](dead-code.sig) | ~25 | Dead code elimination signature |
| [dead-code.fun](dead-code.fun) | ~80 | Dead code elimination pass |

### Build System

- [sources.mlb](sources.mlb) - ML Basis file for building CoreML
- [sources.cm](sources.cm) - CM file (legacy)

## Dead Code Elimination

### Overview

Dead code elimination removes unused value bindings from CoreML programs.

**Process** ([dead-code.fun](dead-code.fun)):
1. **Backward analysis**: Process declarations in reverse order
2. **Mark used variables**: Track which variables are actually used
3. **Filter declarations**: Remove declarations that bind only unused variables
4. **Preserve side effects**: Keep declarations that may have side effects

**Interface** ([dead-code.sig](dead-code.sig)):
```sml
val deadCode:
   {prog: (Dec list * bool) vector} ->
   {prog: Dec list vector}
```

Each element of the input vector is a list of declarations and a boolean indicating whether dead code elimination should be performed on that segment.

### Algorithm

**Key insight**: Only eliminate bindings that are provably unused and side-effect-free.

**Steps**:

1. **Initialize tracking**: Create property list to track variable usage
2. **Process in reverse**: Analyze declarations from end to beginning
3. **Mark uses**: For each declaration, mark variables used in its body
4. **Decide elimination**:
   - Keep if any bound variable is used
   - Keep if declaration may have side effects
   - Keep datatypes and exceptions (needed for type soundness)
   - Eliminate otherwise

**Implementation** ([dead-code.fun](dead-code.fun)):
```sml
fun deadCode {prog} =
   let
      val {get = varIsUsed, set = setVarIsUsed, ...} =
         Property.destGetSet (Var.plist, Property.initConst false)

      fun decIsNeeded (d: Dec) =
         case d of
            Datatype _ => true              (* always keep datatypes *)
          | Exception _ => true              (* always keep exceptions *)
          | Fun {decs, ...} =>
               Vector.exists (decs, varIsUsed o #var)
          | Val {rvbs, vbs, ...} =>
               Vector.exists (rvbs, varIsUsed o #var)
               orelse Vector.exists (vbs, patVarIsUsed o #pat)

      (* Process declarations in reverse order *)
      val prog =
         Vector.tabulate (n, fn i =>
            let val (decs, deadCode) = Vector.sub (prog, m - i)
            in
               if deadCode
                  then List.fold (rev decs, [], fn (dec, decs) =>
                          if decIsNeeded dec
                             then (useDec dec; dec :: decs)
                             else decs)
                  else (List.foreach (decs, useDec); decs)
            end)
   in {prog = Vector.rev prog}
   end
```

### Safe Elimination

**Preserve declarations that**:
- Bind variables that are used elsewhere
- Are datatype or exception declarations (needed for types)
- May have side effects (non-wild, non-unit patterns)

**Example**:
```sml
(* Input *)
val x = 1           (* unused *)
val y = print "hi"  (* side effect *)
val z = x + 2       (* uses x, so x becomes needed *)

(* After dead code elimination *)
val x = 1           (* kept because z uses it *)
val y = print "hi"  (* kept for side effect *)
val z = x + 2
```

### Pattern-Based Elimination

**Wild and unit patterns** can be eliminated if the binding is unused:

```sml
(* Can eliminate *)
val _ = f ()        (* wild pattern, unused *)
val () = g ()       (* unit pattern, unused *)

(* Cannot eliminate - may have side effects *)
val SOME x = h ()   (* refutable pattern *)
```

**Check** ([dead-code.fun](dead-code.fun)):
```sml
fun decIsWildOrUnit (d: Dec) =
   case d of
      Val {rvbs, vbs, ...} =>
         0 = Vector.length rvbs
         andalso 1 = Vector.length vbs
         andalso let
                    val pat = #pat (Vector.first vbs)
                 in
                    Pat.isWild pat orelse Pat.isUnit pat
                 end
    | _ => false
```

## Expression Properties

### Expansiveness

The `isExpansive` predicate determines whether an expression may allocate mutable state:

**Definition** (from Definition of Standard ML, page 19):
- Constants, variables, constructors: non-expansive
- Function applications, ref creation: expansive
- Tuples, records: expansive if any component is expansive
- Case, handle: expansive if any branch is expansive

**Why it matters**: Non-expansive expressions can be generalized when bound by `val`:

```sml
(* Non-expansive: can generalize *)
val id = fn x => x
(* Type: 'a -> 'a *)

(* Expansive: cannot generalize *)
val r = ref []
(* Type: ?? list ref, where ?? is NOT polymorphic *)
```

This implements the **value restriction** for type soundness.

### Variable Traversal

**foreachVar** ([core-ml.fun](core-ml.fun)) traverses an expression or pattern to find all variable uses:

```sml
fun foreachVar (exp: Exp, f: Var -> unit): unit
```

Used by:
- Dead code elimination (finding variable uses)
- Optimizations (analyzing variable occurrences)
- Type checking (verifying variable bindings)

## Profiling Support

### EnterLeave Expressions

**EnterLeave** ([core-ml.sig](core-ml.sig)):
```sml
EnterLeave of Exp * SourceInfo
```

Wraps expressions with source location information for profiling:
- Tracks execution counts at source locations
- Enables time profiling per function/expression
- Added during elaboration when profiling is enabled

**Example**:
```sml
(* Source *)
fun fact n = if n = 0 then 1 else n * fact (n - 1)

(* With profiling *)
EnterLeave (
  lambda (n) =>
    EnterLeave (
      if n = 0 then 1 else n * fact (n - 1),
      SourceInfo("fact", "file.sml", line 2)),
  SourceInfo("fact", "file.sml", line 2))
```

### Profile Removal

**dropProfile** ([core-ml.sig](core-ml.sig)):
```sml
val dropProfile: Program.t -> Program.t
```

Removes all `EnterLeave` expressions, used when compiling without profiling support.

## Layout and Pretty-Printing

### Layout Functions

CoreML provides comprehensive pretty-printing:

**Layouts** ([core-ml.sig](core-ml.sig)):
```sml
val layout: t -> Layout.t              (* for patterns, expressions, declarations *)
val layoutWithType: Exp.t -> Layout.t  (* expressions with explicit type annotations *)
val layoutStats: Program.t -> Layout.t (* program statistics *)
```

**Type display control**:
```sml
Control.showTypes := true   (* show all type annotations *)
Control.showTypes := false  (* minimal type display *)
```

**Example**:
```sml
(* With showTypes = true *)
val (x: int) = (42: int)

(* With showTypes = false *)
val x = 42
```

### Program Output

**toFile** ([core-ml.sig](core-ml.sig)):
```sml
val toFile: {display: t Control.display,
             style: Control.style,
             suffix: string}
```

Outputs CoreML program to a file for inspection:

```bash
mpl -keep-core-ml program.mlb
cat program.mlb.core-ml
```

## Comparison with Other IRs

### CoreML vs AST

**AST** (Abstract Syntax Tree):
- Directly from parser
- No type information
- Contains syntactic sugar
- Has infix operators (not yet resolved)

**CoreML**:
- After type elaboration
- Full type information on every expression/pattern
- Desugared (no syntactic conveniences)
- Infix operators resolved to prefix applications

### CoreML vs XML

**CoreML**:
- Has module system (structures, signatures, functors)
- Explicit type annotations on expressions
- Pattern matching with case expressions
- Exception declarations

**XML**:
- No module system (eliminated by defunctorization)
- Explicit type passing (variables carry type arguments)
- Simplified patterns (less syntax)
- Exceptions implemented as datatypes (in SXML)

**Example transformation**:
```sml
(* CoreML *)
fun 'a id (x: 'a): 'a = x
val n: int = id 42

(* XML *)
Fun ['a] (id : 'a -> 'a) = lambda (x : 'a) => x
MonoVal (n : int) = id [int] 42
```

## Development Guide

### Understanding CoreML Code

To understand a CoreML program:

1. **Read the signature** ([core-ml.sig](core-ml.sig)) to understand the IR structure
2. **Look at expression nodes** to see what constructs are available
3. **Check pattern nodes** to understand pattern matching
4. **Examine declarations** to see how bindings are represented

**Key insight**: CoreML is very close to source-level SML, just with explicit types everywhere.

### Modifying CoreML

**When adding language features**:

1. **Extend AST**: Add new syntax to [ast](../ast/)
2. **Elaborate**: Add elaboration rules in [elaborate](../elaborate/)
3. **CoreML representation**: Decide how feature maps to CoreML (often reuse existing nodes)
4. **Defunctorization**: Ensure [defunctorize](../defunctorize/) handles new feature
5. **Type check**: Verify types are correctly propagated

**When adding optimizations**:

1. **Before defunctorization**: Add as CoreML → CoreML pass (rare)
2. **After defunctorization**: Add as XML → XML or SSA pass (more common)

### Dead Code Elimination Modifications

To change dead code elimination:

1. **Edit criteria**: Modify `decIsNeeded` in [dead-code.fun](dead-code.fun)
2. **Add analysis**: Extend variable usage tracking
3. **Preserve correctness**: Ensure side effects are preserved
4. **Test**: Verify no needed code is eliminated

**Example**: Make dead code elimination more aggressive:

```sml
(* Allow elimination of wild pattern bindings even with side effects *)
fun decIsWildOrUnit (d: Dec) =
   case d of
      Val {vbs, ...} =>
         Vector.exists (vbs, fn {pat, ...} =>
            Pat.isWild pat orelse Pat.isUnit pat)
    | _ => false
```

**Warning**: This could eliminate expressions like `val _ = print "hi"`, which is incorrect!

### Adding Custom Passes

To add a CoreML → CoreML transformation:

1. **Create functor**: Follow pattern in [dead-code.fun](dead-code.fun)
   ```sml
   functor MyPass (S: MY_PASS_STRUCTS): MY_PASS =
   struct
      open S
      open CoreML

      fun transform (Program.T {decs}) =
         let
            val decs' = Vector.map (decs, transformDec)
         in
            Program.T {decs = decs'}
         end
   end
   ```

2. **Add signature**: Create `my-pass.sig`
3. **Wire into pipeline**: Add to compilation pipeline in [main](../main/)
4. **Test**: Ensure correctness with regression tests

## Common Patterns

### Expression Construction

**Helper functions** ([core-ml.fun](core-ml.fun)):

```sml
(* Boolean expressions *)
val truee: Exp
val falsee: Exp
val andAlso: Exp * Exp -> Exp
val orElse: Exp * Exp -> Exp
val iff: Exp * Exp * Exp -> Exp    (* if-then-else *)

(* Tuples and records *)
val tuple: Exp vector -> Exp
val unit: Exp

(* Loops *)
val whilee: {test: Exp, expr: Exp} -> Exp
```

**Example**:
```sml
(* Construct: if x > 0 andalso y > 0 then 1 else 0 *)
let
   val test = Exp.andAlso (greaterThan (x, zero),
                           greaterThan (y, zero))
   val result = Exp.iff (test, one, zero)
in
   result
end
```

### Pattern Construction

**Helper functions** ([core-ml.fun](core-ml.fun)):

```sml
val wild: Type -> Pat
val var: Var * Type -> Pat
val tuple: Pat vector -> Pat
val truee: Pat
val falsee: Pat
```

**Example**:
```sml
(* Construct pattern: (x, _, y) *)
let
   val xPat = Pat.var (x, intType)
   val wildPat = Pat.wild (stringType)
   val yPat = Pat.var (y, intType)
in
   Pat.tuple (Vector.new3 (xPat, wildPat, yPat))
end
```

### Type Queries

**Type inspection** ([core-ml.sig](core-ml.sig)):

```sml
val deConOpt: Type -> (Tycon * Type vector) option
val deRecord: Type -> (Field * Type) vector
val isCharX: Type -> bool
val isInt: Type -> bool
```

**Example**:
```sml
(* Check if expression is an int list *)
case Type.deConOpt (Exp.ty exp) of
   SOME (tycon, #[elemTy]) =>
      if Tycon.equals (tycon, Tycon.list) andalso Type.isInt elemTy
         then (* it's an int list *)
         else (* other type constructor *)
 | _ => (* not a type constructor application *)
```

## Debugging CoreML

### Viewing CoreML Output

**Generate CoreML file**:
```bash
mpl -keep-core-ml program.mlb
cat program.mlb.core-ml
```

**With types shown**:
```bash
mpl -show-types true -keep-core-ml program.mlb
```

### Common Issues

**Issue: Type mismatches in CoreML**

**Symptom**: Type checker error after elaboration

**Cause**: Type elaboration didn't correctly propagate types

**Fix**:
- Check elaboration rules in [elaborate](../elaborate/)
- Verify type unification is correct
- Ensure type applications have correct arity

**Issue: Dead code eliminated too aggressively**

**Symptom**: Program behavior changes after dead code elimination

**Cause**: Side-effecting expression was incorrectly eliminated

**Fix**:
- Check `decIsWildOrUnit` logic in [dead-code.fun](dead-code.fun)
- Verify `isExpansive` predicate is correct
- Ensure pattern refutability is correctly determined

**Issue: Pattern match not exhaustive**

**Symptom**: Warning or error about non-exhaustive patterns

**Cause**: Elaboration detected missing cases

**Fix**:
- Check `matchDiags` in Case expression
- Look at `noMatch` field (should be `RaiseMatch`, not `Impossible`)
- Add missing patterns to source code

## Examples

### Example: Simple Function

**Source**:
```sml
fun double (x: int): int = x + x
```

**CoreML**:
```sml
Fun {
  decs = [{
    var = double,
    lambda = {
      arg = x,
      argType = int,
      body = PrimApp {
        prim = Int_add,
        targs = [],
        args = [Var (x, []), Var (x, [])]
      } : int,
      inline = Default
    }
  }],
  tyvars = fn () => []
}
```

### Example: Pattern Matching

**Source**:
```sml
fun length [] = 0
  | length (x::xs) = 1 + length xs
```

**CoreML**:
```sml
Fun {
  decs = [{
    var = length,
    lambda = {
      arg = arg,
      argType = 'a list,
      body = Case {
        test = Var (arg, []),
        rules = [
          {pat = Con {con = nil, targs = ['a], arg = NONE},
           exp = Const (Int 0)},
          {pat = Con {con = cons, targs = ['a],
                     arg = SOME (Record [(x, 'a), (xs, 'a list)])},
           exp = PrimApp {prim = Int_add,
                         args = [Const (Int 1),
                                App {func = Var (length, ['a]),
                                     arg = Var (xs, [])}]}}
        ],
        noMatch = Impossible,
        ...
      },
      inline = Default
    }
  }],
  tyvars = fn () => [tyvar_a]
}
```

### Example: Exception Handling

**Source**:
```sml
fun safediv (x, y) =
   (x div y) handle Div => 0
```

**CoreML**:
```sml
Fun {
  decs = [{
    var = safediv,
    lambda = {
      arg = arg,
      argType = int * int,
      body = Handle {
        try = PrimApp {
          prim = Int_div,
          args = [Select {tuple = Var (arg, []), offset = 0},
                 Select {tuple = Var (arg, []), offset = 1}]
        },
        catch = (exn, exn),
        handler = Case {
          test = Var (exn, []),
          rules = [{
            pat = Con {con = Div, targs = [], arg = NONE},
            exp = Const (Int 0)
          }],
          noMatch = RaiseAgain,
          ...
        }
      },
      inline = Default
    }
  }],
  tyvars = fn () => []
}
```

## Performance Considerations

**Dead code elimination benefits**:
- Reduces CoreML program size before defunctorization
- Eliminates unused polymorphic functions (saves monomorphisation work)
- Speeds up later compilation stages

**Pattern matching overhead**:
- Complex patterns create large case trees
- Optimization happens later in [match-compile](../match-compile/)
- CoreML preserves source-level patterns for analysis

**Type annotations**:
- Every expression/pattern carries type information
- Enables later type-directed optimizations
- Small overhead in IR size, but necessary for correctness

## See Also

- [Elaborate](../elaborate/) - Type inference and module elaboration (produces CoreML)
- [Defunctorize](../defunctorize/) - Module elimination (consumes CoreML, produces XML)
- [XML IR](../xml/) - Next stage after defunctorization
- [Front-end](../front-end/) - Parsing and syntax analysis
- [AST](../ast/) - Abstract syntax tree (before elaboration)
- [Atoms](../atoms/) - Shared atomic values (Var, Con, Tycon, etc.)

## References

- **Definition of Standard ML (Revised)**: Formal semantics of SML type system and pattern matching
- **MLton Source Code**: CoreML IR implementation and dead code elimination
- **Value Restriction**: Type soundness in the presence of mutable references
