# MLton Defunctorization

Defunctorization eliminates the module system (structures, signatures, functors) from CoreML, producing polymorphic XML.

## Overview

Defunctorization sits between CoreML and XML in the compilation pipeline. It consumes [CoreML](../core-ml/), which contains the full SML module system, and produces [XML](../xml/), a first-order polymorphic intermediate representation without modules.

**Key transformations**:
- **Functor elimination**: Apply functors to arguments, inline functor bodies
- **Module flattening**: Replace structure paths with flat variable names
- **Signature erasure**: Remove signature ascriptions and opaque sealing
- **Pattern compilation**: Compile complex patterns to simple case expressions
- **Type translation**: Convert CoreML types to XML types with explicit type passing

After defunctorization, the program is a flat sequence of polymorphic value and function declarations, ready for monomorphisation.

## Key Concepts

### Module Elimination

**Functors**:
```sml
(* CoreML: functor application *)
functor F (X: SIG) = struct val y = X.x + 1 end
structure A = struct val x = 42 end
structure B = F(A)
val result = B.y

(* XML: functor inlined *)
val x = 42
val y = x + 1
val result = y
```

**Structures**:
```sml
(* CoreML: nested structures *)
structure A = struct
  structure B = struct
    val x = 1
  end
end
val y = A.B.x

(* XML: flattened *)
val A_B_x = 1
val y = A_B_x
```

**Signature ascription**:
```sml
(* CoreML: opaque ascription hides type definition *)
structure A :> sig type t val x: t end = struct
  type t = int
  val x = 42
end

(* XML: type abstraction becomes polymorphism *)
val 'a x = 42 : 'a
```

### Pattern Compilation

Defunctorization compiles CoreML patterns to XML case expressions using [match compilation](../match-compile/).

**Nested patterns**:
```sml
(* CoreML: nested record pattern *)
case x of
  (a, {b, c = (d, e)}) => a + b + d + e

(* XML: flattened with selects *)
let val t = x
    val a = #1 t
    val tuple2 = #2 t
    val b = #1 tuple2
    val c = #2 tuple2
    val d = #1 c
    val e = #2 c
in a + b + d + e end
```

**Layered patterns**:
```sml
(* CoreML: layered pattern *)
case xs of
  (ys as y::_) => (y, ys)

(* XML: binding with case *)
case xs of
  y::rest =>
    let val ys = y::rest
    in (y, ys) end
```

**Or patterns**:
```sml
(* CoreML: or pattern *)
case x of
  (0 | 1 | 2) => "small"
| n => "large"

(* XML: replicated case branches *)
case x of
  0 => "small"
| 1 => "small"
| 2 => "small"
| n => "large"
```

### Type Translation

CoreML types are translated to XML types with explicit type variable tracking:

**Polymorphic values** ([defunctorize.fun](defunctorize.fun)):
```sml
(* CoreML *)
fun 'a id (x: 'a) = x

(* XML *)
PolyVal ['a] (id : 'a -> 'a) =
  lambda (x : 'a) => x
```

**Type applications**:
```sml
(* CoreML *)
val n = id 42
val s = id "hello"

(* XML with explicit type arguments *)
val n = id [int] 42
val s = id [string] "hello"
```

**Constructor types**:
```sml
(* CoreML *)
datatype 'a option = NONE | SOME of 'a

(* XML: same structure, but used differently *)
Datatype option ['a] = NONE | SOME of 'a

(* Constructor applications carry type arguments *)
SOME [int] 42      (* CoreML: SOME 42 *)
NONE [string]      (* CoreML: NONE *)
```

### Match Compilation Integration

Defunctorization uses the match compilation algorithm from [match-compile](../match-compile/) to compile patterns:

**Process**:
1. **Translate patterns**: Convert CoreML patterns to NestedPat representation
2. **Build decision tree**: Create efficient case-testing structure
3. **Generate code**: Produce XML case expressions with simple patterns
4. **Check exhaustiveness**: Report non-exhaustive or redundant matches

**Match diagnostics** ([defunctorize.fun](defunctorize.fun)):
```sml
{
  nonexhaustiveExn: DiagDI,    (* non-exhaustive exception handlers *)
  nonexhaustive: DiagEIW,       (* non-exhaustive value patterns *)
  redundant: DiagEIW            (* redundant pattern clauses *)
}
```

These diagnostics come from CoreML elaboration and are used to generate warnings or errors during defunctorization.

## File Organization

### Core Files

| File | Lines | Purpose |
|------|-------|---------|
| [defunctorize.sig](defunctorize.sig) | ~20 | Defunctorization signature |
| [defunctorize.fun](defunctorize.fun) | ~1,100 | Main defunctorization pass |

### Build System

- [sources.mlb](sources.mlb) - ML Basis file for building defunctorization
- [sources.cm](sources.cm) - CM file (legacy)

## Algorithm

### Overview

Defunctorization is a single traversal of the CoreML program:

**Main function** ([defunctorize.fun](defunctorize.fun)):
```sml
val defunctorize: CoreML.Program.t -> Xml.Program.t
```

**Process**:
1. **Initialize**: Set up type and constructor mappings
2. **Process declarations**: Convert each CoreML declaration to XML
3. **Translate expressions**: Convert expressions, compiling patterns
4. **Flatten modules**: Replace structure paths with flat names
5. **Build XML program**: Assemble final XML program

### Declaration Processing

**Datatypes** ([defunctorize.fun](defunctorize.fun)):
```sml
Datatype {cons, tycon, tyvars} =>
  (* Translate directly to XML datatype *)
  Xml.Dec.Datatype {cons = translateCons cons,
                    tycon = tycon,
                    tyvars = tyvars}
```

**Exceptions**:
```sml
Exception {arg, con, elab} =>
  (* Translate to XML exception declaration *)
  Xml.Dec.Exception {arg = translateType arg,
                     con = con}
```

**Functions**:
```sml
Fun {decs, tyvars} =>
  (* Translate recursive function group *)
  Xml.Dec.Fun {
    decs = Vector.map (decs, fn {lambda, var} =>
             {lambda = translateLambda lambda,
              ty = translateType (lambdaType lambda),
              var = var}),
    tyvars = tyvars ()
  }
```

**Values**:
```sml
Val {rvbs, vbs, tyvars, matchDiags} =>
  (* Recursive value bindings *)
  let
     val recursiveDecs = translateRecursive rvbs
     (* Non-recursive pattern bindings *)
     val valueDecs = Vector.map (vbs, fn {exp, pat, ...} =>
        let
           val (xmlExp, ty) = translateExp exp
           val pat = translatePat pat
        in
           (* Compile pattern match if needed *)
           if NestedPat.isSimple pat
              then simpleBinding (pat, xmlExp)
              else compilePatternBinding (pat, xmlExp, matchDiags)
        end)
  in
     (* Combine into let expression *)
     Xml.Exp.lett {decs = recursiveDecs @ valueDecs, body = ...}
  end
```

### Expression Translation

**Core translations** ([defunctorize.fun](defunctorize.fun)):

| CoreML Expression | XML Expression |
|-------------------|----------------|
| `App {func, arg}` | `Xml.Exp.app {func, arg, ty}` |
| `Case {...}` | Match compilation (see below) |
| `Con (con, targs)` | `Xml.Exp.conApp {con, targs, arg}` |
| `Const c` | `Xml.Exp.const c` |
| `Handle {try, catch, handler}` | `Xml.Exp.handlee {try, catch, handler}` |
| `Lambda {arg, body}` | `Xml.Exp.lambda {arg, argType, body}` |
| `Let (decs, body)` | `Xml.Exp.lett {decs, body}` |
| `PrimApp {prim, args}` | `Xml.Exp.primApp {prim, targs, args}` |
| `Raise exn` | `Xml.Exp.raisee {exn, extend=true}` |
| `Record r` | `Xml.Exp.record r` |
| `Var (x, targs)` | `Xml.Exp.var {var=x, targs}` |

**Special case: ref**:
```sml
(* In CoreML, ref is a constructor *)
Con (Con.reff, targs)

(* In XML, ref is a primitive *)
Xml.Exp.primApp {prim = Prim.Ref_ref,
                 targs = targs,
                 args = [...]}
```

### Pattern Translation

**Pattern representation**:

CoreML uses `CoreML.Pat` (rich pattern syntax)
↓
NestedPat intermediate representation
↓
XML uses `Xml.Pat` (simple patterns: Var, Con, Wild)

**NestedPat** ([defunctorize.fun](defunctorize.fun)):
```sml
datatype NestedPat.node =
   Con of {con: Con, targs: Type vector, arg: NestedPat option}
 | Const of Const
 | Layered of Var * NestedPat
 | List of NestedPat vector
 | Or of NestedPat vector
 | Record of (Field * NestedPat) vector
 | Var of Var
 | Vector of NestedPat vector
 | Wild
```

**Simple patterns**: Translated directly to XML
- `Wild` → `Xml.Pat.Wild`
- `Var x` → `Xml.Pat.Var x`
- `Con {con, arg=NONE}` → `Xml.Pat.Con {con, arg=NONE}`

**Complex patterns**: Compiled using match compilation
- Nested records: Generate select operations
- Layered patterns: Generate intermediate bindings
- Or patterns: Replicate case branches
- List/Vector patterns: Desugar to constructor patterns

### Match Compilation

**Simple case** (no pattern compilation needed):
```sml
case x of
  SOME y => y
| NONE => default

(* Direct translation *)
Case {
  test = x,
  cases = [
    (Pat.Con {con=SOME, arg=SOME (Pat.Var y)}, y),
    (Pat.Con {con=NONE, arg=NONE}, default)
  ]
}
```

**Complex case** (requires compilation):
```sml
case x of
  (a, b::bs) => a + length bs

(* Compiled to decision tree *)
let val t = x
    val a = #1 t
    val rest = #2 t
in
   case rest of
     b::bs => a + length bs
end
```

**Match compilation driver** ([defunctorize.fun](defunctorize.fun)):
```sml
fun casee {caseType, cases, conTycon, matchDiags, noMatch,
           region, test, tyconCons} =
   let
      (* Add default case if non-exhaustive *)
      val cases =
         case noMatch of
            Impossible => cases
          | RaiseMatch => cases @ [default (fn _ => Xml.Exp.match)]
          | RaiseBind => cases @ [default (fn _ => Xml.Exp.bind)]
          | RaiseAgain => cases @ [default (fn e => Xml.Exp.monoVar e)]

      (* Invoke match compiler *)
      val (body, nonexhaustiveExamples) =
         MatchCompile.matchCompile {
           caseType = caseType,
           cases = cases,
           test = testVar,
           testType = testType,
           ...
         }

      (* Report diagnostics *)
      val _ = reportNonexhaustive (nonexhaustiveExamples, matchDiags)
      val _ = reportRedundant (cases, matchDiags)
   in
      Xml.Exp.let1 {var = testVar,
                    exp = test,
                    body = body}
   end
```

### Polymorphic Value Bindings

**Value restriction**:

Only non-expansive expressions can be generalized:

```sml
(* Non-expansive: can create PolyVal *)
val id = fn x => x
(* PolyVal ['a] (id : 'a -> 'a) = ... *)

(* Expansive: must use thunk *)
val r = ref []
(* Wrap in thunk to preserve polymorphism *)
(* PolyVal ['a] (r : unit -> 'a list ref) = lambda () => ref [] *)
(* val r_inst = r () [int]  (* instantiate with unit arg *) *)
```

**Handling expansive polymorphic values** ([defunctorize.fun](defunctorize.fun)):
```sml
if isExpansive exp andalso not (Vector.isEmpty tyvars)
   then
      (* Wrap in thunk: val 'a x = e becomes val 'a x = fn () => e *)
      let
         val thunk = lambda {arg = unit, body = exp, ...}
         val thunkTy = unit -> expType
      in
         PolyVal {var = x,
                  ty = thunkTy,
                  tyvars = tyvars,
                  exp = thunk}
      end
   else
      (* Simple polymorphic value *)
      PolyVal {var = x,
               ty = expType,
               tyvars = tyvars,
               exp = exp}
```

### Polymorphic Patterns

**Polymorphic pattern bindings**:
```sml
(* CoreML *)
val 'a SOME x = opt

(* XML: expand to individual bindings *)
val 'a tmp = opt
val SOME _ = tmp [unit]      (* instantiate for match checking *)
val 'a x = case tmp of SOME x' => x'
```

**Process** ([defunctorize.fun](defunctorize.fun)):
1. Bind entire pattern to temporary variable (polymorphic)
2. Instantiate pattern with unit types for exhaustiveness check
3. Extract each pattern variable as separate polymorphic binding

## Match Diagnostics

### Non-exhaustive Patterns

**Detection**: Match compiler analyzes pattern coverage and generates counterexamples.

**Example**:
```sml
fun f (SOME x) = x
(* Warning: pattern match non-exhaustive
 *   NONE => ...
 *)
```

**Handling** ([defunctorize.fun](defunctorize.fun)):
```sml
case nonexhaustiveDiag of
   Control.Elaborate.DiagEIW.Error =>
      Control.error (layoutCounterexample examples)
 | Control.Elaborate.DiagEIW.Warn =>
      Control.warning (layoutCounterexample examples)
 | Control.Elaborate.DiagEIW.Ignore => ()
```

### Redundant Patterns

**Detection**: Match compiler tracks which patterns are reachable.

**Example**:
```sml
case x of
  NONE => 0
| SOME y => y
| NONE => 1        (* redundant! *)
```

**Handling** ([defunctorize.fun](defunctorize.fun)):
```sml
(* Track pattern usage *)
val numPats = ref 0     (* how many patterns generated *)
val numUses = ref 0     (* how many times pattern was used *)

(* After match compilation *)
if !numPats > 0 andalso !numUses = 0
   then reportRedundant (pat, region)
   else ()
```

### Exception Pattern Diagnostics

**Special handling for exception patterns**:

```sml
(* Exception handlers are not required to be exhaustive *)
e handle
  Div => 0
(* No warning about other exceptions *)
```

**Control** ([defunctorize.fun](defunctorize.fun)):
```sml
case nonexhaustiveExnDiag of
   Control.Elaborate.DiagDI.Default =>
      (* Report non-exhaustiveness for exception patterns *)
      {dropOnlyExns = false}
 | Control.Elaborate.DiagDI.Ignore =>
      (* Don't report for exception patterns *)
      {dropOnlyExns = true}
```

## Profiling Support

### Enter/Leave Wrapping

When profiling is enabled, defunctorization adds `EnterLeave` wrappers:

**Source information** ([defunctorize.fun](defunctorize.fun)):
```sml
fun enterLeave (exp, ty, sourceInfo) =
   Xml.Exp.enterLeave (exp, ty, sourceInfo)
```

**Applied to**:
- Function bodies
- Case branches (when `profileRaise` is enabled)
- Non-exhaustive match defaults

**Example**:
```sml
(* Function with profiling *)
fun f x = x + 1

(* Defunctorized with profiling enabled *)
Fun {
  lambda = {
    body = EnterLeave (
             x + 1,
             SourceInfo.function {name = ["f"], region = ...})
  }
}
```

## Type Variable Scoping

### Free Type Variables

Defunctorization must track type variable scoping carefully:

**Rule**: Type variables are scoped by:
- Datatype declarations
- Function declarations (`Fun`)
- Polymorphic value declarations (`PolyVal`)

**Example**:
```sml
(* Type variables scoped properly *)
fun 'a map (f: 'a -> 'b) (xs: 'a list): 'b list = ...
(*   ^^                                              *)
(*   'a and 'b scoped to this function              *)
```

**Type substitution** ([defunctorize.fun](defunctorize.fun)):
```sml
(* Replace free type variables in polymorphic pattern *)
fun subst (ty, tyvars) =
   Type.substitute (ty, Vector.map (tyvars, fn a =>
                                    (a, Type.unit)))

(* Instantiate polymorphic value with unit *)
val targs = Vector.map (tyvars, fn _ => Type.unit)
```

## Development Guide

### Understanding Defunctorization

To understand defunctorization:

1. **Read signature** ([defunctorize.sig](defunctorize.sig)) - Simple interface
2. **Examine main functor** ([defunctorize.fun](defunctorize.fun))
3. **Follow declaration processing** - See how each CoreML Dec becomes XML
4. **Study pattern compilation** - Understand NestedPat translation
5. **Trace type translation** - See how types become explicit

**Key insight**: Defunctorization is mostly a straightforward translation, except for:
- Pattern compilation (delegated to MatchCompile)
- Polymorphic pattern bindings (expanded to multiple bindings)
- Expansive polymorphic values (wrapped in thunks)

### Modifying Defunctorization

**When adding CoreML features**:

1. **Extend CoreML IR**: Add new declaration or expression form
2. **Add translation case**: Handle new form in defunctorization
3. **Decide XML representation**: Map to existing XML or extend XML
4. **Update pattern compiler**: If new patterns are added
5. **Test**: Verify correct translation with regression tests

**When changing pattern compilation**:

- Modifications usually go in [match-compile](../match-compile/)
- Defunctorization just invokes `MatchCompile.matchCompile`
- Change how patterns are translated to `NestedPat` if needed

### Adding Custom Transformations

To add a transformation before defunctorization:

1. **CoreML → CoreML pass**: Add to CoreML pipeline (rare)
2. **Test**: Ensure CoreML invariants are preserved
3. **Defunctorize**: Existing defunctorization should handle transformed code

To add a transformation after defunctorization:

1. **XML → XML pass**: More common (see [xml](../xml/))
2. **Use polymorphic representation**: Work with PolyVal, type variables
3. **Preserve XML invariants**: Explicit types, first-order

## Common Patterns

### Functor Instantiation

**Pattern**: Instantiate functor by substituting actual argument for formal parameter.

**Example**:
```sml
functor F (X: sig val n: int end) = struct val m = X.n + 1 end
structure A = struct val n = 42 end
structure B = F(A)
```

**Translation**:
```sml
(* Inline functor body with A substituted for X *)
val n = 42
val m = n + 1
```

### Signature Matching

**Pattern**: Signature ascription restricts visibility.

**Transparent ascription** (`:`):
```sml
structure A : sig val x: int end = struct
  val x = 1
  val y = 2  (* not in signature *)
end
(* XML: both bindings present, but y is not exported *)
val A_x = 1
val A_y = 2
```

**Opaque ascription** (`:>`):
```sml
structure A :> sig type t val x: t end = struct
  type t = int
  val x = 42
end
(* XML: type t becomes abstract via polymorphism *)
val 'a A_x = 42 : 'a
```

### Nested Structures

**Pattern**: Flatten nested structure paths to flat variable names.

**Example**:
```sml
structure A = struct
  structure B = struct
    structure C = struct
      val x = 1
    end
  end
end
val y = A.B.C.x
```

**Translation**:
```sml
val A_B_C_x = 1
val y = A_B_C_x
```

## Debugging Defunctorization

### Viewing Defunctorization Output

**Generate XML output**:
```bash
mpl -keep-xml program.mlb
cat program.mlb.xml
```

**With detailed types**:
```bash
mpl -show-types true -keep-xml program.mlb
```

### Common Issues

**Issue: Non-exhaustive pattern match**

**Symptom**: Warning or error during defunctorization

**Cause**: Pattern match doesn't cover all cases

**Fix**:
- Add missing patterns to source code
- Check `matchDiags` settings in source
- Examine counterexamples in warning message

**Issue: Type variable escapes scope**

**Symptom**: Type error in generated XML

**Cause**: Type variable used outside its binding scope

**Fix**:
- Check type variable scoping in CoreML
- Verify `tyvars` functions return correct variables
- Ensure type substitution is applied correctly

**Issue: Module path not found**

**Symptom**: Undefined variable in generated XML

**Cause**: Structure or functor not properly flattened

**Fix**:
- Check structure bindings in CoreML
- Verify functor instantiation
- Trace variable renaming through defunctorization

**Issue: Polymorphic recursion not supported**

**Symptom**: Type error or incorrect behavior

**Cause**: Function calls itself at different type

**Example**:
```sml
fun 'a f (x: 'a) =
   ... f [different-type] ...
```

**Fix**: MLton doesn't support polymorphic recursion; restructure code to avoid it.

## Performance Considerations

**Pattern compilation overhead**:
- Complex patterns generate large decision trees
- Match compiler optimizes for common cases
- Deeply nested patterns may increase code size

**Functor inlining**:
- Each functor application creates a copy
- Large functors can significantly increase code size
- Trade-off: modularity vs. code duplication

**Module flattening**:
- Very fast transformation (simple renaming)
- No runtime overhead
- Enables later optimizations by removing abstraction barriers

## Examples

### Example: Simple Functor

**CoreML**:
```sml
functor Inc (X: sig val n: int end) = struct
  val m = X.n + 1
end
structure A = struct val n = 42 end
structure B = Inc(A)
val result = B.m
```

**XML**:
```sml
val A_n = 42
val B_m = A_n + 1
val result = B_m
```

### Example: Polymorphic Pattern

**CoreML**:
```sml
val 'a SOME x = opt
```

**XML**:
```sml
val 'a tmp = opt
val () = (case tmp [unit] of SOME _ => () | NONE => raise Match)
val 'a x = case tmp of SOME x' => x' | NONE => raise Match
```

### Example: Complex Pattern Match

**CoreML**:
```sml
fun sum pairs =
   case pairs of
     (x, y) :: rest => x + y + sum rest
   | [] => 0
```

**XML**:
```sml
Fun (sum : (int * int) list -> int) =
  lambda (pairs : (int * int) list) =>
    case pairs of
      Cons arg =>
        let val x = #1 arg
            val y = #2 arg
            val rest = #rest arg
        in x + y + sum rest end
    | Nil => 0
```

### Example: Exception Handler

**CoreML**:
```sml
fun safediv (x, y) =
   (x div y) handle Div => 0
                  | Overflow => 0
```

**XML**:
```sml
Fun (safediv : int * int -> int) =
  lambda (arg : int * int) =>
    let val x = #1 arg
        val y = #2 arg
    in
       Handle {
         try = x div y,
         catch = (exn, exn),
         handler =
           case exn of
             Div => 0
           | Overflow => 0
           | _ => raise exn    (* re-raise other exceptions *)
       }
    end
```

## See Also

- [CoreML IR](../core-ml/) - Input to defunctorization
- [XML IR](../xml/) - Output of defunctorization
- [Match Compile](../match-compile/) - Pattern compilation algorithm
- [Elaborate](../elaborate/) - Type inference and module elaboration (produces CoreML)
- [Monomorphise](../xml/) - Type specialization (consumes XML)

## References

- **Definition of Standard ML (Revised)**: Module system semantics
- **MLton Source Code**: Defunctorization implementation
- **Match Compilation**: Decision tree algorithm for pattern matching
- **Module Systems**: Functor instantiation and signature matching in ML
