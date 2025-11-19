# MLton XML IR

XML (eXtensible ML) is a polymorphic, typed, first-order intermediate representation produced by defunctorization and consumed by monomorphisation.

## Overview

XML sits between CoreML and SSA in the compilation pipeline. It is produced by [defunctorization](../defunctorize/), which eliminates the module system from CoreML, and is consumed by monomorphisation, which produces SXML (Simple XML) — a monomorphic variant suitable for closure conversion.

**Key characteristics**:
- **Polymorphic**: Retains type variables and type abstraction
- **Typed**: Every expression has an explicit type
- **First-order**: Functions are first-class values (not yet converted to closures)
- **Module-free**: No structures, signatures, or functors
- **Explicit type application**: Variables applied to type arguments (`VarExp`)

After XML optimization passes, the program is monomorphised (specialized for each type instantiation) to produce SXML, which then undergoes closure conversion to [SSA](../ssa/).

## Key Concepts

### XML vs SXML

**XML (Polymorphic)**:
- Type variables allowed: `'a`, `'b`, etc.
- PolyVal declarations for polymorphic values
- Type arguments at variable uses
- Exception declarations present

**SXML (Simple XML, Monomorphic)**:
- No type variables (all types concrete)
- No PolyVal declarations (only MonoVal)
- Empty type argument lists
- No exception declarations (implemented as datatypes)

**Example transformation**:
```sml
(* XML *)
PolyVal ['a] (id : 'a -> 'a) = fn x => x

(* SXML after monomorphisation *)
MonoVal (id_int : int -> int) = fn x => x
MonoVal (id_string : string -> string) = fn x => x
```

###

 XML IR Structure

**Program** ([xml-tree.sig](xml-tree.sig)):
```sml
Program = {
  datatypes: Datatype vector,
  body: Exp
}
```

**Declarations** ([xml-tree.sig](xml-tree.sig)):
```sml
datatype Dec =
   Exception of {con: Con, arg: Type option}
 | Fun of {tyvars: Tyvar vector,
           decs: {var: Var, ty: Type, lambda: Lambda} vector}
 | MonoVal of {var: Var, ty: Type, exp: PrimExp}
 | PolyVal of {var: Var, ty: Type, tyvars: Tyvar vector, exp: Exp}
```

- **Exception**: Exception constructor declarations
- **Fun**: (Mutually) recursive function declarations with type variables
- **MonoVal**: Monomorphic value binding
- **PolyVal**: Polymorphic value binding (eliminated by monomorphisation)

**Expressions** ([xml-tree.sig](xml-tree.sig)):
```sml
Exp = {decs: Dec list, result: VarExp}
```

Expressions are a sequence of declarations followed by a result variable.

**Primitive Expressions** ([xml-tree.sig](xml-tree.sig)):
```sml
datatype PrimExp =
   App of {func: VarExp, arg: VarExp}
 | Case of {test: VarExp, cases: (Pat, Exp) vector, default: Exp option}
 | ConApp of {con: Con, targs: Type vector, arg: VarExp option}
 | Const of Const
 | Handle of {try: Exp, catch: Var * Type, handler: Exp}
 | Lambda of Lambda
 | PrimApp of {prim: Prim, targs: Type vector, args: VarExp vector}
 | Profile of ProfileExp
 | Raise of {exn: VarExp, extend: bool}
 | Select of {tuple: VarExp, offset: int}
 | Tuple of VarExp vector
 | Var of VarExp
```

**VarExp** (Type Application):
```sml
datatype VarExp = VarExp of {var: Var, targs: Type vector}
```

Variables applied to type arguments: `f [int, bool]`

### Type Passing

XML uses **explicit type passing**:

**Polymorphic function**:
```sml
fun 'a id (x: 'a) = x
```

**XML representation**:
```sml
Fun ['a] (id : 'a -> 'a) = lambda (x : 'a) => x
```

**Uses instantiate with type arguments**:
```sml
id [int] 42
id [string] "hello"
```

Each use specifies concrete types, enabling later monomorphisation.

## File Organization

### Core XML Files

| File | Lines | Purpose |
|------|-------|---------|
| [xml.sig](xml.sig) | ~20 | Main XML interface |
| [xml.fun](xml.fun) | ~10 | XML functor composition |
| [xml-tree.sig](xml-tree.sig) | ~250 | XML IR structure |
| [xml-tree.fun](xml-tree.fun) | ~1,400 | XML IR implementation |
| [xml-type.sig](xml-type.sig) | ~15 | XML type interface |

### Transformation Passes

| File | Lines | Purpose |
|------|-------|---------|
| [monomorphise.sig](monomorphise.sig) | ~20 | Monomorphisation signature |
| [monomorphise.fun](monomorphise.fun) | ~550 | Type specialization (XML → SXML) |
| [implement-exceptions.fun](implement-exceptions.fun) | ~800 | Implement exceptions as datatypes |
| [implement-suffix.fun](implement-suffix.fun) | ~160 | Implement program exit suffixes |
| [polyvariance.fun](polyvariance.fun) | ~750 | Polyvariant value analysis |
| [uncurry.fun](uncurry.fun) | ~800 | Uncurrying optimization |

### Optimization Passes

| File | Lines | Purpose |
|------|-------|---------|
| [shrink.fun](shrink.fun) | ~850 | Shrinking (dead code, simplification) |
| [xml-simplify.fun](xml-simplify.fun) | ~80 | XML simplification driver |
| [simplify-types.fun](simplify-types.fun) | ~400 | Type simplification |
| [scc-funs.fun](scc-funs.fun) | ~190 | Strongly-connected components for functions |

### SXML Files

| File | Lines | Purpose |
|------|-------|---------|
| [sxml.sig](sxml.sig) | ~25 | SXML signature (monomorphic XML) |
| [sxml.fun](sxml.fun) | ~25 | SXML functor |
| [sxml-tree.sig](sxml-tree.sig) | ~20 | SXML tree structure |
| [sxml-simplify.sig](sxml-simplify.sig) | ~20 | SXML simplification signature |
| [sxml-simplify.fun](sxml-simplify.fun) | ~200 | SXML simplification |
| [sxml-exns.sig](sxml-exns.sig) | ~20 | SXML exception interface |

### Supporting Files

| File | Lines | Purpose |
|------|-------|---------|
| [type-check.fun](type-check.fun) | ~460 | XML type checker |
| [call-count.fun](call-count.fun) | ~180 | Call counting for inlining |
| [cps-transform.fun](cps-transform.fun) | ~630 | CPS transformation (experimental) |

### Build System

- [sources.mlb](sources.mlb) - ML Basis file for building XML
- [sources.cm](sources.cm) - CM file (legacy)

## Monomorphisation

### Overview

Monomorphisation eliminates polymorphism by creating specialized copies of polymorphic functions for each type at which they're used.

**Process** ([monomorphise.fun](monomorphise.fun)):
1. **Gather instantiations**: Find all type applications of polymorphic values
2. **Specialize**: Create monomorphic copy for each instantiation
3. **Substitute**: Replace polymorphic uses with specialized versions
4. **Eliminate type variables**: Remove all type variables and PolyVal decs

**Example**:
```sml
(* Input: XML *)
PolyVal ['a] (map : ('a -> 'b) -> 'a list -> 'b list) = ...
val xs = map [int, bool] isEven numbers
val ys = map [string, int] String.size strings

(* Output: SXML *)
MonoVal (map_int_bool : (int -> bool) -> int list -> bool list) = ...
MonoVal (map_string_int : (string -> int) -> string list -> int list) = ...
val xs = map_int_bool isEven numbers
val ys = map_string_int String.size strings
```

### Caching

Monomorphisation caches specialized versions to avoid duplication:

**Cache structure** ([monomorphise.fun](monomorphise.fun)):
```sml
HashTable (Type vector -> instantiation)
```

For each polymorphic value, map type argument vectors to specialized variables.

**Example**:
```sml
map [int, bool] → map_int_bool
map [int, bool] → map_int_bool (reuse)
map [string, int] → map_string_int (new)
```

## Exception Implementation

### Transformation

Exceptions are implemented as a special datatype ([implement-exceptions.fun](implement-exceptions.fun)):

**Before**:
```sml
exception Overflow
exception Fail of string
raise Overflow
```

**After**:
```sml
datatype exn_sum = Overflow | Fail of string | ...
val Overflow = fn () => inject_exn Overflow
val Fail = fn s => inject_exn (Fail s)
raise (Overflow ())
```

### Key Components

**Top-level handler** ([implement-exceptions.fun](implement-exceptions.fun)):
- Global ref cell holding `exn -> unit` function
- Called on unhandled exceptions
- Set by runtime initialization

**Exception extra field**:
- Supports exception history/metadata
- Type determined by `Exn_extra` primitive uses
- Default value created for initial exceptions

**Exception names**:
- String representation of each exception constructor
- Used for printing and debugging

## Suffix Implementation

Program exit suffixes are implemented as a cleanup mechanism ([implement-suffix.fun](implement-suffix.fun)):

**Top-level suffix**:
- Global ref cell holding `unit -> unit` function
- Called on program exit
- Allows registration of cleanup handlers

**Transformation**:
Wraps main program body to call suffix on exit (normal or exceptional).

## XML Optimization Passes

### Shrinking

**Shrink** ([shrink.fun](shrink.fun)) performs:
- **Dead code elimination**: Remove unused bindings
- **Constant folding**: Evaluate constant expressions
- **Inlining**: Inline small values and functions
- **Tuple flattening**: Flatten nested tuples
- **Unused argument elimination**: Remove unused function parameters

**Example**:
```sml
(* Before *)
val x = 42
val y = x + 0
val z = y * 1

(* After *)
val z = 42
```

### Uncurrying

**Uncurry** ([uncurry.fun](uncurry.fun)) transforms curried functions to direct multi-argument form:

**Before**:
```sml
fun f x = fn y => fn z => x + y + z
val result = f 1 2 3
```

**After**:
```sml
fun f (x, y, z) = x + y + z
val result = f (1, 2, 3)
```

**Benefits**:
- Fewer intermediate closures
- More efficient calling convention
- Enables better optimization in later passes

### Type Simplification

**SimplifyTypes** ([simplify-types.fun](simplify-types.fun)):
- Remove unused type variables
- Simplify type applications
- Canonicalize type representations

### Polyvariance Analysis

**Polyvariance** ([polyvariance.fun](polyvariance.fun)):
- Analyze which values should be inlined at each call site
- Support for flow-sensitive specialization
- Trade code size for performance

## XML Simplification Pipeline

**XML.simplify** ([xml-simplify.fun](xml-simplify.fun)):

```
Input: XML Program
  ↓
Shrink (eliminate dead code, inline)
  ↓
Uncurry (multi-argument functions)
  ↓
Polyvariance (flow-sensitive inlining)
  ↓
Shrink (cleanup after transformations)
  ↓
SimplifyTypes (canonicalize types)
  ↓
Output: Optimized XML Program
```

## Type Checking

**TypeCheck** ([type-check.fun](type-check.fun)) verifies XML programs:

**Checks**:
- All variable references are bound
- Types match at application sites
- Pattern matching is well-typed
- Type arguments have correct arity
- Exception handling is well-typed

**Invocation**:
```sml
Xml.typeCheck program
```

Called after each transformation to ensure correctness.

## Development Guide

### Adding XML Transformations

To add a new XML → XML transformation:

1. **Create transformation file**: `my-transform.fun`
   ```sml
   functor MyTransform (S: XML_TRANSFORM_STRUCTS): XML_TRANSFORM =
   struct
      open S
      fun transform (Program.T {datatypes, body}) =
         let
            (* Transform body *)
            val body' = transformExp body
         in
            Program.T {datatypes = datatypes, body = body'}
         end
   end
   ```

2. **Add to simplification pipeline**: Edit [xml-simplify.fun](xml-simplify.fun)
   ```sml
   val program = MyTransform.transform program
   ```

3. **Type check**: Add type checking after transformation
   ```sml
   val () = typeCheck program
   ```

4. **Test**: Run regression tests
   ```bash
   ./bin/regression
   ```

### Modifying Monomorphisation

To change monomorphisation behavior:

1. **Edit [monomorphise.fun](monomorphise.fun)**
   - Modify cache strategy
   - Change specialization criteria
   - Add/remove monomorphic instances

2. **Consider code size**: Over-specialization increases code size

3. **Test polymorphic code**: Ensure all instantiations are found

4. **Verify SXML invariants**:
   - No type variables in output
   - No PolyVal declarations
   - All type argument lists empty

### Debugging XML

**View XML IR**:
```bash
mpl -keep-xml program.mlb
cat program.mlb.xml
```

**Type check explicitly**:
```bash
mpl -type-check-xml true program.mlb
```

**Common issues**:
- **Unbound type variable**: Monomorphisation missed an instantiation
- **Type arity mismatch**: Type arguments don't match type parameters
- **Missing specialization**: Polymorphic function not specialized for some type

## Performance Considerations

**Monomorphisation code explosion**:
- Creates copy for each instantiation
- Can significantly increase code size
- Trade-off: performance vs. code size

**Shrinking effectiveness**:
- Multiple shrink passes catch more opportunities
- Interleave with other transformations
- Diminishing returns after 2-3 iterations

**Uncurrying benefits**:
- Reduces closure allocations (important for functional code)
- Better inlining opportunities
- Most beneficial for frequently-called curried functions

**Type passing overhead**:
- XML carries explicit type information
- SXML eliminates this overhead via monomorphisation
- Critical for performance of polymorphic code

## Examples

### Example: Monomorphisation

**Input XML**:
```sml
PolyVal ['a] (id : 'a -> 'a) =
   lambda (x : 'a) => x

MonoVal (n : int) = id [int] 42
MonoVal (s : string) = id [string] "hello"
```

**Output SXML**:
```sml
MonoVal (id_int : int -> int) =
   lambda (x : int) => x

MonoVal (id_string : string -> string) =
   lambda (x : string) => x

MonoVal (n : int) = id_int 42
MonoVal (s : string) = id_string "hello"
```

### Example: Exception Implementation

**Input XML**:
```sml
Exception (Overflow : exn)
Exception (Fail : string -> exn)

MonoVal (f : unit -> int) =
   lambda (_ : unit) =>
      Handle {
         try = 100 div 0,
         catch = (e, exn),
         handler = -1
      }
```

**After implementation**:
```sml
Datatype exn_sum = Overflow | Fail of string | ...

MonoVal (Overflow : unit -> exn) =
   lambda (_ : unit) => inject_exn (sum_Overflow)

MonoVal (f : unit -> int) =
   lambda (_ : unit) =>
      Handle {
         try = 100 div 0,
         catch = (e, exn),
         handler = -1
      }
```

### Example: Uncurrying

**Input**:
```sml
Fun (add : int -> int -> int) =
   lambda (x : int) =>
      lambda (y : int) => x + y

MonoVal (result : int) =
   App (App (add, 10), 32)
```

**After uncurrying**:
```sml
Fun (add : (int * int) -> int) =
   lambda ((x, y) : int * int) => x + y

MonoVal (result : int) =
   App (add, (10, 32))
```

## Common Issues

### Issue: Code Size Explosion

**Symptom**: Compiled program much larger after monomorphisation

**Cause**: Excessive polymorphic instantiations

**Fix**:
- Use monomorphic code where possible
- Limit generic programming in hot paths
- Consider manual specialization

### Issue: Missing Type Instantiation

**Symptom**: "Unbound variable" error in SXML

**Cause**: Monomorphisation didn't find all uses

**Fix**:
- Check type application is explicit
- Verify all polymorphic uses have type arguments
- Debug monomorphisation cache

### Issue: Type Variable Remains After Monomorphisation

**Symptom**: SXML has type variables (violates invariant)

**Cause**: Incomplete monomorphisation

**Fix**:
- Check all PolyVal decs are eliminated
- Verify type substitution is complete
- Run type checker on SXML

## See Also

- [CoreML IR](../core-ml/) - Input to defunctorization
- [Defunctorize](../defunctorize/) - CoreML → XML transformation
- [Closure Conversion](../closure-convert/) - SXML → SSA transformation
- [SSA IR](../ssa/) - Next stage after closure conversion
- [Type System](../elaborate/) - Type inference that produces typed CoreML

## References

- **MLton Source Code**: XML IR implementation
- **Monomorphisation**: Type specialization technique for polymorphic languages
- **Type Passing**: Explicit representation of types at runtime (vs. type erasure)
