# MLton Closure Conversion

Closure conversion transforms first-order SXML (monomorphic XML) to SSA form, converting lambda expressions to explicit closure data structures and top-level functions.

## Overview

Closure conversion sits between SXML and SSA in the compilation pipeline. It consumes [SXML](../xml/) (monomorphic, first-order) and produces [SSA](../ssa/) (explicit closures, continuation-passing style).

**Key transformations**:
- **Lambda lifting**: Convert nested lambdas to top-level functions
- **Closure creation**: Package free variables into environment records
- **Function calls**: Pass environment explicitly to converted functions
- **Globalization**: Identify variables that don't need closure capture
- **Value analysis**: Track abstract values through the program

After closure conversion, all functions are top-level, all free variables are explicitly captured in closures, and the program is in SSA form ready for optimization.

## Key Concepts

### Closure Representation

**Lambda in SXML**:
```sml
let val x = 10
    val f = fn y => x + y
in f 32 end
```

**After closure conversion**:
```sml
(* Top-level function with environment parameter *)
fun f_code (env, y) =
   let val x = #x env
   in x + y end

(* Closure creation *)
let val x = 10
    val f_env = {x = x}
    val f = (f_code, f_env)
in
   (* Closure application *)
   let val (code, env) = f
   in code (env, 32) end
end
```

**Closure tuple**:
```sml
closure = (code_pointer, environment_record)
```

- `code_pointer`: Function to call
- `environment_record`: Captured free variables

### Globalization

**Globalization** determines which variables are "global" (don't need closure capture):

**Global variables** ([globalize.fun](globalize.fun)):
- Top-level declarations
- Variables not free in any lambda
- Variables that would create circular closures

**Local variables**:
- Variables free in lambdas
- Must be captured in closure environment

**Example**:
```sml
val a = 10           (* global: top-level *)
fun f x =
   let val b = 20    (* local: free in g *)
       val g = fn y => x + y + b
   in g end

(* Globalization analysis *)
(* a: global (top-level) *)
(* x: parameter of f (treated specially) *)
(* b: local (free in lambda g) *)
(* g: depends on analysis (could be global or local) *)
```

**Process** ([globalize.fun](globalize.fun)):
```sml
val globalize: {
   program: Program.t,
   lambdaFree: Lambda.t -> Var.t vector,
   varGlobal: Var.t -> bool ref
} -> unit
```

Sets `varGlobal` property for each variable based on:
1. **Top-level binding**: Automatically global
2. **Free variable analysis**: If variable is never free in a lambda
3. **Circularity**: If making it local would create circular dependencies

### Free Variable Analysis

**LambdaFree** ([lambda-free.sig](lambda-free.sig), [lambda-free.fun](lambda-free.fun)) computes free variables for each lambda:

**For simple lambdas**:
```sml
val f = fn x => x + y + z
(* Free variables: {y, z} *)
```

**For mutually recursive functions**:
```sml
val rec f = fn x => ... y ... g ... f ...
and g = fn z => ... f ... w ...

(* lambdaFree(fn x =>) = {y, w}  (union of all frees in group) *)
(* lambdaFree(fn z =>) = {y, w}  (same union) *)
(* lambdaRec(fn x =>) = {g, f}    (functions in group) *)
(* lambdaRec(fn z =>) = {f}       (which functions this one calls) *)
```

**Key insight**: For mutually recursive functions, all functions in the group share the same closure environment containing the union of free variables.

**Interface** ([lambda-free.sig](lambda-free.sig)):
```sml
val lambdaFree: {
   program: Program.t,
   varInfo: Var.t -> {frees: Var.t list ref ref,
                      status: Status.t ref},
   lambdaInfo: Lambda.t -> {frees: Var.t vector ref,
                           recs: Var.t vector ref}
} -> unit
```

### Abstract Value Analysis

**Value** ([abstract-value.sig](abstract-value.sig), [abstract-value.fun](abstract-value.fun)) tracks abstract values through the program:

**Abstract value types**:
```sml
datatype dest =
   Array of t
 | Lambdas of Lambdas.t       (* Set of possible lambda values *)
 | Ref of t
 | Tuple of t vector
 | Type of Type.t              (* First-order type, no lambdas *)
 | Vector of t
 | Weak of t
```

**Purpose**:
- Track which lambdas might flow to which variables
- Determine closure types for SSA
- Enable optimizations (known function calls)

**Example**:
```sml
val f = fn x => x + 1
val g = fn x => x * 2
val h = if b then f else g

(* Abstract value of h: Lambdas {f, g} *)
```

**Lambdas set** ([abstract-value.sig](abstract-value.sig)):
```sml
structure Lambdas:
   sig
      type t
      val equals: t * t -> bool
      val plist: t -> PropertyList.t
      val toList: t -> Lambda.t list
   end
```

Represents a set of lambdas that may flow to a variable.

### Closure Environment Types

**Environment records** contain captured free variables:

**Simple closure**:
```sml
(* Source lambda *)
fn y => x + z

(* Environment type *)
{x: int, z: int}

(* Closure type in SSA *)
(code: int * int -> int) * {x: int, z: int}
```

**Recursive function closure**:
```sml
(* Mutually recursive functions *)
val rec f = fn x => ... y ... g ...
and g = fn z => ... f ... w ...

(* Shared environment *)
{y: int, w: int, f: closure, g: closure}
(*                 ^^^^^^^^  ^^^^^^^^ recursive references *)
```

**Circular closures**: Handled by allocating environment record first, then filling in recursive references.

### SSA Type Generation

**valueType** converts SXML types to SSA types:

**First-order types**: Direct translation
```sml
int          → int
int * bool   → int * bool
int list     → int list
```

**Function types**: Converted to closures
```sml
int -> bool  → (code: int -> bool) * env
'a -> 'a     → ERROR (SXML is monomorphic, no type variables)
```

**Closure types**:
```sml
(* If free variables {x: int, y: bool} *)
int -> int   → (code: {x: int, y: bool} * int -> int)
               * {x: int, y: bool}
```

**Abstract value to SSA type**:
```sml
val ssaType: Value.t -> Ssa.Type.t option ref
```

Tracks the SSA type computed for each abstract value.

## File Organization

### Core Files

| File | Lines | Purpose |
|------|-------|---------|
| [closure-convert.sig](closure-convert.sig) | ~20 | Main signature |
| [closure-convert.fun](closure-convert.fun) | ~2,000 | Main conversion algorithm |
| [lambda-free.sig](lambda-free.sig) | ~50 | Free variable analysis signature |
| [lambda-free.fun](lambda-free.fun) | ~200 | Free variable analysis |
| [globalize.sig](globalize.sig) | ~25 | Globalization signature |
| [globalize.fun](globalize.fun) | ~200 | Globalization analysis |
| [abstract-value.sig](abstract-value.sig) | ~75 | Abstract value signature |
| [abstract-value.fun](abstract-value.fun) | ~500 | Abstract value implementation |

### Build System

- [sources.mlb](sources.mlb) - ML Basis file
- [sources.cm](sources.cm) - CM file (legacy)

## Algorithm

### Overview

Closure conversion is a multi-phase algorithm:

**Main function** ([closure-convert.sig](closure-convert.sig)):
```sml
val closureConvert: Sxml.Program.t -> Ssa.Program.t
```

**Phases**:

1. **Free variable analysis** (LambdaFree)
   - Compute free variables for each lambda
   - Compute recursive function groups

2. **Globalization** (Globalize)
   - Determine which variables are global
   - Avoid unnecessary closure captures

3. **Abstract value analysis** (Value)
   - Track abstract values through program
   - Build Lambdas sets for function-typed values

4. **Closure conversion** (main pass)
   - Convert lambdas to top-level functions
   - Create closure records
   - Transform function applications

5. **SSA generation**
   - Linearize to basic blocks
   - Generate SSA form

### Closure Conversion Pass

**Main loop structure** ([closure-convert.fun](closure-convert.fun)):

```sml
fun convertDec (dec: Sxml.Dec.t): Accum.t =
   case dec of
      Sxml.Dec.MonoVal {var, ty, exp} =>
         convertMonoVal (var, ty, exp)
    | Sxml.Dec.Fun {decs, tyvars} =>
         convertFun (decs, tyvars)
    | Sxml.Dec.Exception {...} =>
         convertException (...)
```

**Converting MonoVal**:
```sml
fun convertMonoVal (var, ty, exp) =
   let
      val value = getValue var
      val ssaTy = valueType value
      val (ssaExp, accum) = convertPrimExp exp
   in
      if isGlobal var
         then Accum.addGlobal (accum, {var = var,
                                       ty = ssaTy,
                                       exp = ssaExp})
         else (* local binding, handled by enclosing context *)
            accum
   end
```

**Converting Fun** (recursive functions):
```sml
fun convertFun (decs, tyvars) =
   let
      (* All functions in group share same environment *)
      val frees = union (map lambdaFree decs)
      val env = makeClosure frees

      (* Convert each lambda to top-level function *)
      val functions =
         map (decs, fn {var, lambda} =>
            convertLambdaToFunction (lambda, env))

      (* Create closure bindings *)
      val closures =
         map (decs, fn {var, lambda} =>
            {var = var,
             ty = closureType lambda,
             exp = makeClosure (function, env)})
   in
      Accum.addFuncs (functions, closures)
   end
```

**Converting lambda to function**:
```sml
fun convertLambdaToFunction (lambda, sharedEnv) =
   let
      val {arg, argType, body} = Lambda.dest lambda
      val frees = lambdaFree lambda

      (* Function takes environment + argument *)
      val envParam = Var.newNoname ()
      val envType = recordType frees

      (* Extract free variables from environment *)
      val bindings =
         map (frees, fn (x, i) =>
            {var = x,
             exp = Select {tuple = envParam, offset = i}})

      (* Convert body with free variables bound *)
      val body' = convertExp body

      (* Build SSA function *)
      val func =
         Function.new {
            args = [(envParam, envType), (arg, argType)],
            blocks = linearize body',
            name = Func.newNoname (),
            ...
         }
   in
      func
   end
```

### Variable Renaming

**Problem**: SXML variables can't be reused in SSA (single assignment).

**Solution**: Rename all local variables at each lambda binding.

**Implementation** ([closure-convert.fun](closure-convert.fun)):
```sml
(* Property list on each variable *)
val {get = getNewVar, set = setNewVar, ...} =
   Property.getSetOnce (Var.plist, Property.initRaise "newVar")

fun newScope (var, thunk) =
   let
      val old = getNewVar var
      val new = Var.new var
      val _ = setNewVar (var, new)
      val result = thunk ()
      val _ = setNewVar (var, old)  (* restore *)
   in
      result
   end
```

**Usage**:
```sml
(* When entering a lambda that binds x *)
newScope (x, fn () =>
   (* Inside lambda, getNewVar x returns fresh variable *)
   convertExp body)
(* After lambda, getNewVar x returns old binding *)
```

### Mutual Recursion

**Challenge**: Mutually recursive functions share an environment that includes themselves.

**Solution**:
1. Allocate closure environment record
2. Fill in non-recursive free variables
3. Fill in recursive function pointers (cyclic references)

**Example**:
```sml
val rec f = fn x => ... g ... y ...
and g = fn z => ... f ... w ...

(* Step 1: Allocate environment *)
val env = allocRecord {y: _, w: _, f: _, g: _}

(* Step 2: Fill non-recursive *)
val env = update (env, #y, y_value)
val env = update (env, #w, w_value)

(* Step 3: Create closures (circular) *)
val f_closure = (f_code, env)
val g_closure = (g_code, env)
val env = update (env, #f, f_closure)
val env = update (env, #g, g_closure)
```

## SSA Generation

### Linearization

**Direct expressions** (Dexp) are converted to SSA basic blocks:

**Process**:
1. **Build control flow**: Case expressions → branches
2. **Create basic blocks**: Each block has straight-line code + transfer
3. **Generate labels**: Each block gets a label
4. **Compute handler**: Track exception handlers

**Example**:
```sml
(* Direct expression *)
let val x = 1 + 2
    val y = if x > 0 then 10 else 20
in x + y end

(* SSA basic blocks *)
Block L1:
  x = PrimApp Int_add (1, 2)
  goto L2

Block L2:
  b = PrimApp Int_gt (x, 0)
  if b then L3 else L4

Block L3:
  y = 10
  goto L5

Block L4:
  y = 20
  goto L5

Block L5:
  result = PrimApp Int_add (x, y)
  return result
```

### Handler Management

**Exception handlers** affect closure conversion:

**Caller handler**:
- Used for most function calls
- Exception propagates to caller

**Handle expression**:
```sml
e1 handle exn => e2

(* SSA *)
Block L1:
  push_handler L_handler
  result = convertExp e1
  pop_handler
  goto L_continue

Block L_handler (exn):
  result = convertExp e2
  goto L_continue

Block L_continue:
  return result
```

## Optimizations

### Known Function Calls

**Optimization**: If we know exactly which function is being called, call it directly instead of through closure.

**Example**:
```sml
val f = fn x => x + 1
val result = f 42

(* Unoptimized: closure call *)
val f_env = {}
val f = (f_code, f_env)
val result = (let val (code, env) = f in code (env, 42) end)

(* Optimized: direct call *)
val f_env = {}
val f = (f_code, f_env)
val result = f_code (f_env, 42)
```

**Enabled by**: Abstract value analysis tracking exact lambda set.

### Global Variable Elimination

**Optimization**: Don't capture global variables in closures.

**Example**:
```sml
val x = 10
val f = fn y => x + y

(* Without globalization: capture x *)
val x = 10
val f_env = {x = x}
val f = (f_code, f_env)

(* With globalization: x is global *)
val x = 10
val f_env = {}
val f = (f_code, f_env)

fun f_code (env, y) = x + y  (* access global x directly *)
```

### Closure Flattening

**Optimization**: Flatten nested closure captures.

**Example**:
```sml
val x = 1
val f = fn y =>
   let val g = fn z => x + y + z
   in g end

(* Without flattening *)
(* f captures x, g captures (x, y) via f's closure *)

(* With flattening *)
(* f and g both directly capture x and y *)
```

## Development Guide

### Understanding Closure Conversion

To understand closure conversion:

1. **Read signatures**: Start with [closure-convert.sig](closure-convert.sig)
2. **Study phases**:
   - [lambda-free.fun](lambda-free.fun) - Free variable analysis
   - [globalize.fun](globalize.fun) - Global variable analysis
   - [abstract-value.fun](abstract-value.fun) - Value flow analysis
   - [closure-convert.fun](closure-convert.fun) - Main conversion
3. **Trace examples**: Follow simple examples through each phase
4. **Understand SSA output**: See [ssa](../ssa/) documentation

### Modifying Closure Conversion

**When adding SXML features**:

1. **Extend SXML IR**: Add new primitive expression form
2. **Add abstract value handling**: Update Value analysis if needed
3. **Add conversion case**: Handle new form in convertPrimExp
4. **Test**: Verify SSA output is correct

**When changing closure strategy**:

1. **Modify globalization**: Change what counts as "global"
2. **Update free variable analysis**: Change what counts as "free"
3. **Adjust closure creation**: Change environment record structure
4. **Test**: Ensure closures are correctly formed

### Debugging Closure Conversion

**View SSA output**:
```bash
mpl -keep-ssa program.mlb
cat program.mlb.ssa
```

**Enable diagnostics**:
```bash
mpl -diag-pass closureConvert program.mlb
```

**Common issues**:

**Issue: Variable not in scope**

**Symptom**: SSA uses undefined variable

**Cause**: Variable renaming failed or free variable not captured

**Fix**:
- Check lambdaFree analysis
- Verify variable is in closure environment
- Trace newScope / getNewVar calls

**Issue: Circular closure doesn't work**

**Symptom**: Recursive function can't call itself

**Cause**: Recursive reference not properly inserted into environment

**Fix**:
- Check mutual recursion handling in convertFun
- Verify environment record is filled with recursive pointers
- Ensure lambdaRec analysis is correct

**Issue: Closure environment type wrong**

**Symptom**: Type error in SSA

**Cause**: Mismatch between computed free variables and environment type

**Fix**:
- Check valueType computation
- Verify free variable list matches record type
- Trace Value.tuple creation

## Performance Considerations

**Closure allocation overhead**:
- Each lambda creates a closure record
- Globalization reduces unnecessary captures
- Known function optimization eliminates some allocations

**Flattening benefits**:
- Reduces indirection through nested closures
- More efficient access to captured variables
- Trade-off: larger closure records vs. fewer dereferences

**SSA size**:
- Closure conversion can significantly increase program size
- Each lambda becomes a top-level function
- Basic block linearization expands control flow

## Examples

### Example: Simple Closure

**SXML**:
```sml
MonoVal (result : int) =
   let
      MonoVal (x : int) = 10
      MonoVal (f : int -> int) = lambda (y : int) => x + y
   in
      App {func = f [int], arg = 42}
   end
```

**SSA**:
```sml
(* Top-level function *)
fun f_code (env : {x: int}, y : int) : int =
   let val x = #x env
   in x + y end

(* Main program *)
val x : int = 10
val f_env : {x: int} = {x = x}
val f : (({x: int} * int -> int) * {x: int}) = (f_code, f_env)
val (code, env) = f
val result : int = code (env, 42)
```

### Example: Mutual Recursion

**SXML**:
```sml
Fun (even : int -> bool, odd : int -> bool) =
   even = lambda (n : int) =>
      if n = 0 then true else odd (n - 1)
   odd = lambda (n : int) =>
      if n = 0 then false else even (n - 1)
```

**SSA**:
```sml
(* Environment type *)
type env = {even: closure, odd: closure}

(* Top-level functions *)
fun even_code (env : env, n : int) : bool =
   if n = 0
      then true
      else let val odd_closure = #odd env
               val (code, env') = odd_closure
           in code (env', n - 1) end

fun odd_code (env : env, n : int) : bool =
   if n = 0
      then false
      else let val even_closure = #even env
               val (code, env') = even_closure
           in code (env', n - 1) end

(* Closure creation *)
val env : env = allocate {even: _, odd: _}
val even : closure = (even_code, env)
val odd : closure = (odd_code, env)
val env = update (env, #even, even)
val env = update (env, #odd, odd)
```

### Example: Globalization

**SXML**:
```sml
MonoVal (limit : int) = 100
MonoVal (check : int -> bool) =
   lambda (x : int) => x < limit
```

**SSA** (with globalization):
```sml
val limit : int = 100

(* limit is global, not captured in closure *)
fun check_code (env : {}, x : int) : bool =
   x < limit  (* access global directly *)

val check_env : {} = {}
val check : closure = (check_code, check_env)
```

**SSA** (without globalization):
```sml
val limit : int = 100

fun check_code (env : {limit: int}, x : int) : bool =
   let val limit = #limit env
   in x < limit end

val check_env : {limit: int} = {limit = limit}
val check : closure = (check_code, check_env)
```

## See Also

- [SXML IR](../xml/) - Input to closure conversion (monomorphic XML)
- [SSA IR](../ssa/) - Output of closure conversion
- [XML IR](../xml/) - Polymorphic IR before monomorphisation
- [Monomorphise](../xml/) - Type specialization that produces SXML
- [Backend](../backend/) - Machine code generation from SSA

## References

- **Closure Conversion**: Classic compiler transformation for functional languages
- **SSA Form**: Static Single Assignment representation
- **Abstract Interpretation**: Value flow analysis technique
- **Lambda Lifting**: Related transformation for first-class functions
