# Atoms: Primitive Operations and Basic Compiler Building Blocks

The atoms directory contains fundamental compiler building blocks including primitive operations, constants, variables, types, and other basic constructs used throughout the compiler.

## Overview

**Purpose**: Core data structures and primitives for the entire compiler
- Primitive operations (`Prim.t`): Low-level operations
- Variables, functions, labels: Naming and identification
- Types and type constructors: Type representations
- Constants: Literal values
- FFI support: Foreign function interface structures

**Key Concept**: "Atoms" are atomic, indivisible units that form the basis of all compiler intermediate representations.

## Key Files and Structures

### Core Primitive Operations

#### [prim.sig](prim.sig), [prim.fun](prim.fun) - Primitive Operations
**Most important files in atoms/**: Define all primitive operations supported by the compiler.

**datatype Prim.t**:
The `Prim.t` datatype enumerates all primitive operations. These are low-level operations that cannot be expressed in Standard ML and must be provided by the compiler/runtime.

**Categories**:
- **Array operations**: alloc, array, length, sub, update, CAS
- **Vector operations**: vector, length, sub
- **Reference operations**: ref, deref, assign, CAS
- **Word operations**: arithmetic, bitwise, comparisons (sized)
- **Real operations**: arithmetic, math functions (sized)
- **IntInf operations**: Arbitrary-precision integer operations
- **CPointer operations**: C pointer manipulation
- **Thread operations**: Thread control, atomic operations
- **GC operations**: Garbage collection interface
- **Exception operations**: Exception handling
- **FFI operations**: Foreign function calls
- **Parallel operations**: Fork, join, work-stealing

See prim.sig:27-195 for complete list.

### Variables and Identifiers

#### [var.sig](var.sig), [var.fun](var.fun) - Variables
**Purpose**: Variable identifiers throughout compilation
- Unique naming for SSA and other IRs
- Property lists for analysis
- Alpha-renaming support

#### [func.sig](func.sig) - Functions
**Purpose**: Function identifiers
- Function names in all IRs
- Used for function calls and definitions

#### [label.sig](label.sig) - Labels
**Purpose**: Basic block labels
- Control flow destinations
- Used in SSA Transfer constructs

#### [con-.sig](con-.sig), [con-.fun](con-.fun) - Constructors
**Purpose**: Datatype constructors
- Constructor names for pattern matching
- Used in case expressions

### Type System Atoms

#### [tycon.sig](tycon.sig), [tycon.fun](tycon.fun) - Type Constructors
**Purpose**: Type constructor names
- Base type constructors (int, real, array, etc.)
- User-defined datatype names
- Parameterized types

#### [tyvar.sig](tyvar.sig), [tyvar.fun](tyvar.fun) - Type Variables
**Purpose**: Type variables for polymorphism
- Used in type inference and elaboration
- Tracked through XML IR

### Constants

#### [const.sig](const.sig), [const.fun](const.fun) - Constants
**Purpose**: Literal constant values
- Integer constants (all sizes)
- Real constants (all sizes)
- Word constants (all sizes)
- String and character constants
- Special constants (IntInf)

**Used by**: All IRs for literal values

### Foreign Function Interface

#### [c-function.sig](c-function.sig), [c-function.fun](c-function.fun) - C Functions
**Purpose**: C function declarations for FFI
- Function name and signature
- Calling convention
- Return type and arguments

#### [c-type.sig](c-type.sig), [c-type.fun](c-type.fun) - C Types
**Purpose**: C type representations
- Primitive C types (int, float, pointer)
- Struct and array types
- Type conversion between ML and C

#### [c-symbol.sig](c-symbol.sig), [c-symbol.fun](c-symbol.fun) - C Symbols
**Purpose**: C symbol names and scope
- External symbol naming
- Linkage and visibility

#### [ffi.sig](ffi.sig), [ffi.fun](ffi.fun) - FFI Support
**Purpose**: Foreign function interface utilities
- FFI declarations
- Type marshalling
- Symbol management

### Size and Attributes

#### [word-size.sig](word-size.sig), [word-size.fun](word-size.fun) - Word Sizes
**Purpose**: Word type sizes
- W8, W16, W32, W64
- Used for sized primitive operations

#### [real-size.sig](real-size.sig), [real-size.fun](real-size.fun) - Real Sizes
**Purpose**: Floating-point sizes
- R32 (float), R64 (double)
- Used for sized real operations

#### [int-size.sig](int-size.sig), [int-size.fun](int-size.fun) - Integer Sizes
**Purpose**: Integer type sizes
- Corresponds to word sizes
- Used for integer operations

#### [char-size.sig](char-size.sig), [char-size.fun](char-size.fun) - Character Sizes
**Purpose**: Character representation sizes
- char (8-bit) and widechar (32-bit)

### Parallel Primitives

#### [spid.sig](spid.sig), [spid.fun](spid.fun) - Spork IDs
**Purpose**: Unique identifiers for parallel spork operations
- Tracks fork-join pairs
- Used in Spork/Spoin transfers

### Other Atoms

#### [field.sig](field.sig), [field.fun](field.fun) - Record Fields
**Purpose**: Record and tuple field names

#### [inline-attr.sig](inline-attr.sig), [inline-attr.fun](inline-attr.fun) - Inlining Attributes
**Purpose**: Control inlining decisions
- Inline, NoInline, AlwaysInline

#### [profile-exp.sig](profile-exp.sig), [profile-exp.fun](profile-exp.fun) - Profiling
**Purpose**: Profiling expressions

#### [handler.sig](handler.sig), [handler.fun](handler.fun) - Exception Handlers
**Purpose**: Exception handler representations

### Top-Level Module

#### [atoms.sig](atoms.sig), [atoms.fun](atoms.fun)
**Purpose**: Aggregates all atom structures
- Provides single module with all atoms
- Used by IR implementations

## Primitive Operations in Detail

### Array Primitives

**Creation and initialization**:
- `Array_alloc {raw}`: Allocate array (raw = uninitialized)
- `Array_array`: Construct initialized array
- `Array_uninit`: Mark array element as uninitialized
- `Array_uninitIsNop`: Check if uninit is noop for type

**Access**:
- `Array_length`: Get array length
- `Array_sub {readBarrier}`: Array subscript with optional read barrier
- `Array_update {writeBarrier}`: Array update with optional write barrier

**Concurrent**:
- `Array_cas ct`: Compare-and-swap on array element

**Conversion**:
- `Array_toVector`: Convert array to vector
- `Array_toArray`: Identity (for type coercion)

**Copying**:
- `Array_copyArray`: Copy between arrays
- `Array_copyVector`: Copy vector to array

### Reference Primitives

- `Ref_ref`: Create reference
- `Ref_deref {readBarrier}`: Dereference with optional barrier
- `Ref_assign {writeBarrier}`: Assignment with optional barrier
- `Ref_cas ct`: Compare-and-swap on reference

**Read/Write barriers**: Used for concurrent GC coordination

### Word Primitives

All word operations are sized (Word8, Word16, Word32, Word64):

**Arithmetic**:
- `Word_add ws`: Addition
- `Word_sub ws`: Subtraction
- `Word_mul (ws, {signed})`: Multiplication
- `Word_neg ws`: Negation
- `Word_quot (ws, {signed})`: Quotient
- `Word_rem (ws, {signed})`: Remainder

**Checked arithmetic** (overflow detection):
- `Word_addCheckP (ws, {signed})`: Checked addition
- `Word_subCheckP (ws, {signed})`: Checked subtraction
- `Word_mulCheckP (ws, {signed})`: Checked multiplication
- `Word_negCheckP (ws, {signed})`: Checked negation

**Bitwise**:
- `Word_andb ws`: Bitwise AND
- `Word_orb ws`: Bitwise OR
- `Word_xorb ws`: Bitwise XOR
- `Word_notb ws`: Bitwise NOT

**Shifts and rotates**:
- `Word_lshift ws`: Left shift
- `Word_rshift (ws, {signed})`: Right shift (logical or arithmetic)
- `Word_rol ws`: Rotate left
- `Word_ror ws`: Rotate right

**Comparisons**:
- `Word_equal ws`: Equality
- `Word_lt (ws, {signed})`: Less than

**Conversions**:
- `Word_extdToWord (ws1, ws2, {signed})`: Extend/truncate
- `Word_castToReal (ws, rs)`: Cast to real
- `Word_rndToReal (ws, rs, {signed})`: Convert to real
- `Word_toIntInf`: Convert to arbitrary precision

### Real Primitives

Floating-point operations (sized: Real32, Real64):

**Arithmetic**:
- `Real_add rs`, `Real_sub rs`, `Real_mul rs`, `Real_div rs`
- `Real_neg rs`, `Real_abs rs`

**Fused multiply-add**:
- `Real_muladd rs`: a * b + c
- `Real_mulsub rs`: a * b - c

**Comparisons**:
- `Real_equal rs`, `Real_lt rs`, `Real_le rs`
- `Real_qequal rs`: Quiet equality (no NaN exceptions)

**Math functions**:
- `Real_Math_sqrt rs`, `Real_Math_exp rs`, `Real_Math_ln rs`
- `Real_Math_sin rs`, `Real_Math_cos rs`, `Real_Math_tan rs`
- `Real_Math_asin rs`, `Real_Math_acos rs`, `Real_Math_atan rs`
- `Real_Math_atan2 rs`, `Real_Math_log10 rs`

**Conversions**:
- `Real_rndToWord (rs, ws, {signed})`: Convert to word
- `Real_rndToReal (rs1, rs2)`: Convert between sizes
- `Real_castToWord (rs, ws)`: Bit pattern cast
- `Real_round rs`: Round to nearest integer
- `Real_ldexp rs`: ldexp function (x * 2^n)

### IntInf Primitives

Arbitrary-precision integer operations (delegated to GMP library):

**Arithmetic**:
- `IntInf_add`, `IntInf_sub`, `IntInf_mul`
- `IntInf_neg`, `IntInf_quot`, `IntInf_rem`
- `IntInf_gcd`: Greatest common divisor

**Bitwise**:
- `IntInf_andb`, `IntInf_orb`, `IntInf_xorb`, `IntInf_notb`
- `IntInf_lshift`, `IntInf_arshift`: Shifts

**Other**:
- `IntInf_compare`: Three-way comparison
- `IntInf_toString`: String conversion
- `IntInf_toWord`, `IntInf_toVector`: Conversions

### Thread and Parallel Primitives

**Thread control**:
- `Thread_atomicBegin`, `Thread_atomicEnd`: Atomic sections
- `Thread_atomicState`: Query atomic state
- `Thread_switchTo`: Context switch (enters runtime)
- `Thread_copy`, `Thread_copyCurrent`: Thread duplication
- `Thread_returnToC`: Return to C runtime

**Parallel operations**:
- `Spork {tokenSplitPolicy}`: Fork parallel computation
- `Spork_forkThreadAndSetData {youngest}`: Low-level fork
- `Spork_getData spid`: Get spork-specific data

**Heartbeat** (granularity control):
- `Heartbeat_tokens`: Get available heartbeat tokens

### GC and Runtime Primitives

**Garbage collection**:
- `GC_collect`: Force garbage collection
- `GC_state`: Get GC state pointer

**MLton runtime**:
- `MLton_bogus`: Create bogus value of any type
- `MLton_bug`: Abort with bug message
- `MLton_eq`: Physical equality
- `MLton_equal`: Polymorphic equality (implemented by poly-equal pass)
- `MLton_hash`: Polymorphic hash (implemented by poly-hash pass)
- `MLton_halt`: Halt execution
- `MLton_size`: Size of value in bytes
- `MLton_touch`: Keep value live
- `MLton_share`: Maximize sharing

**Signal handling**:
- `MLton_installSignalHandler`: Install signal handler
- `MLton_handlesSignals`: Query if program handles signals

### Exception Primitives

- `Exn_extra`: Exception extra field
- `Exn_name`: Exception name
- `Exn_setExtendExtra`: Set extended extra field
- `TopLevel_getHandler`, `TopLevel_setHandler`: Top-level handler

### CPointer Primitives

C pointer manipulation:

**Arithmetic**:
- `CPointer_add`, `CPointer_sub`: Pointer arithmetic
- `CPointer_diff`: Pointer difference

**Comparisons**:
- `CPointer_equal`, `CPointer_lt`: Comparisons

**Conversions**:
- `CPointer_fromWord`, `CPointer_toWord`: Word conversions

**Memory access**:
- `CPointer_getCPointer`, `CPointer_setCPointer`: Pointer fields
- `CPointer_getObjptr`, `CPointer_setObjptr`: Object pointer fields
- `CPointer_getReal rs`, `CPointer_setReal rs`: Real fields
- `CPointer_getWord ws`, `CPointer_setWord ws`: Word fields


## Adding a New Primitive

Complete workflow with example:

### 1. Add Constructor to Prim.t

**In [prim.sig](prim.sig)** (signature):
```sml
(* Around line 27-195, add in appropriate category *)
datatype 'a t =
   ...
 | MyNewPrimitive of {arg1: SomeType, arg2: bool}
   ...
```

**In [prim.fun](prim.fun)** (implementation):
```sml
(* Match the datatype definition *)
datatype 'a t =
   ...
 | MyNewPrimitive of {arg1: SomeType, arg2: bool}
   ...
```

### 2. Add String Name (toString)

**In [prim.fun](prim.fun)**:
```sml
fun toString prim =
   case prim of
      ...
    | MyNewPrimitive _ => "my_new_primitive"
      ...
```

This name is used for:
- Diagnostic output
- Matching with basis library `_prim` declarations
- Debug printing

### 3. Implement Type Checking (checkApp)

**In [prim.fun](prim.fun)**:
```sml
fun checkApp (prim, {args, result, targs, typeOps = {...}}) =
   case prim of
      ...
    | MyNewPrimitive _ =>
         (* Check argument types and result type *)
         Vector.length args = 2
         andalso isIntType (Vector.sub (args, 0))
         andalso equals (result, Vector.sub (args, 1))
      ...
```

**Purpose**: Verify primitive application is well-typed

### 4. Implement extractTargs (if polymorphic)

**In [prim.fun](prim.fun)**:
```sml
fun extractTargs (prim, {args, result, typeOps = {deArray, ...}}) =
   case prim of
      ...
    | MyNewPrimitive _ =>
         (* Extract type arguments from args/result *)
         Vector.new1 (deArray (Vector.sub (args, 0)))
      ...
```

**Purpose**: Extract type arguments for type-checking

### 5. Implement map (if contains type variables)

**In [prim.fun](prim.fun)**:
```sml
fun map (prim: 'a t, f: 'a -> 'b): 'b t =
   case prim of
      ...
    | MyNewPrimitive {arg1, arg2} =>
         (* If prim contains type variables, map over them *)
         MyNewPrimitive {arg1 = arg1, arg2 = arg2}
      ...
```

### 6. Add Basis Library Declaration

**In basis-library/** (appropriate module):
```sml
(* Declare primitive with type signature *)
val myFunction =
   _prim "MyNewPrimitive": int * 'a -> 'a;

(* Optional: wrapper function *)
fun myPublicFunction (x, y) =
   let
      (* Validation or setup *)
      val () = if x < 0 then raise Domain else ()
   in
      myFunction (x, y)
   end
```

**`_prim` keyword**: Compiler recognizes this and links to primitive

### 7. Implement in Compiler Passes

Decide where primitive is lowered/implemented:

**Option A: Closure Conversion** (SXML → SSA):
```sml
(* In mlton/closure-convert/closure-convert.fun *)
case Sprim.name prim of
   Sprim.MyNewPrimitive =>
      (* Generate SSA code implementing primitive *)
      ...
```

**Option B: SSA Pass**:
```sml
(* In mlton/ssa/<pass>.fun *)
case Prim.name prim of
   Prim.MyNewPrimitive =>
      (* Transform or optimize primitive *)
      ...
```

**Option C: Backend (SSA2 → RSSA → Machine)**:
```sml
(* In mlton/backend/ or mlton/codegen/ *)
case Prim.name prim of
   Prim.MyNewPrimitive =>
      (* Generate low-level code *)
      ...
```

**Option D: Codegen** (Machine → C):
```sml
(* In mlton/codegen/c-codegen.fun *)
case Prim.name prim of
   Prim.MyNewPrimitive =>
      (* Generate C code *)
      ...
```

**Choose based on**:
- Complexity: Complex primitives implement in closure conversion or early SSA
- Optimization: If needs optimization, implement in SSA passes
- Low-level: Memory/hardware ops implement in backend/codegen

### 8. Test

```bash
# Rebuild compiler
make compiler

# Test program using primitive
cat > test.sml <<EOF
val x = myFunction (42, "hello")
EOF

# Compile
mpl test.mlb

# Debug
mpl -diag-pass closureConvert test.mlb    # If implemented in closure conversion
mpl -keep-ssa test.mlb                    # View SSA representation
```


## Primitive Application

### In SSA IR

Primitives appear as:
```sml
Statement.T {var = SOME x,
             ty = resultType,
             exp = Exp.PrimApp {prim = Prim.Word_add WordSize.word32,
                                targs = Vector.new0 (),
                                args = #[y, z]}}
```

### Type Checking Primitive Applications

```sml
(* Automatic via Ssa.typeCheck *)
val ok = Prim.checkApp (prim, {args = argTypes,
                                result = resultType,
                                targs = typeArgs,
                                typeOps = ...})
```

## Common Issues

**Issue**: Primitive not recognized
- **Solution**: Check spelling in `_prim` declaration matches `toString`
- **Debug**: Grep for primitive name in prim.fun

**Issue**: Type error in primitive application
- **Solution**: Check `checkApp` implementation
- **Debug**: Add diagnostic in checkApp, use `-type-check true`

**Issue**: Primitive not implemented
- **Solution**: Implement in appropriate pass (closure conversion, SSA, backend)
- **Debug**: Grep for primitive name in compiler, check which pass should handle it

**Issue**: Compile-time choice not working
- **Solution**: Check closure conversion implementation and threshold
- **Debug**: Use `-diag-pass closureConvert` to see decisions

## See Also

- [prim.sig](prim.sig), [prim.fun](prim.fun) - Primitive operations
- [atoms.sig](atoms.sig), [atoms.fun](atoms.fun) - Atom aggregation
- [../closure-convert/closure-convert.fun](../closure-convert/closure-convert.fun) - Closure conversion
- [../ssa/README.md](../ssa/README.md) - SSA IR
- [../backend/README.md](../backend/README.md) - Backend
- [../control/README.md](../control/README.md) - Compiler flags
- [../README.md](../README.md) - Compiler overview

## Quick Reference

### Common Primitive Patterns

**Array subscript**:
```sml
Prim.Array_sub {readBarrier = false}
```

**Word addition**:
```sml
Prim.Word_add WordSize.word32
```

**Real multiplication**:
```sml
Prim.Real_mul RealSize.real64
```

**Reference dereference**:
```sml
Prim.Ref_deref {readBarrier = false}
```

**Compare-and-swap**:
```sml
Prim.Array_cas (SOME CType.Word32)
```

**FFI call**:
```sml
Prim.CFunction (CFunction.T {name = "foo", ...})
```
