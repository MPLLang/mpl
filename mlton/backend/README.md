# MLton Backend

The backend transforms SSA2 to machine-level representations (RSSA, Machine) and generates C code.

## Overview

The backend sits at the end of the compilation pipeline, converting optimized [SSA2](../ssa/) to executable code. It consists of three main stages:

1. **SSA2 → RSSA**: Convert SSA2 to RSSA (Representational SSA) with explicit data layout
2. **RSSA → Machine**: Lower RSSA to machine-level operations
3. **Machine → C**: Generate C code (only supported backend in MPL)

**Pipeline**:
```
SSA2 (optimized)
  ↓ ssa2-to-rssa.fun (data representation, stack layout)
RSSA (with explicit representation)
  ↓ backend.fun (register allocation, code layout)
Machine (low-level operations)
  ↓ c-codegen.fun (C code generation)
C code
  ↓ gcc/clang
Executable
```

## Key Concepts

### IRs in the Backend

**SSA2**:
- High-level types (int, int list, etc.)
- Abstract data representation
- No stack/heap layout decisions

**RSSA** (Representational SSA):
- Explicit data representation (object headers, padding)
- Stack frame layout
- Runtime system interface
- Still in SSA form

**Machine**:
- Low-level operations (move, indexed load/store)
- Explicit operands (temporaries, stack offsets, globals)
- Basic blocks with transfers
- Register allocation decisions

**C**:
- Target language (generated code)
- Calls runtime system functions
- Portable across platforms

### Data Representation

**Packed representation** ([packed-representation.fun](packed-representation.fun)): Determines memory layout for all types:

**Basic types**:
```sml
int          → 8 bytes (64-bit)
word         → 8 bytes
real         → 8 bytes (double)
char         → 1 byte
```

**Compound types**:
```sml
int * bool   → tuple with object header + fields
int list     → datatype with tag + fields
int array    → array with length + elements
```

**Object headers**:
```c
struct {
  GC_header header;   /* type tag, mark bits, etc. */
  ...fields...
}
```

**Alignment**: Objects aligned to word boundaries (8 bytes on 64-bit).

**Padding**: Added to maintain alignment:
```sml
{a: word8, b: word64}
(* Layout: [header][a:1 byte][padding:7 bytes][b:8 bytes] *)
```

### Stack Layout

**Stack frames** contain:
- Local variables
- Spilled registers
- Return address
- Exception handler

**StackOffset** ([machine.sig](machine.sig)):
```sml
datatype StackOffset = T of {
   offset: Bytes.t,      (* offset from frame pointer *)
   ty: Type.t,           (* type of value *)
   volatile: bool        (* can change between saves *)
}
```

**Frame layout**:
```
High addresses
  [return address]
  [saved registers]
  [local variable 1] ← StackOffset {offset=0, ...}
  [local variable 2] ← StackOffset {offset=8, ...}
  [spilled temp 1]   ← StackOffset {offset=16, ...}
  ...
Low addresses (stack pointer)
```

### Register Allocation

**Variables** are assigned to:
- **Temporaries**: Register candidates
- **Stack offsets**: Spilled variables
- **Globals**: Top-level variables

**AllocateVariables** ([allocate-variables.fun](allocate-variables.fun)) assigns locations to all variables:

**Temporaries** ([machine.sig](machine.sig)):
```sml
structure Temporary:
   sig
      type t
      val new: Type.t * int option -> t
      val index: t -> int          (* register number *)
      val ty: t -> Type.t
   end
```

**Algorithm**:
1. **Liveness analysis**: Determine variable lifetimes
2. **Interference graph**: Track which variables overlap
3. **Graph coloring**: Assign registers avoiding conflicts
4. **Spilling**: Move to stack when registers exhausted

**Example**:
```sml
(* SSA2 *)
val x = 1 + 2
val y = x * 3
val z = x + y

(* After register allocation *)
Temp_0 = 1 + 2        (* x → Temporary 0 *)
Temp_1 = Temp_0 * 3   (* y → Temporary 1 *)
Temp_2 = Temp_0 + Temp_1  (* z → Temporary 2 *)
```

### Operands

**Machine operands** ([machine.sig](machine.sig)) represent values:

```sml
datatype Operand =
   Cast of Operand * Type
 | Const of Const
 | Frontier                       (* allocation pointer *)
 | GCState                        (* GC state *)
 | Global of Global               (* top-level variable *)
 | Label of Label                 (* code label *)
 | Offset of {base, offset, ty}  (* memory dereference *)
 | SequenceOffset of {base, index, scale, offset, ty}  (* array indexing *)
 | StackOffset of StackOffset     (* local variable *)
 | StackTop                       (* stack pointer *)
 | Temporary of Temporary         (* register variable *)
 | Address of Operand             (* address-of *)
```

**Example operands**:
```sml
Temporary (Temp 0)                    (* register %0 *)
StackOffset {offset=8, ty=int}        (* local var at offset 8 *)
Global (Global 5)                     (* global variable *)
Const (Int 42)                        (* immediate constant *)
Offset {base=Temp 1, offset=16}      (* *(temp1 + 16) *)
```

### Machine Instructions

**Statement** ([machine.sig](machine.sig)):
```sml
datatype Statement =
   Move of {dst: Operand, src: Operand}
 | PrimApp of {dst: Operand option,
               prim: Prim,
               args: Operand vector}
 | ProfileLabel of ProfileLabel
```

**Transfer** ([machine.sig](machine.sig)):
```sml
datatype Transfer =
   CCall of {args: Operand vector,
             func: CFunction,
             return: Label option}
 | Call of {label: Label,
            live: Live vector,
            return: Return}
 | Goto of {dst: Label,
            live: Live vector}
 | Raise of {live: Live vector}
 | Return of {live: Live vector}
 | Switch of {cases: (WordX.t * Label) vector,
              default: Label option,
              size: WordSize,
              test: Operand}
```

**Example**:
```sml
(* x = y + z *)
PrimApp {
   dst = SOME (Temporary temp_x),
   prim = Word_add WordSize.word64,
   args = [Temporary temp_y, Temporary temp_z]
}

(* goto L5 *)
Goto {
   dst = L5,
   live = [Temporary temp_x, Temporary temp_y]
}

(* if x < 10 goto L1 else L2 *)
Switch {
   test = Temporary temp_x,
   cases = [(0w10, L1)],
   default = SOME L2,
   size = WordSize.word64
}
```

## File Organization

### Backend Core

| File | Lines | Purpose |
|------|-------|---------|
| [backend.sig](backend.sig) | ~25 | Backend signature (RSSA → Machine) |
| [backend.fun](backend.fun) | ~1,500 | Main backend driver |
| [ssa2-to-rssa.sig](ssa2-to-rssa.sig) | ~20 | SSA2 → RSSA signature |
| [ssa2-to-rssa.fun](ssa2-to-rssa.fun) | ~2,500 | SSA2 to RSSA conversion |

### RSSA

| File | Lines | Purpose |
|------|-------|---------|
| [rssa.sig](rssa.sig) | ~15 | RSSA signature |
| [rssa.fun](rssa.fun) | ~10 | RSSA functor composition |
| [rssa-tree.sig](rssa-tree.sig) | ~250 | RSSA IR structure |
| [rssa-tree.fun](rssa-tree.fun) | ~900 | RSSA IR implementation |
| [rssa-simplify.fun](rssa-simplify.fun) | ~50 | RSSA simplification driver |
| [rssa-shrink.fun](rssa-shrink.fun) | ~300 | RSSA shrinking |
| [rssa-live.fun](rssa-live.fun) | ~350 | Liveness analysis |
| [rssa-restore.fun](rssa-restore.fun) | ~650 | Restore RSSA from Machine |
| [rssa-type-check.fun](rssa-type-check.fun) | ~900 | RSSA type checker |

### Machine

| File | Lines | Purpose |
|------|-------|---------|
| [machine.sig](machine.sig) | ~300 | Machine IR signature |
| [machine.fun](machine.fun) | ~1,800 | Machine IR implementation |

### Data Representation

| File | Lines | Purpose |
|------|-------|---------|
| [packed-representation.fun](packed-representation.fun) | ~2,700 | Memory layout computation |
| [rep-type.sig](rep-type.sig) | ~100 | Representation types signature |
| [rep-type.fun](rep-type.fun) | ~1,000 | Representation types |
| [object.sig](object.sig) | ~55 | Object layout signature |
| [object.fun](object.fun) | ~150 | Object layout implementation |
| [objptr-tycon.sig](objptr-tycon.sig) | ~30 | Object pointer type constructors |
| [objptr-tycon.fun](objptr-tycon.fun) | ~45 | Object pointer implementation |

### Optimizations & Transformations

| File | Lines | Purpose |
|------|-------|---------|
| [allocate-variables.sig](allocate-variables.sig) | ~65 | Register allocation signature |
| [allocate-variables.fun](allocate-variables.fun) | ~700 | Register allocation |
| [chunkify.sig](chunkify.sig) | ~20 | Code chunking signature |
| [chunkify.fun](chunkify.fun) | ~400 | Code chunking (split large functions) |
| [implement-handlers.fun](implement-handlers.fun) | ~200 | Exception handler implementation |
| [implement-profiling.fun](implement-profiling.fun) | ~1,000 | Profiling instrumentation |
| [parallel-move.sig](parallel-move.sig) | ~30 | Parallel assignment signature |
| [parallel-move.fun](parallel-move.fun) | ~65 | Parallel assignment implementation |

### Runtime Interface

| File | Lines | Purpose |
|------|-------|---------|
| [runtime.sig](runtime.sig) | ~60 | Runtime system interface |
| [runtime.fun](runtime.fun) | ~160 | Runtime function calls |

### Utilities

| File | Lines | Purpose |
|------|-------|---------|
| [backend-atoms.sig](backend-atoms.sig) | ~45 | Shared backend atoms |
| [backend-atoms.fun](backend-atoms.fun) | ~40 | Atoms implementation |
| [switch.sig](switch.sig) | ~35 | Switch optimization signature |
| [switch.fun](switch.fun) | ~75 | Switch optimization |
| [equivalence-graph.sig](equivalence-graph.sig) | ~50 | Variable equivalence tracking |
| [equivalence-graph.fun](equivalence-graph.fun) | ~50 | Equivalence graph implementation |

## SSA2 to RSSA Conversion

### Overview

**Ssa2ToRssa** ([ssa2-to-rssa.fun](ssa2-to-rssa.fun)) converts abstract SSA2 to RSSA with explicit representation:

**Key transformations**:
1. **Object headers**: Add GC headers to heap objects
2. **Type representation**: Compute memory layout for all types
3. **Array/Vector**: Add length fields, compute element offsets
4. **Stack variables**: Determine stack frame layout
5. **Runtime calls**: Insert GC checks, allocation calls

### Object Representation

**Heap objects** get explicit headers:

**Before (SSA2)**:
```sml
val x = (1, true, 3.0)
```

**After (RSSA)**:
```sml
(* Allocate object with header *)
val objSize = 8 (header) + 8 (int) + 8 (bool+padding) + 8 (real) = 32 bytes
val obj = alloc objSize
store (obj + 0, header)    (* GC header with type tag *)
store (obj + 8, 1)         (* field 0: int *)
store (obj + 16, true)     (* field 1: bool *)
store (obj + 24, 3.0)      (* field 2: real *)
val x = obj
```

**Header layout**:
```c
typedef uint64_t GC_header;
/* bits: [type_index | mark_bits | counter_bits | ...] */
```

### Array Representation

**Arrays** include length and elements:

**Before (SSA2)**:
```sml
val arr = Array.array (10, 0)
```

**After (RSSA)**:
```sml
val elementSize = 8 (* int *)
val numElements = 10
val arrSize = 8 (header) + 8 (length) + (10 * 8) (elements) = 96 bytes
val arr = alloc arrSize
store (arr + 0, header)
store (arr + 8, numElements)
(* Initialize elements to 0 *)
for i = 0 to 9:
   store (arr + 16 + i*8, 0)
```

**Array indexing**:
```sml
(* arr[i] *)
val elemAddr = arr + 16 + (i * elementSize)
val elem = load elemAddr
```

### GC Interface

**Allocation** calls runtime system:

**Small objects**:
```sml
(* Check if space available *)
if frontier + objSize > limit then
   callGC (objSize)
val obj = frontier
frontier := frontier + objSize
```

**Large objects**:
```sml
val obj = GC_allocateArray (numElements, elementSize)
```

**GC state**:
- `frontier`: Next allocation address
- `limit`: End of current heap block
- `stackTop`: Top of stack
- `exnStack`: Exception handler stack

## RSSA to Machine Conversion

### Overview

**Backend.toMachine** ([backend.fun](backend.fun)) lowers RSSA to Machine representation:

**Transformations**:
1. **Register allocation**: Assign variables to temporaries or stack
2. **Code layout**: Organize basic blocks for efficiency
3. **Chunking**: Split large functions into chunks
4. **Profile instrumentation**: Add profiling code if enabled
5. **Runtime interface**: Generate C function calls

### Register Allocation

**AllocateVariables** ([allocate-variables.fun](allocate-variables.fun)) performs graph-coloring register allocation:

**Steps**:
1. **Liveness analysis**: Compute live ranges
2. **Build interference graph**: Variables live at same time interfere
3. **Simplify**: Remove nodes with degree < k (available registers)
4. **Spill**: Choose variables to move to stack
5. **Color**: Assign registers to non-spilled variables

**Example**:
```sml
(* Before *)
x = 1
y = x + 2
z = x * y
w = y + 3

(* Liveness *)
x: live at y, z
y: live at z, w
z: live at (none after use)
w: live at (none after use)

(* Interference *)
x ↔ y (both live at z)
y ↔ z (not needed, different times)

(* Allocation *)
x → Temporary 0
y → Temporary 1
z → Temporary 0 (reuse, x dead)
w → Temporary 1 (reuse, y dead)
```

### Chunkifying

**Chunkify** ([chunkify.fun](chunkify.fun)) splits large functions:

**Why**: C compilers struggle with huge functions (slow compilation, high memory).

**Process**:
1. Identify large functions (> threshold basic blocks)
2. Split into chunks at safe points
3. Generate chunk labels and jumps
4. Maintain calling convention across chunks

**Example**:
```sml
(* Before: large function with 1000 blocks *)
fun f () =
   [L0, L1, ..., L999]

(* After: split into 10 chunks *)
chunk_0: [L0..L99]   → jump chunk_1
chunk_1: [L100..L199] → jump chunk_2
...
chunk_9: [L900..L999] → return
```

### Calling Convention

**Function calls** follow C calling convention:

**Arguments**: Passed via stack or registers (platform-dependent).

**Return values**: Returned via register or stack.

**Caller-save vs callee-save**: Determined by C ABI.

**Example**:
```sml
(* Call function f with args (x, y) *)
Call {
   label = f_label,
   live = [Temporary x_temp, Temporary y_temp],
   return = Return {
      handler = HandlerLabel h_label,
      size = SOME 16
   }
}
```

## Machine to C Codegen

### Overview

**CCodegen** ([c-codegen.fun](../codegen/c-codegen/c-codegen.fun)) generates C code from Machine:

**Generated files**:
- `program.c`: Main program logic
- `program.h`: Declarations
- `program-consts.c`: Constants and static data

### C Code Structure

**Main function**:
```c
int main (int argc, char** argv) {
  /* Initialize runtime */
  GC_init (&argc, &argv);

  /* Call compiled code */
  Main_main ();

  /* Cleanup */
  GC_done ();
  return 0;
}
```

**Compiled functions**:
```c
/* SSA function becomes C function */
static void Func_name (GC_state gcState) {
   /* Access arguments via stack */
   /* Temporaries as local C variables */

   /* Function body */
   L_0:
      tmp0 = arg0 + arg1;
      goto L_1;

   L_1:
      if (tmp0 < 10) goto L_2; else goto L_3;

   L_2:
      ...
}
```

**Globals**:
```c
/* Global variables */
static uint64_t Global_0;
static double Global_1;
```

**Static heap**:
```c
/* Immutable constants in static storage */
static struct {
   GC_header header;
   uint64_t length;
   char data[13];
} String_0 = {
   .header = STRING_HEADER,
   .length = 12,
   .data = "hello world"
};
```

### Primitive Operations

**Primitives** map to C operations:

| SSA Primitive | C Code |
|---------------|--------|
| `Word_add` | `a + b` |
| `Word_mul` | `a * b` |
| `Word_lt` | `a < b` |
| `Array_sub` | `((T*)(arr + HEADER_SIZE + LENGTH_SIZE))[index]` |
| `Ref_assign` | `*(T*)(ref + HEADER_SIZE) = value` |
| `Real_add` | `a + b` (for doubles) |

**Checked operations**:
```c
/* Integer add with overflow check */
int64_t checked_add (int64_t a, int64_t b) {
   int64_t result;
   if (__builtin_add_overflow (a, b, &result)) {
      raise_overflow ();
   }
   return result;
}
```

### GC Interface in C

**Allocation**:
```c
/* Fast path: inline bump-pointer allocation */
if (gcState->frontier + size <= gcState->limit) {
   object = gcState->frontier;
   gcState->frontier += size;
} else {
   /* Slow path: call GC */
   object = GC_allocate (gcState, size, typeIndex);
}
```

**GC calls**:
```c
/* Force garbage collection */
void GC_collect (GC_state gcState, size_t bytesRequested);

/* Allocate array */
pointer GC_allocateArray (GC_state gcState,
                         size_t length,
                         size_t elementSize,
                         uint32_t typeIndex);
```

## Development Guide

### Understanding the Backend

To understand the backend:

1. **Read signatures**: [backend.sig](backend.sig), [machine.sig](machine.sig)
2. **Study data representation**: [packed-representation.fun](packed-representation.fun)
3. **Follow SSA2 → RSSA**: [ssa2-to-rssa.fun](ssa2-to-rssa.fun)
4. **Trace RSSA → Machine**: [backend.fun](backend.fun)
5. **Examine C output**: [c-codegen.fun](../codegen/c-codegen/c-codegen.fun)

### Modifying the Backend

**When changing data layout**:
1. Edit [packed-representation.fun](packed-representation.fun)
2. Update object header format
3. Adjust GC to match new layout
4. Test with runtime system

**When adding primitives**:
1. Add to [atoms/prim.sig](../atoms/prim.sig)
2. Handle in SSA passes
3. Add RSSA case in [ssa2-to-rssa.fun](ssa2-to-rssa.fun)
4. Implement in [c-codegen.fun](../codegen/c-codegen/c-codegen.fun)
5. Possibly add runtime support

**When optimizing codegen**:
1. Modify register allocation ([allocate-variables.fun](allocate-variables.fun))
2. Improve instruction selection ([backend.fun](backend.fun))
3. Optimize C output ([c-codegen.fun](../codegen/c-codegen/c-codegen.fun))
4. Profile generated code

### Debugging the Backend

**View RSSA output**:
```bash
mpl -keep-rssa program.mlb
cat program.mlb.rssa
```

**View Machine output**:
```bash
mpl -keep-machine program.mlb
cat program.mlb.machine
```

**View generated C**:
```bash
mpl -keep-g program.mlb
cat program.c program.h
```

**Type-check RSSA**:
```bash
mpl -type-check-rssa true program.mlb
```

**Common issues**:

**Issue: Stack overflow**

**Symptom**: Segmentation fault at runtime

**Cause**: Insufficient stack space

**Fix**:
- Check stack frame sizes in RSSA
- Verify stack pointer management
- Increase system stack limit

**Issue: GC assertion failure**

**Symptom**: Runtime error in garbage collector

**Cause**: Invalid object header or corrupted heap

**Fix**:
- Check object layout in [packed-representation.fun](packed-representation.fun)
- Verify header initialization in [ssa2-to-rssa.fun](ssa2-to-rssa.fun)
- Trace allocation in generated C code

**Issue: Register allocation failure**

**Symptom**: Compiler error during backend

**Cause**: Too many live variables for available registers

**Fix**:
- Increase spilling in [allocate-variables.fun](allocate-variables.fun)
- Simplify SSA program
- Split large functions

## Performance Considerations

**Register allocation quality**:
- Graph coloring is NP-hard
- Heuristics impact code quality
- More spilling → slower code

**Code layout**:
- Block ordering affects branch prediction
- Chunking impacts instruction cache
- Trade-off: compile time vs. runtime

**C compiler optimization**:
- Generated C must be optimizer-friendly
- Inline hints for hot paths
- Avoid constructs that confuse optimizer

**Data layout**:
- Alignment affects memory access speed
- Padding wastes space
- Trade-off: speed vs. memory usage

## See Also

- [SSA IR](../ssa/) - Input to backend (SSA2)
- [Closure Convert](../closure-convert/) - Produces SSA
- [C Codegen](../codegen/c-codegen/) - C code generation
- [Runtime System](../../runtime/) - GC and runtime support

## References

- **SSA Form**: Static Single Assignment representation
- **Graph Coloring**: Register allocation algorithm
- **Calling Conventions**: Platform-specific function call rules
- **Garbage Collection**: Memory management integration
