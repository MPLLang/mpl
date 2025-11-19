# MLton Code Generation

Code generation transforms Machine IR to target language code (C, x86, AMD64, or LLVM).

## Overview

The codegen stage is the final compiler pass, generating executable code from [Machine IR](../backend/). MPL only supports **C code generation** (the x86, AMD64, and LLVM backends are present but not maintained for MPL).

**Supported codegens**:
- **C** ([c-codegen/](c-codegen/)): **Primary backend for MPL**, portable, maintained
- x86 ([x86-codegen/](x86-codegen/)): Not supported in MPL
- AMD64 ([amd64-codegen/](amd64-codegen/)): Not supported in MPL
- LLVM ([llvm-codegen/](llvm-codegen/)): Not supported in MPL

**This documentation focuses on C codegen**, the only backend used in MPL.

## C Code Generation

### Overview

**CCodegen** ([c-codegen/c-codegen.fun](c-codegen/c-codegen.fun)) generates C code from Machine IR:

**Input**: Machine.Program.t (Machine IR)
**Output**: C source files (`.c` and `.h`)

**Generated files**:
```
program.c           # Main compiled code
program.h           # Declarations and headers
program-consts.c    # Constants and static data
```

### File Organization

| File | Lines | Purpose |
|------|-------|---------|
| [c-codegen/c-codegen.sig](c-codegen/c-codegen.sig) | ~40 | C codegen signature |
| [c-codegen/c-codegen.fun](c-codegen/c-codegen.fun) | ~2,000 | C code generation |

### C Code Structure

**Overall structure**:
```c
/*** program.h ***/
#include "mlton-types.h"
#include "platform.h"

/* Forward declarations */
static void Chunk_0 (GC_state gcState);
static void Chunk_1 (GC_state gcState);
...

/* Global declarations */
static int64_t Global_0;
static double Global_1;
...

/*** program.c ***/
#include "program.h"

/* Function implementations */
static void Chunk_0 (GC_state gcState) {
   /* Temporaries as local variables */
   int64_t tmp0, tmp1, tmp2;
   double tmp3;

   /* Basic blocks with labels and gotos */
L_0:
   tmp0 = *(int64_t*)(gcState->stackTop + 0);
   tmp1 = *(int64_t*)(gcState->stackTop + 8);
   tmp2 = tmp0 + tmp1;
   goto L_1;

L_1:
   ...
}

int main (int argc, char** argv) {
   GC_state gcState = GC_init (&argc, &argv);
   Chunk_0 (gcState);
   GC_done (gcState);
   return 0;
}
```

### Generating Operands

**Operands** ([c-codegen.fun](c-codegen/c-codegen.fun)) translate to C expressions:

**Machine Operand** → **C Expression**:

```c
/* Constant */
Const (Int 42) → "42"
Const (Real 3.14) → "3.14"
Const (Word 0xFF) → "0xFF"

/* Temporary (register variable) */
Temporary {index=0, ty=int64} → "tmp0"
Temporary {index=1, ty=real64} → "ftmp1"

/* Global */
Global {index=5} → "Global_5"

/* Stack offset */
StackOffset {offset=16, ty=int64} →
   "*(int64_t*)(gcState->stackTop + 16)"

/* GC state */
GCState → "gcState"
Frontier → "gcState->frontier"
StackTop → "gcState->stackTop"

/* Memory offset */
Offset {base=Temporary 0, offset=8, ty=int64} →
   "*(int64_t*)(tmp0 + 8)"

/* Array indexing */
SequenceOffset {base=Temporary 0, index=Temporary 1,
                scale=8, offset=16, ty=int64} →
   "*(int64_t*)(tmp0 + 16 + (tmp1 * 8))"

/* Cast */
Cast (Temporary 0, real64) → "(double)tmp0"

/* Address */
Address (Global 0) → "&Global_0"
```

### Generating Statements

**Machine statements** translate to C statements:

**Move**:
```c
/* Machine: Move {dst = Temporary 0, src = Const 42} */
/* C: */
tmp0 = 42;
```

**PrimApp** (primitive operation):
```c
/* Machine: PrimApp {dst = Temporary 0,
                      prim = Word_add,
                      args = [Temporary 1, Temporary 2]} */
/* C: */
tmp0 = tmp1 + tmp2;
```

**Array subscript**:
```c
/* Machine: PrimApp {dst = Temporary 0,
                      prim = Array_sub,
                      args = [Temporary 1, Temporary 2]} */
/* C: */
tmp0 = ((int64_t*)(tmp1 + HEADER_SIZE + LENGTH_SIZE))[tmp2];
```

**Ref assignment**:
```c
/* Machine: PrimApp {prim = Ref_assign,
                      args = [Temporary 0, Temporary 1]} */
/* C: */
*(int64_t*)(tmp0 + HEADER_SIZE) = tmp1;
```

### Generating Transfers

**Transfers** ([c-codegen.fun](c-codegen/c-codegen.fun)) control flow:

**Goto**:
```c
/* Machine: Goto {dst = L_5, live = [...]} */
/* C: */
goto L_5;
```

**Call**:
```c
/* Machine: Call {label = Func_f, live = [...],
                   return = {handler = ..., size = ...}} */
/* C: */
gcState->returnAddress = &&Return_0;
Func_f (gcState);
Return_0: ;
```

**Return**:
```c
/* Machine: Return {live = [...]} */
/* C: */
goto *(gcState->returnAddress);
```

**Raise** (exception):
```c
/* Machine: Raise {live = [...]} */
/* C: */
gcState->exnStack = *(void**)(gcState->exnStack);
goto *(gcState->returnAddress);
```

**Switch** (multi-way branch):
```c
/* Machine: Switch {test = Temporary 0,
                     cases = [(1, L_1), (2, L_2)],
                     default = SOME L_default,
                     size = word64} */
/* C: */
switch (tmp0) {
   case 1: goto L_1;
   case 2: goto L_2;
   default: goto L_default;
}
```

**CCall** (C function call):
```c
/* Machine: CCall {func = "malloc",
                    args = [Temporary 0],
                    return = SOME Return_0} */
/* C: */
malloc (tmp0);
goto Return_0;
```

### Primitive Operations

**Primitives** ([c-codegen.fun](c-codegen/c-codegen.fun)) map to C operations or runtime calls:

**Arithmetic**:
```c
Word_add        → a + b
Word_sub        → a - b
Word_mul        → a * b
Word_quot       → a / b        (unsigned)
Word_rem        → a % b        (unsigned)
Int_quot        → a / b        (signed)
Real_add        → a + b
Real_mul        → a * b
```

**Comparisons**:
```c
Word_lt         → a < b
Word_le         → a <= b
Word_equal      → a == b
Int_lt          → a < b
Real_lt         → a < b
Real_equal      → a == b
```

**Bitwise**:
```c
Word_andb       → a & b
Word_orb        → a | b
Word_xorb       → a ^ b
Word_notb       → ~a
Word_lshift     → a << b
Word_rshift     → a >> b      (logical shift)
Word_arshift    → a >> b      (arithmetic shift)
```

**Conversions**:
```c
Word_toInt      → (int64_t)a
Int_toWord      → (uint64_t)a
Real_toInt      → (int64_t)a
Int_toReal      → (double)a
Word8_toWord64  → (uint64_t)a
Word64_toWord8  → (uint8_t)a
```

**Memory operations**:
```c
Array_sub       → ((T*)(base + HEADER + LENGTH))[index]
Array_update    → ((T*)(base + HEADER + LENGTH))[index] = value
Ref_deref       → *(T*)(base + HEADER)
Ref_assign      → *(T*)(base + HEADER) = value
```

**Runtime primitives**:
```c
GC_collect      → GC_collect (gcState, bytesRequested)
GC_allocateArray→ GC_allocateArray (gcState, length, elementSize, typeIndex)
Thread_copyCurrent → Thread_copyCurrent (gcState)
Thread_switchTo → Thread_switchTo (gcState, thread)
```

### Checked Operations

**Overflow checking**:
```c
/* Int_addCheck: add with overflow detection */
int64_t checked_add (int64_t a, int64_t b) {
   int64_t result;
   if (__builtin_add_overflow (a, b, &result)) {
      /* Raise Overflow exception */
      gcState->exnStack = (void*)Exn_Overflow;
      goto RaiseOverflow;
   }
   return result;
}
```

**Array bounds checking**:
```c
/* Array_sub with bounds check */
if (index < 0 || index >= length) {
   /* Raise Subscript exception */
   gcState->exnStack = (void*)Exn_Subscript;
   goto RaiseSubscript;
}
return ((int64_t*)(array + HEADER + LENGTH))[index];
```

### Static Heap

**Static heap** ([c-codegen.fun](c-codegen/c-codegen.fun)) contains compile-time constants:

**String constants**:
```c
/* String "hello" */
struct {
   GC_header header;
   uint64_t length;
   char data[6];  /* including null terminator */
} String_0 = {
   .header = STRING_HEADER (5),
   .length = 5,
   .data = "hello"
};
```

**Immutable tuples**:
```c
/* Tuple (1, 2, 3) */
struct {
   GC_header header;
   int64_t field0;
   int64_t field1;
   int64_t field2;
} Tuple_0 = {
   .header = TUPLE_HEADER,
   .field0 = 1,
   .field1 = 2,
   .field2 = 3
};
```

**Datatype constructors**:
```c
/* SOME 42 */
struct {
   GC_header header;
   uint32_t tag;
   int64_t value;
} Option_SOME_42 = {
   .header = DATATYPE_HEADER,
   .tag = TAG_SOME,
   .value = 42
};
```

### GC Interface

**Allocation fast path**:
```c
/* Inline bump-pointer allocation */
#define GC_ALLOCATE(size, typeIndex) \
   ({ \
      void* object; \
      if (gcState->frontier + (size) <= gcState->limit) { \
         object = gcState->frontier; \
         gcState->frontier += (size); \
         *(GC_header*)object = GC_MAKE_HEADER (typeIndex); \
      } else { \
         object = GC_allocate (gcState, (size), (typeIndex)); \
      } \
      object; \
   })
```

**GC triggers**:
```c
/* Before large allocation */
if (gcState->frontier + requiredSize > gcState->limit) {
   /* Save live registers to stack */
   *(int64_t*)(gcState->stackTop + 0) = tmp0;
   *(int64_t*)(gcState->stackTop + 8) = tmp1;

   /* Perform GC */
   GC_collect (gcState, requiredSize);

   /* Restore registers */
   tmp0 = *(int64_t*)(gcState->stackTop + 0);
   tmp1 = *(int64_t*)(gcState->stackTop + 8);
}

/* Allocate */
object = gcState->frontier;
gcState->frontier += requiredSize;
```

**Exception handling**:
```c
/* Push exception handler */
struct {
   void* previous;
   void* handler;
} exnFrame;
exnFrame.previous = gcState->exnStack;
exnFrame.handler = &&ExnHandler;
gcState->exnStack = &exnFrame;

/* Protected code */
...

/* Pop handler on normal return */
gcState->exnStack = exnFrame.previous;
goto NormalReturn;

/* Exception handler */
ExnHandler:
   gcState->exnStack = exnFrame.previous;
   /* Handle exception */
   ...
```

## Optimization Techniques

### Inline Small Operations

**Simple operations** inline directly:
```c
/* Instead of function call */
int64_t word_add (int64_t a, int64_t b) { return a + b; }
tmp0 = word_add (tmp1, tmp2);

/* Inline directly */
tmp0 = tmp1 + tmp2;
```

### Compiler Hints

**Inline hints**:
```c
static inline __attribute__((always_inline))
int64_t fast_path (int64_t x) {
   return x + 1;
}
```

**Unlikely branches**:
```c
if (__builtin_expect (frontier + size > limit, 0)) {
   /* Slow path: GC */
   GC_collect (gcState, size);
}
```

### Register Variables

**Hot temporaries** as register variables:
```c
register int64_t tmp0 asm ("rax");
register int64_t tmp1 asm ("rbx");
```

(Note: Modern C compilers largely ignore `register` keyword, but it can hint at importance.)

## Development Guide

### Understanding C Codegen

To understand C code generation:

1. **Read signature**: [c-codegen.sig](c-codegen/c-codegen.sig)
2. **Study main functor**: [c-codegen.fun](c-codegen/c-codegen.fun)
3. **Examine generated code**: Compile with `-keep-g` and inspect `.c` files
4. **Trace operand generation**: See how Machine operands become C expressions
5. **Follow control flow**: Understand labels and gotos

### Modifying C Codegen

**When adding primitives**:
1. Add case in `outputPrimApp` ([c-codegen.fun](c-codegen/c-codegen.fun))
2. Map to C operation or runtime call
3. Handle type conversions
4. Add runtime support if needed

**When optimizing output**:
1. Improve operand generation (avoid redundant casts)
2. Optimize control flow (reduce gotos where possible)
3. Better use of C compiler hints
4. Profile generated code

**When changing runtime interface**:
1. Update GC allocation code
2. Modify exception handling
3. Adjust thread operations
4. Keep in sync with runtime system

### Debugging C Codegen

**View generated C**:
```bash
mpl -keep-g program.mlb
cat program.c
```

**Compile with debug symbols**:
```bash
mpl -debug true -debug-runtime true program.mlb
```

**GDB debugging**:
```bash
gdb ./program
(gdb) break L_42        # Break at label
(gdb) print tmp0        # Inspect temporary
(gdb) print *gcState    # Inspect GC state
```

**Common issues**:

**Issue: C compiler errors**

**Symptom**: GCC/Clang fails to compile generated code

**Cause**: Invalid C syntax in generated code

**Fix**:
- Check operand generation
- Verify type casts
- Ensure label names are valid

**Issue: Runtime crash**

**Symptom**: Segmentation fault

**Cause**: Invalid memory access or GC corruption

**Fix**:
- Check generated memory operations
- Verify object header initialization
- Trace GC allocations

**Issue: Performance regression**

**Symptom**: Generated code slower than expected

**Cause**: Poor C code prevents compiler optimization

**Fix**:
- Reduce gotos (harder for optimizer)
- Improve temporary usage
- Add compiler hints
- Profile with `perf` or similar tools

## Performance Considerations

**C compiler optimization**:
- Generated code must be optimizer-friendly
- Excessive gotos hurt optimization
- Type casts can prevent optimizations
- Inline hints for hot paths

**Code size**:
- Large programs generate large C files
- C compiler memory usage scales with code size
- Chunking helps manage large programs

**Compilation time**:
- C compilation is the slowest part
- Parallel compilation of chunks helps
- Trade-off: codegen time vs. runtime performance

**Runtime efficiency**:
- Inline allocation fast path crucial
- GC interface overhead significant
- Exception handling on critical path

## Examples

### Example: Simple Function

**Machine IR**:
```sml
Chunk_0:
Block L_0 (args: none):
   tmp0 = StackOffset {offset=0, ty=int64}
   tmp1 = StackOffset {offset=8, ty=int64}
   tmp2 = Word_add (tmp0, tmp1)
   Return
```

**Generated C**:
```c
static void Chunk_0 (GC_state gcState) {
   int64_t tmp0, tmp1, tmp2;

L_0:
   tmp0 = *(int64_t*)(gcState->stackTop + 0);
   tmp1 = *(int64_t*)(gcState->stackTop + 8);
   tmp2 = tmp0 + tmp1;
   goto *(gcState->returnAddress);
}
```

### Example: Array Access

**Machine IR**:
```sml
Block L_0:
   arr = Temporary 0
   index = Temporary 1
   baseAddr = Offset {base=arr, offset=16, ty=int64}
   elemAddr = SequenceOffset {base=baseAddr, index=index, scale=8}
   value = Load elemAddr
   Return
```

**Generated C**:
```c
L_0:
   /* arr already in tmp0, index in tmp1 */
   tmp2 = *(int64_t*)(tmp0 + 16 + (tmp1 * 8));
   goto *(gcState->returnAddress);
```

### Example: Exception Handler

**Machine IR**:
```sml
Block L_try:
   PushHandler L_catch
   ...protected code...
   PopHandler
   Goto L_continue

Block L_catch:
   exn = ExnValue
   ...handler code...
   Goto L_continue
```

**Generated C**:
```c
L_try:
   struct {
      void* previous;
      void* handler;
   } exnFrame;
   exnFrame.previous = gcState->exnStack;
   exnFrame.handler = &&L_catch;
   gcState->exnStack = &exnFrame;

   /* Protected code */
   ...

   gcState->exnStack = exnFrame.previous;
   goto L_continue;

L_catch:
   tmp0 = *(int64_t*)(gcState->exnStack);
   gcState->exnStack = *(void**)(gcState->exnStack);
   /* Handler code */
   ...
   goto L_continue;
```

## See Also

- [Backend](../backend/) - Produces Machine IR (input to codegen)
- [Machine IR](../backend/machine.sig) - Machine representation
- [Runtime System](../../runtime/) - GC and runtime support
- [SSA IR](../ssa/) - Higher-level IR

## References

- **C Code Generation**: Techniques for compiling functional languages to C
- **Calling Conventions**: Platform-specific function call ABIs
- **Garbage Collection**: Integration with conservative/precise GC
- **Exception Handling**: Zero-cost vs. setjmp/longjmp approaches
