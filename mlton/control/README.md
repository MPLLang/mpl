# Compiler Control System

The control system manages compiler flags, options, and configuration throughout the compilation pipeline. It provides a central mechanism for controlling optimization levels, diagnostics, code generation, and other compiler behaviors.

## Overview

**Purpose**: Centralized control and configuration for the entire compiler
- Compiler flags and command-line options
- Optimization pass control
- Diagnostic and debugging output
- Code generation options
- Elaboration controls

**Key Components**:
- Flag definitions ([control-flags.sig](control-flags.sig), [control-flags.sml](control-flags.sml))
- Command-line parsing ([../main/main.fun](../main/main.fun))
- Control structure ([control.sig](control.sig), [control.sml](control.sml))
- Pass management and diagnostics

## Key Files

### [control-flags.sig](control-flags.sig)
**Purpose**: Signature defining all compiler flags
- Declares all flag references
- Types for flag values (bool, int, string, enums)
- Elaboration control structure
- Format and codegen options

### [control-flags.sml](control-flags.sml)
**Purpose**: Implementation of compiler flags
- Defines default values
- Implements flag accessors
- Provides serialization (layout, all)

### [control.sig](control.sig), [control.sml](control.sml)
**Purpose**: Main control structure
- Pass management (translatePass, simplifyPass)
- Error tracking and reporting
- Diagnostic output
- Type checking control

### [../main/main.fun](../main/main.fun)
**Purpose**: Command-line interface
- Parses command-line arguments
- Maps options to flags
- Validates flag values
- Help and usage messages

## Flag Categories

### Optimization Control

**Pass execution**:
```sml
val executePasses: (Regexp.Compiled.t * bool) list ref
```
Enable/disable specific optimization passes:
```bash
mpl -disable-pass inline program.mlb        # Disable inlining
mpl -enable-pass deep-flatten program.mlb   # Enable deep flattening
```

**Optimization level**:
```bash
mpl -opt-passes minimal program.mlb   # Minimal optimizations (poly-equal, poly-hash only)
mpl -opt-passes default program.mlb   # Full optimization pipeline
```

### Diagnostic and Debugging

**Pass diagnostics**:
```sml
val diagPasses: Regexp.Compiled.t list ref
```
View diagnostic output from specific passes:
```bash
mpl -diag-pass closureConvert program.mlb   # See closure conversion decisions
mpl -diag-pass inline program.mlb           # See inlining decisions
mpl -diag-pass ".*" program.mlb             # All pass diagnostics (regex match)
```

**Keep intermediate files**:
```sml
val keepXML: bool ref
val keepSXML: bool ref
val keepSSA: bool ref
val keepSSA2: bool ref
val keepRSSA: bool ref
val keepMachine: bool ref
```
```bash
mpl -keep-xml -keep-ssa -keep-ssa2 program.mlb
# Produces program.{xml,ssa,ssa2}
```

**Keep after specific pass**:
```bash
mpl -keep-pass constantPropagation program.mlb
# Produces program.constantPropagation.ssa
```

**Debug mode**:
```sml
val debug: bool ref
```
```bash
mpl -debug true program.mlb           # Debug info in compiler
mpl -debug-runtime true program.mlb   # Use debug runtime library
```

### Type Checking

```sml
val typeCheck: bool ref
```
```bash
mpl -type-check true program.mlb   # Enable extra type checking
```

### Code Generation

**Codegen selection**:
```sml
datatype Codegen.t = CCodegen
val codegen: Codegen.t ref
```
```bash
mpl -codegen c program.mlb   # C code generation (only supported in MPL)
```

**Comments in generated code**:
```sml
val codegenComments: int ref
```
```bash
mpl -codegen-comments 2 program.mlb   # More comments in generated C
```

**Optimization fusion**:
```sml
val codegenFuseOpAndChk: bool ref
```

### Parallel and MPL-Specific

**Entanglement detection**:
```sml
val detectEntanglement: bool ref
val detectEntanglementRuntime: bool ref
```
```bash
mpl -detect-entanglement true program.mlb
```

**Compiler analysis thresholds**:
Various compiler flags control optimization thresholds and heuristics. Check `-help` for available threshold flags.

### Closure Conversion

```sml
val closureConvertGlobalize: bool ref
val closureConvertShrink: bool ref
```
```bash
mpl -closure-convert-globalize true program.mlb
mpl -closure-convert-shrink false program.mlb
```

### Inlining Control

```sml
val inlineLeafA: {product: int, size: int} ref
val inlineLeafB: {product: int, size: int} ref
val inlineNonRec: {product: int, small: int, size: int} ref
```

Heuristics for inlining decisions:
- `product`: Product of caller size and callee size
- `size`: Maximum function size
- `small`: Size threshold for "small" functions

```bash
mpl -inline-leaf-a "(320, 20)" program.mlb
mpl -inline-non-rec "(60, 350, 60)" program.mlb
```

### Elaboration Control

The elaborate structure controls type checking and language features:

```sml
structure Elaborate:
   sig
      val allowFFI: (bool,bool) t
      val allowPrim: (bool,bool) t
      val allowConstant: (bool,bool) t
      val deadCode: (bool,bool) t
      val nonexhaustiveMatch: (DiagEIW.t,DiagEIW.t) t
      ...
   end
```

**Examples**:
```bash
mpl -default-ann 'nonexhaustiveMatch warn' program.mlb
mpl -default-ann 'allowFFI true' program.mlb
```

### Default Types

```sml
val defaultInt: string ref
val defaultWord: string ref
val defaultReal: string ref
val defaultChar: string ref
```

```bash
mpl -default-type int64 program.mlb
mpl -default-type word64 program.mlb
```

### Output Control

**Output file**:
```bash
mpl -output foo program.mlb   # Creates executable 'foo'
```

**Export header**:
```sml
val exportHeader: File.t option ref
```
```bash
mpl -export-header program.h program.mlb
```

**Stop after stage**:
```bash
mpl -stop xml program.mlb     # Stop after XML
mpl -stop ssa program.mlb     # Stop after SSA
mpl -stop ssa2 program.mlb    # Stop after SSA2
mpl -stop g program.mlb       # Stop after C generation
```

## Adding a New Compiler Flag

Follow this workflow to add a new flag:

### 1. Declare Flag in control-flags.sig

Add declaration to signature:

```sml
(* In control-flags.sig *)
signature CONTROL_FLAGS =
   sig
      ...
      val myNewFlag: int ref
      ...
   end
```

### 2. Implement Flag in control-flags.sml

Add implementation with default value:

```sml
(* In control-flags.sml *)
structure ControlFlags: CONTROL_FLAGS =
struct
   ...
   val myNewFlag: int ref =
      control {name = "my-new-flag",
               default = 100,
               toString = Int.toString}
   ...
end
```

**The `control` function**:
- Registers the flag with the control system
- Sets default value
- Provides string conversion for display
- Returns a ref that can be read/written

### 3. Add Command-Line Parsing in main.fun

Add option to the options list in [../main/main.fun](../main/main.fun):

```sml
(* In main.fun, in the 'options' list *)
(Expert, "my-new-flag", " <n>",
 "description of what this flag does, default = 100",
 Int (fn n =>
      if n < 0
         then usage "my-new-flag must be non-negative"
      else myNewFlag := n)),
```

**Option tuple structure**:
```sml
(Visibility, "flag-name", " <arg-format>",
 "description",
 Handler)
```

**Visibility**:
- `Normal`: Regular users should know about this
- `Expert`: For compiler developers/experts
- `Other`: Internal or special purpose

**Handlers**:
- `Bool (fn b => flag := b)`: Boolean flag
- `Int (fn n => flag := n)`: Integer flag
- `String (fn s => flag := s)`: String flag
- `SpaceString (fn s => ...)`: String with required space
- Custom validation: Check value and call `usage` if invalid

### 4. Use Flag in Compiler Passes

Access the flag using `!Control.flagName`:

```sml
(* In any compiler pass *)
fun transform program =
   let
      val threshold = !Control.myNewFlag
      val () = if threshold > 50
               then (* do something *)
               else (* do something else *)
   in
      ...
   end
```

### 5. Test the Flag

```bash
# Rebuild compiler
make compiler

# Test new flag
./build/bin/mpl -my-new-flag 200 test.mlb

# Check help message
./build/bin/mpl -help | grep my-new-flag
```

## Example: Adding an Optimization Threshold Flag

Here's a complete example of adding a threshold flag for an optimization:

### 1. Declaration (control-flags.sig)
```sml
val myOptThreshold: int ref
```

### 2. Implementation (control-flags.sml)
```sml
val myOptThreshold: int ref =
   control {name = "my-opt-threshold",
            default = 100,
            toString = Int.toString}
```

### 3. Command-Line Parsing (main.fun)
```sml
(Expert, "my-opt-threshold", " <n>",
 "threshold for my optimization decision, default = 100",
 Int (fn n =>
      if n < 0
         then usage "my-opt-threshold must be non-negative"
      else myOptThreshold := n)),
```

### 4. Usage (in optimization pass)
```sml
val threshold = !Control.myOptThreshold
val shouldOptimize = functionSize <= threshold
```

### 5. Testing
```bash
mpl -my-opt-threshold 150 program.mlb
mpl -diag-pass myPass program.mlb   # See optimization decisions
```

## Pass Management

### Control.translatePass

Runs a transformation pass with tracking and diagnostics:

```sml
val translatePass:
   {arg: 'a,
    doit: 'a -> 'b,
    keepIL: bool,
    name: string,
    srcToFile: ('a -> unit) option,
    tgtStats: ('b -> Layout.t) option,
    tgtToFile: ('b -> unit) option,
    tgtTypeCheck: ('b -> unit, bool option) option}
   -> 'b
```

**Parameters**:
- `arg`: Input to the pass
- `doit`: Transformation function
- `keepIL`: Whether to keep IR after this pass (controlled by `-keep-X` flags)
- `name`: Pass name (for diagnostics and `-keep-pass`)
- `srcToFile`: How to save source IR
- `tgtStats`: How to compute stats on result
- `tgtToFile`: How to save target IR
- `tgtTypeCheck`: Optional type checking function

**Example**:
```sml
val xml =
   Control.translatePass
   {arg = coreML,
    doit = Defunctorize.defunctorize,
    keepIL = false,
    name = "defunctorize",
    srcToFile = SOME CoreML.Program.toFile,
    tgtStats = SOME Xml.Program.layoutStats,
    tgtToFile = SOME Xml.Program.toFile,
    tgtTypeCheck = SOME (Xml.typeCheck, NONE)}
```

### Control.simplifyPass

Runs an optimization pass with shrinking:

```sml
val simplifyPass:
   {arg: 'a,
    doit: 'a -> 'a,
    execute: bool,
    forceTypeCheck: bool option,
    keepIL: bool,
    name: string,
    stats: 'a -> Layout.t,
    toFile: 'a -> unit,
    typeCheck: 'a -> unit}
   -> 'a
```

**Parameters**:
- `execute`: Whether to actually run the pass (controlled by `-enable-pass`/`-disable-pass`)
- `forceTypeCheck`: Override type checking decision
- Other parameters similar to translatePass

### Control.diagnostic

Output diagnostic information:

```sml
val diagnostic: (unit -> Layout.t) -> unit
```

**Usage**:
```sml
val () = Control.diagnostic
         (fn () => Layout.str "Inlined 5 functions")
```

**When it prints**:
- Only when `-diag-pass <passName>` matches current pass
- Controlled by `diagPasses` flag (regex matching)

## Common Flag Patterns

### Boolean Flag with Default

```sml
(* Signature *)
val myFeature: bool ref

(* Implementation *)
val myFeature: bool ref =
   control {name = "my-feature",
            default = false,
            toString = Bool.toString}

(* Command-line *)
(Normal, "my-feature", " {true|false}",
 "enable my feature",
 Bool (fn b => myFeature := b))

(* Usage *)
val enabled = !Control.myFeature
```

### Integer Flag with Validation

```sml
(* Signature *)
val threshold: int ref

(* Implementation *)
val threshold: int ref =
   control {name = "threshold",
            default = 100,
            toString = Int.toString}

(* Command-line with validation *)
(Expert, "threshold", " <n>",
 "size threshold for optimization, default = 100",
 Int (fn n =>
      if n < 0 orelse n > 1000
         then usage "threshold must be between 0 and 1000"
      else threshold := n))
```

### Enum Flag

```sml
(* Signature *)
structure Mode:
   sig
      datatype t = Fast | Small | Safe
      val toString: t -> string
      val fromString: string -> t option
   end
val mode: Mode.t ref

(* Implementation *)
val mode: Mode.t ref =
   control {name = "mode",
            default = Mode.Fast,
            toString = Mode.toString}

(* Command-line *)
(Normal, "mode", " {fast|small|safe}",
 "optimization mode",
 SpaceString (fn s =>
              case Mode.fromString s of
                 SOME m => mode := m
               | NONE => usage "invalid mode"))
```

### Record Flag (Multiple Related Options)

```sml
(* Signature *)
val inlineThreshold: {product: int, size: int} ref

(* Implementation *)
val inlineThreshold: {product: int, size: int} ref =
   control {name = "inline-threshold",
            default = {product = 320, size = 60},
            toString = fn {product, size} =>
                       concat ["(", Int.toString product, ", ",
                               Int.toString size, ")"]}

(* Command-line *)
(Expert, "inline-threshold", " (product, size)",
 "inlining thresholds",
 String (fn s => inlineThreshold := parseThreshold s))
```

## Diagnostic System

### Viewing Pass Diagnostics

Enable diagnostics for specific passes:

```bash
# Single pass
mpl -diag-pass inline program.mlb

# Multiple passes (regex)
mpl -diag-pass "inline|contify" program.mlb

# All passes
mpl -diag-pass ".*" program.mlb
```

### Adding Diagnostics to a Pass

In your pass implementation:

```sml
functor MyPass (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
   open S

   fun transform program =
      let
         val () = Control.diagnostic
                  (fn () => Layout.str "MyPass: starting optimization")

         (* Do optimization *)
         val numOptimized = ...

         val () = Control.diagnostic
                  (fn () => Layout.seq
                            [Layout.str "MyPass: optimized ",
                             Int.layout numOptimized,
                             Layout.str " functions"])
      in
         program
      end
end
```

**Benefits**:
- Track optimization effectiveness
- Debug pass behavior
- Profile compilation

### Example Diagnostic Output

```bash
$ mpl -diag-pass closureConvert test.mlb

closureConvert: analyzing function foo
  closure escapes: no
  size: 15

closureConvert: analyzing function bar
  closure escapes: yes
  size: 250
```

## Keeping Intermediate Files

### By IR Stage

```bash
mpl -keep-xml program.mlb       # Keep XML IR
mpl -keep-ssa program.mlb       # Keep SSA IR
mpl -keep-ssa2 program.mlb      # Keep SSA2 IR
mpl -keep-machine program.mlb   # Keep Machine IR
mpl -keep-g program.mlb         # Keep generated C files
```

### By Pass Name

```bash
mpl -keep-pass inline program.mlb
# Produces: program.inline.ssa

mpl -keep-pass constantPropagation program.mlb
# Produces: program.constantPropagation.ssa
```

### Keep All

```bash
mpl -keep-xml -keep-sxml -keep-ssa -keep-ssa2 -keep-rssa -keep-machine -keep-g program.mlb
```

## Error Handling

### Control.error

Report compilation error:

```sml
val error: Region.t * Layout.t * Layout.t -> unit
```

**Usage**:
```sml
Control.error (region, Layout.str "Type error",
               Layout.str "Expected int, got bool")
```

### Control.checkForErrors

Check if any errors occurred and abort if so:

```sml
val checkForErrors: unit -> unit
```

**Usage**:
```sml
val () = elaborate program
val () = Control.checkForErrors ()   (* Abort if elaboration had errors *)
```

## Common Issues

**Issue**: Flag not recognized
- **Solution**: Check spelling, check if flag is in `main.fun` options list
- **Debug**: Run `mpl -help` and grep for flag name

**Issue**: Flag has no effect
- **Solution**: Check that pass actually uses `!Control.flagName`
- **Debug**: Add diagnostic output to verify flag value

**Issue**: Pass not running with `-enable-pass`
- **Solution**: Check pass name matches exactly (case-sensitive)
- **Debug**: Use `-diag-pass` to see which passes run

**Issue**: Kept file not generated
- **Solution**: Check that pass actually runs and produces output
- **Debug**: Use `-keep-pass <passName>` with exact pass name

## See Also

- [control-flags.sig](control-flags.sig), [control-flags.sml](control-flags.sml) - Flag definitions
- [control.sig](control.sig), [control.sml](control.sml) - Control structure
- [../main/main.fun](../main/main.fun) - Command-line parsing
- [../README.md](../README.md) - Compiler overview
- [../ssa/README.md](../ssa/README.md) - SSA optimization passes
- [../atoms/README.md](../atoms/README.md) - Primitives and atoms

## Quick Reference

### Common Flags

```bash
# Optimization
-opt-passes {minimal|default}
-disable-pass <passName>
-enable-pass <passName>

# Diagnostics
-diag-pass <regex>
-keep-xml, -keep-ssa, -keep-ssa2
-keep-pass <passName>

# Debugging
-debug {true|false}
-debug-runtime {true|false}
-type-check {true|false}

# Code generation
-codegen c
-output <filename>

# Parallel/MPL
-spork-choose-threshold <n>
-detect-entanglement {true|false}

# Types
-default-type int64
-default-type word64

# Inlining
-inline-leaf-a "(<product>, <size>)"
-inline-non-rec "(<product>, <small>, <size>)"

# Elaboration
-default-ann <annotation>

# Output control
-stop {xml|ssa|ssa2|g}
-export-header <file>
```

### Help Commands

```bash
mpl -help                  # All options
mpl -expert-help           # Expert options only
mpl -help | grep <flag>    # Search for specific flag
```
