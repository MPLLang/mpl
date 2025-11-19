# MLton Match Compilation

Match compilation transforms complex pattern matching into efficient decision trees.

## Overview

Match compilation is used by [defunctorization](../defunctorize/) to compile SML pattern matching. It takes nested patterns and generates an efficient decision tree that tests the minimum necessary information to determine which case matches.

**Key transformations**:
- **Pattern analysis**: Understand pattern structure (constructors, constants, records, vectors)
- **Decision tree generation**: Build efficient testing order
- **Exhaustiveness checking**: Detect missing patterns (non-exhaustive matches)
- **Redundancy detection**: Find unreachable patterns
- **Counterexample generation**: Provide example values for missing cases

**Used by**: Defunctorization for compiling `case` expressions

## Key Concepts

### Pattern Representation

**NestedPat** ([nested-pat.sig](nested-pat.sig), [nested-pat.fun](nested-pat.fun)) represents patterns:

```sml
datatype NestedPat.t = T of {pat: node, ty: Type.t}
and node =
   Con of {arg: t option, con: Con, targs: Type vector}
 | Const of {const: Const, isChar: bool, isInt: bool}
 | Layered of Var * t
 | Or of t vector
 | Record of t SortedRecord
 | Var of Var
 | Vector of t vector
 | Wild
```

**Pattern types**:
- **Con**: Constructor pattern (`SOME x`, `x::xs`)
- **Const**: Constant pattern (`42`, `#"a"`, `"hello"`)
- **Layered**: As-pattern (`x as SOME y`)
- **Or**: Alternative patterns (`0 | 1 | 2`)
- **Record**: Record/tuple pattern (`{a, b}`, `(x, y)`)
- **Var**: Variable binding (`x`)
- **Vector**: Vector pattern (`#[a, b, c]`)
- **Wild**: Wildcard (`_`)

### Decision Trees

**Match compilation algorithm** builds a decision tree:

```sml
(* Example pattern match *)
case x of
  (SOME 0, true) => 1
| (SOME _, false) => 2
| (NONE, _) => 3

(* Decision tree *)
case x of
  (tuple_arg) =>
    let val opt = #1 tuple_arg
        val bool = #2 tuple_arg
    in
      case opt of
        SOME val_arg =>
          case val_arg of
            0 => if bool then 1 else 2
          | _ => if bool then <error> else 2
      | NONE => 3
    end
```

**Tree structure**:
1. **Test nodes**: Switch on constructor, constant, or field
2. **Leaf nodes**: Expression to evaluate when pattern matches
3. **Failure nodes**: What to do if no pattern matches

### Algorithm

**matchCompile** ([match-compile.fun](match-compile.fun)) generates decision trees:

**Input**:
```sml
val matchCompile: {
   caseType: Type,                    (* result type *)
   cases: (NestedPat * (int -> (Var -> Var) -> Exp)) vector,
   conTycon: Con -> Tycon,           (* constructor → tycon *)
   test: Var,                         (* variable being matched *)
   testType: Type,                    (* type of test *)
   tyconCons: Tycon -> {con: Con, hasArg: bool} vector
} -> Exp * ({dropOnlyExns: bool} -> Layout option)
```

**Output**:
- **Exp**: Decision tree expression
- **Layout option**: Counterexamples for non-exhaustive matches (or NONE if exhaustive)

**Process**:

1. **Flatten patterns**: Convert nested patterns to canonical form
2. **Build rules matrix**: Organize patterns into testable structure
3. **Choose test**: Pick best variable/field to test first
4. **Split by cases**: Partition rules by constructor/constant tested
5. **Recurse**: Apply algorithm to each partition
6. **Build tree**: Combine results into decision tree

### Test Selection

**Heuristics for choosing which test to perform first**:

1. **Constructor tests**: Test constructors (datatype) before constants
2. **Complete sets**: Prefer tests that partition into complete cases
3. **Shallow tests**: Test top-level patterns before nested
4. **Fewest alternatives**: Test patterns with fewer cases first

**Example**:
```sml
case (x, y) of
  (0, SOME _) => ...    (* 4 cases: x ∈ {0,other}, y ∈ {SOME, NONE} *)
| (_, NONE) => ...

(* Better to test y first (2 cases: SOME, NONE) *)
(* Than to test x first (many cases: 0, 1, 2, ...) *)
```

### Exhaustiveness Checking

**Exhaustiveness** ensures all possible values are covered:

**Exhaustive** (good):
```sml
case opt of
  SOME x => x
| NONE => 0
```

**Non-exhaustive** (warning):
```sml
case opt of
  SOME x => x
(* Missing: NONE *)
```

**Checking process**:
1. Track which constructors/constants are tested in each branch
2. Compute which values remain untested
3. Generate counterexamples for missing cases

**Counterexamples** ([match-compile.fun](match-compile.fun)):
```sml
datatype Example =
   ConApp of {arg: Example option, con: Con}
 | ConstRange of {lo: Const option, hi: Const option, isChar: bool, isInt: bool}
 | Exn
 | Or of Example vector
 | Record of Example SortedRecord
 | Vector of Example vector * {dots: bool}
 | Wild
```

**Example counterexample**:
```sml
(* Pattern match *)
fun f (SOME 0) = 1

(* Compiler warning *)
Warning: match non-exhaustive
  SOME 1 => ...     (* counterexample: any non-zero *)
  SOME ~1 => ...    (* another counterexample *)
  NONE => ...       (* another counterexample *)
```

### Redundancy Detection

**Redundant patterns** are unreachable (already covered by earlier patterns):

```sml
case x of
  SOME _ => 1
| SOME 0 => 2    (* redundant! SOME 0 already matched by SOME _ *)
| NONE => 3
```

**Detection**:
- Track which patterns have been tested
- Mark pattern as redundant if all of its cases are covered by earlier patterns
- Report redundancy with pattern location

### Or Patterns

**Or patterns** (`p1 | p2 | p3`) match if any alternative matches:

```sml
case x of
  (0 | 1 | 2) => "small"
| n => "large"
```

**Compilation strategy**:
- Expand or-patterns to separate rules
- Share common right-hand side
- Detect if or-pattern arms bind different variables (illegal)

**Expansion**:
```sml
(* Source *)
case x of
  (0 | 1) => "zero or one"

(* Expanded *)
case x of
  0 => "zero or one"
| 1 => "zero or one"
```

### Layered Patterns

**Layered patterns** (`x as p`) bind variable and match nested pattern:

```sml
case xs of
  (ys as y::_) => (y, ys)
```

**Compilation**:
1. Match nested pattern `p`
2. Bind variable `x` to the same value
3. Proceed to right-hand side with both bindings

## File Organization

| File | Lines | Purpose |
|------|-------|---------|
| [match-compile.sig](match-compile.sig) | ~70 | Match compilation signature |
| [match-compile.fun](match-compile.fun) | ~1,200 | Decision tree generation |
| [nested-pat.sig](nested-pat.sig) | ~60 | Nested pattern signature |
| [nested-pat.fun](nested-pat.fun) | ~150 | Nested pattern implementation |

## Algorithm Details

### Pattern Matrix

**Rules are organized as a matrix**:

```
Patterns          | Expression
------------------+------------
p1_1 p1_2 ... p1_n | e1
p2_1 p2_2 ... p2_n | e2
...                | ...
pm_1 pm_2 ... pm_n | em
```

Each row is a rule, each column is a tested value position.

**Example**:
```sml
case (x, y, z) of
  (0, SOME a, _) => a
| (1, NONE, b) => b
| (_, SOME c, d) => c + d

(* Matrix *)
(*  x    y        z   | expr *)
    0    SOME a   _   | a
    1    NONE     b   | b
    _    SOME c   d   | c + d
```

### Heuristic: Constructor Arity

**Choose test that partitions into most cases**:

```sml
(* Many integer constants vs. few constructors *)
case (x, opt) of
  (0, SOME _) => ...
| (1, NONE) => ...
| (2, SOME _) => ...
| (3, NONE) => ...

(* Better to test opt first (2 cases) than x (many cases) *)
```

**Partition by constructor**:
- Bool: 2 cases (true, false)
- Option: 2 cases (SOME, NONE)
- List: 2 cases (::, nil)
- Custom datatypes: N cases (one per constructor)

### Decision Tree Construction

**Process** ([match-compile.fun](match-compile.fun)):

```sml
fun matchCompile (rules, test, testType) =
   if allRulesHaveWildInColumn then
      (* All wild in first column: proceed to next column *)
      matchCompile (advanceColumn rules, nextTest, nextType)
   else if allRulesHaveSameConstructor then
      (* All same constructor: descend into argument *)
      let val arg = extractConstructorArg test
      in matchCompile (descendIntoArg rules, arg, argType)
      end
   else
      (* Split by constructor *)
      let val constructors = getAllConstructors testType
          val branches =
             List.map (constructors, fn con =>
                let val matching = filterRulesByConstructor (rules, con)
                in (con, matchCompile (matching, arg, argType))
                end)
      in
         Exp.casee {
            test = test,
            cases = branches,
            default = computeDefault ()
         }
      end
```

### Optimization: Column Reordering

**Reorder columns** to minimize decision tree depth:

```sml
(* Poor ordering: test x first (many cases) *)
case (x, opt) of
  (0, _) => ...
| (1, _) => ...
| ...  (* 100 more integer cases *)
| (_, SOME a) => ...
| (_, NONE) => ...

(* Better: test opt first (2 cases), then x *)
case opt of
  SOME a => ...
| NONE =>
    case x of
      0 => ... | 1 => ... | ...
```

## Examples

### Example: Simple Datatype

**Source**:
```sml
case opt of
  SOME x => x + 1
| NONE => 0
```

**Decision tree**:
```sml
case opt of
  SOME x_arg =>
    let val x = x_arg
    in x + 1 end
| NONE => 0
```

### Example: Nested Pattern

**Source**:
```sml
case tree of
  Leaf x => x
| Node (Leaf a, Leaf b) => a + b
| Node (l, r) => f (l, r)
```

**Decision tree**:
```sml
case tree of
  Leaf x_arg =>
    let val x = x_arg
    in x end
| Node tuple_arg =>
    let val l = #1 tuple_arg
        val r = #2 tuple_arg
    in
      case l of
        Leaf a_arg =>
          (case r of
             Leaf b_arg =>
               let val a = a_arg
                   val b = b_arg
               in a + b end
           | Node _ => f (l, r))
      | Node _ => f (l, r)
    end
```

### Example: Or Pattern

**Source**:
```sml
case x of
  (0 | 1 | 2) => "small"
| 3 => "three"
| _ => "large"
```

**Decision tree**:
```sml
case x of
  0 => "small"
| 1 => "small"
| 2 => "small"
| 3 => "three"
| _ => "large"
```

### Example: Non-exhaustive Match

**Source**:
```sml
fun head (x::xs) = x
```

**Warning**:
```
Warning: match non-exhaustive
  nil => ...
```

**Generated decision tree** (with error for nil):
```sml
fn list_arg =>
   case list_arg of
     x::xs => x
   | nil => raise Match
```

### Example: Redundant Pattern

**Source**:
```sml
case x of
  SOME 0 => "zero"
| SOME _ => "other"
| SOME 1 => "one"    (* redundant! *)
| NONE => "none"
```

**Warning**:
```
Warning: match redundant
  SOME 1 => ...
```

## Development Guide

### Understanding Match Compilation

To understand match compilation:

1. **Read signatures**: [match-compile.sig](match-compile.sig), [nested-pat.sig](nested-pat.sig)
2. **Study algorithm**: Read [match-compile.fun](match-compile.fun) top-down
3. **Trace examples**: Follow simple cases through the algorithm
4. **Examine counterexamples**: Understand how exhaustiveness checking works

**Key insight**: Match compilation is about finding an efficient way to ask yes/no questions (constructor tests, constant comparisons) to determine which case to execute.

### Modifying Match Compilation

**When changing pattern syntax**:
1. Add new `NestedPat.node` constructor
2. Update `flatten`, `isRefutable`, `removeVars`, etc.
3. Add compilation case in [match-compile.fun](match-compile.fun)
4. Update counterexample generation

**When improving efficiency**:
1. Modify test selection heuristics
2. Add new optimizations (e.g., jump table for dense integer ranges)
3. Improve column reordering
4. Add special cases for common patterns

**When improving diagnostics**:
1. Enhance counterexample generation
2. Add more specific redundancy messages
3. Improve layout of examples

### Debugging Match Compilation

**Trace pattern compilation**:
```bash
# Defunctorization will show pattern compilation
mpl -diag-pass defunctorize program.mlb
```

**Common issues**:

**Issue: Non-exhaustiveness false positive**

**Symptom**: Compiler claims pattern is non-exhaustive when it appears complete

**Cause**: Pattern match analysis doesn't understand invariant (e.g., integer range)

**Fix**: Add wildcard case or refactor to make exhaustiveness obvious

**Issue: Redundancy false negative**

**Symptom**: Pattern is redundant but no warning

**Cause**: Or-patterns or complex nesting confuses analysis

**Fix**: Simplify patterns or reorder cases

**Issue: Poor decision tree**

**Symptom**: Generated code tests same value multiple times

**Cause**: Heuristics chose poor test order

**Fix**: Improve test selection heuristics in [match-compile.fun](match-compile.fun)

## Performance Considerations

**Decision tree size**:
- Or-patterns expand to multiple cases (code duplication)
- Deep nesting creates deep trees (more testing)
- Many cases create large trees (more branches)

**Test ordering**:
- Good ordering → shallow tree, fast execution
- Poor ordering → deep tree, redundant tests
- Heuristics are crucial for efficiency

**Exhaustiveness checking**:
- Must analyze all possible values
- Can be slow for large pattern matches
- Caching and memoization help

## See Also

- [Defunctorize](../defunctorize/) - Uses match compilation for pattern compilation
- [Elaborate](../elaborate/) - Checks pattern exhaustiveness during type checking
- [CoreML IR](../core-ml/) - Source patterns before defunctorization

## References

- **Pattern Matching**: Classic compiler transformation
- **Decision Trees**: Efficient representation for multi-way branching
- **Exhaustiveness Checking**: Ensures pattern matches are complete
- **Redundancy Analysis**: Detects unreachable code
