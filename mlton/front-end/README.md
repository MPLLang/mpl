# MLton Front-End

The front-end is responsible for parsing Standard ML source code and ML Basis (MLB) files, transforming them into an Abstract Syntax Tree (AST) representation.

## Overview

The front-end is the entry point of the MLton compilation pipeline. It takes `.sml`, `.sig`, `.fun`, and `.mlb` files as input and produces an AST that preserves source location information for error reporting. The front-end handles:

- **Standard ML parsing**: Lexical analysis and syntactic analysis of SML source code
- **ML Basis parsing**: Parsing MLB project files that specify compilation units and dependencies
- **Source location tracking**: Maintaining accurate position information for error messages
- **Line directive handling**: Processing `(*#line ...)` directives for source mapping
- **Syntax validation**: Basic syntactic correctness checking

After parsing, the AST is passed to the [elaborate](../elaborate/) phase for type inference and module elaboration.

## Key Concepts

### Lexical Analysis (Lexing)

The lexer converts a stream of characters into a stream of tokens:

- **ML Lexer** ([ml.lex](ml.lex)): Tokenizes Standard ML source
  - Keywords: `val`, `fun`, `datatype`, `structure`, etc.
  - Identifiers and symbolic operators
  - Numeric and string literals (with extended constant support)
  - Comments (nested `(* ... *)` style)
  - Line directives for source position tracking

- **MLB Lexer** ([mlb.lex](mlb.lex)): Tokenizes ML Basis files
  - MLB keywords: `bas`, `basis`, `open`, `local`, `structure`, `signature`, `functor`
  - Path variables: `$(SML_LIB)`, `$(LIB_MLTON_DIR)`, etc.
  - File references with various extensions (`.sml`, `.sig`, `.fun`, `.mlb`)
  - Annotations: `ann "option" in ... end`

Both lexers are specified using ML-Lex syntax and generate SML code that implements the tokenization logic.

### Syntactic Analysis (Parsing)

The parser constructs an AST from the token stream using LR parsing:

- **ML Parser** ([ml.grm](ml.grm)): Parses Standard ML syntax
  - Expression grammar: applications, let-bindings, case expressions, etc.
  - Declaration grammar: value bindings, datatype declarations, structures, signatures
  - Pattern matching syntax
  - Module system: structures, signatures, functors
  - Infix operator handling with precedence

- **MLB Parser** ([mlb.grm](mlb.grm)): Parses ML Basis syntax
  - Basis declarations: imports, local scoping, annotations
  - Module bindings: structure/signature/functor aliases
  - Basis expressions: composition of basis libraries

Both parsers are specified using ML-Yacc (mlyacc) syntax and generate SML code that implements the parsing logic.

### Abstract Syntax Tree (AST)

The AST preserves the structure of the source program with region information:

- **Core AST** ([ast-core](../ast/ast-core.sig)): Expressions, patterns, types, declarations
- **Module AST** ([ast-modules](../ast/ast-modules.sig)): Structures, signatures, functors
- **MLB AST** ([ast-mlbs](../ast/ast-mlbs.sig)): Basis declarations and expressions
- **Programs** ([ast-programs](../ast/ast-programs.sig)): Top-level program structure

Each AST node carries a `Region.t` indicating its source location (`left` and `right` positions).

### ML Basis Files

MLB files organize multi-file SML projects:

```sml
(* Example MLB file *)
local
   $(SML_LIB)/basis/basis.mlb
   $(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
in
   structure MyLib
   foo.sml
   bar.sig
   bar.sml
end
```

**Key features**:
- **Path variables**: `$(VAR)` expands to configured paths
- **Local scoping**: `local ... in ... end` controls visibility
- **Selective export**: Only explicitly listed structures/signatures/functors are exported
- **Annotations**: `ann "option" in ... end` applies compiler options to specific files
- **Recursive imports**: MLB files can reference other MLB files (with cycle detection)

## File Organization

### Core Front-End Files

| File | Lines | Purpose |
|------|-------|---------|
| [front-end.sig](front-end.sig) | ~20 | ML source front-end signature |
| [front-end.fun](front-end.fun) | ~70 | ML source front-end implementation |
| [mlb-front-end.sig](mlb-front-end.sig) | ~22 | MLB front-end signature |
| [mlb-front-end.fun](mlb-front-end.fun) | ~300 | MLB front-end with path resolution |

### Lexer Specifications

| File | Lines | Purpose |
|------|-------|---------|
| [ml.lex](ml.lex) | ~800 | ML-Lex specification for Standard ML |
| [mlb.lex](mlb.lex) | ~400 | ML-Lex specification for ML Basis files |

**Generated files** (from ML-Lex):
- `ml.lex.sml` (~22,000 lines) - Generated ML lexer
- `mlb.lex.sml` (~5,000 lines) - Generated MLB lexer

### Parser Specifications

| File | Lines | Purpose |
|------|-------|---------|
| [ml.grm](ml.grm) | ~1,700 | ML-Yacc grammar for Standard ML |
| [mlb.grm](mlb.grm) | ~200 | ML-Yacc grammar for ML Basis files |

**Generated files** (from ML-Yacc):
- `ml.grm.sml` (~8,000 lines) - Generated ML parser
- `ml.grm.sig` (~150 lines) - Parser signature
- `mlb.grm.sml` (~1,000 lines) - Generated MLB parser
- `mlb.grm.sig` (~40 lines) - Parser signature

### Build System

- [sources.mlb](sources.mlb) - ML Basis file for building the front-end
- [sources.cm](sources.cm) - CM file (legacy)

## Lexing and Parsing Workflow

### Standard ML Source Files

**Entry point**: `FrontEnd.lexAndParseFile: File.t -> Ast.Program.t`

**Process**:
1. **Open file**: Create input stream for the source file
2. **Create lexer**: `MLLexFun` generates token stream from input
3. **Create parser**: `MLLrValsFun` and `JoinWithArg` create LR parser
4. **Parse**: LR parser with 30-token lookahead produces AST
5. **Syntax check**: `Ast.Program.checkSyntax` validates the AST
6. **Optional output**: If `-keep ast` flag is set, write AST to `.ast` file
7. **Return**: `Ast.Program.t` containing the parsed program

**Error handling**:
- Lexical errors: Reported with source position (invalid characters, unclosed strings, etc.)
- Parse errors: Reported with region information (syntax errors)
- Recovery: On parse failure, returns empty program after reporting error

### ML Basis Files

**Entry point**: `MLBFrontEnd.lexAndParseString: String.t -> Ast.Basdec.t`

**Process**:
1. **Tokenize**: MLB lexer processes the input string
2. **Parse**: MLB parser constructs basis declaration AST
3. **Path expansion**: Resolve `$(VAR)` path variables using `mlb-path-map`
4. **File references**: Recursively parse referenced `.sml` and `.mlb` files
5. **Cycle detection**: Track visited files to prevent infinite recursion
6. **Promise-based loading**: Use promises for lazy evaluation of file contents
7. **Return**: `Ast.Basdec.t` representing the complete basis

**Path resolution**:
- **Path variables**: Configured via `-mlb-path-var` or `-mlb-path-map` flags
- **Variable expansion**: `$(VAR)` → configured directory path
- **Relative paths**: Resolved relative to MLB file's directory
- **Absolute paths**: Used as-is (when `-prefer-abs-paths` is true)
- **Canonicalization**: Paths are normalized to canonical form

**MLB-specific features**:
- **Recursive parsing**: When lexer encounters file reference, it invokes parser recursively
- **File type detection**: Extension determines whether to parse as `.sml` or `.mlb`
- **Deduplication**: Same file parsed only once (cached via hash table)
- **Error recovery**: Invalid paths or cyclic imports reported with region info

## Lexer Details

### ML Lexer Features

**Numeric constants**:
- Decimal integers: `123`, `~456`
- Hexadecimal: `0x1A`, `0w1A` (word)
- Binary: `0b1010` (when `-allow-extended-num-consts true`)
- Reals: `1.23`, `1.23e10`, `1.23E~10`

**String constants**:
- Standard escape sequences: `\n`, `\t`, `\\`, `\"`
- Character codes: `\065` (decimal), `\x41` (hex)
- Unicode escapes: `\u0041`, `\U00000041` (when `-allow-extended-text-consts true`)
- Gap characters: `\  \` (whitespace in strings)

**Comments**:
- Nested block comments: `(* ... (* nested *) ... *)`
- Line comments: `// comment` (when `-allow-line-comments true`)
- Comment error tracking: Unclosed comments reported at end of file

**Line directives**:
- Format: `(*#line line:col "file"*)`
- Purpose: Map generated code back to original source locations
- Effect: Adjusts `Source.t` position tracking for subsequent tokens

**Identifiers**:
- Alphanumeric: `foo`, `bar'`, `x123`
- Symbolic: `+`, `::`, `*>>=`, etc.
- Prime suffixes: `x'`, `x''`, etc.

### MLB Lexer Features

**Keywords**:
- `bas`, `basis`, `open`, `local`, `in`, `end`
- `structure`, `signature`, `functor`
- `ann` (for annotations)
- `prim` (primitive basis)

**Path variables**:
- Syntax: `$(VARIABLE_NAME)`
- Example: `$(SML_LIB)/basis/basis.mlb`
- Expansion: Replaced with configured directory path

**File references**:
- Recognized extensions: `.mlb`, `.sml`, `.sig`, `.fun`, `.ML`
- Absolute or relative paths
- Quoted strings for paths with spaces

## Parser Details

### ML Parser Structure

The ML grammar follows the Standard ML Definition with some extensions:

**Expression grammar** (simplified):
```
exp ::= atexp                          (* atomic expression *)
     |  exp atexp                      (* application *)
     |  exp : ty                       (* type constraint *)
     |  exp andalso exp                (* short-circuit and *)
     |  exp orelse exp                 (* short-circuit or *)
     |  raise exp                      (* exception raising *)
     |  fn match                       (* anonymous function *)
     |  case exp of match              (* pattern matching *)
     |  if exp then exp else exp       (* conditional *)

atexp ::= const                        (* constants *)
       |  longvid                      (* variables *)
       |  { row }                      (* records *)
       |  let dec in exp end           (* local bindings *)
       |  ( exp )                      (* parentheses *)
```

**Declaration grammar**:
```
dec ::= val valbind                    (* value bindings *)
     |  fun fvalbind                   (* function bindings *)
     |  datatype datbind               (* datatype declarations *)
     |  type typbind                   (* type aliases *)
     |  exception exnbind              (* exception declarations *)
     |  local dec in dec end           (* local declarations *)
     |  structure strbind              (* structure bindings *)
     |  signature sigbind              (* signature bindings *)
```

**Extensions**:
- **Optional bar**: Allow `|` before first case in pattern match (when `-allow-opt-bar true`)
- **Optional semicolon**: Allow trailing `;` in sequences (when `-allow-opt-semicolon true`)
- **Record punning**: Allow `{x, y}` for `{x = x, y = y}` in expressions (when `-allow-record-pun-exps true`)

### MLB Parser Structure

**Basis declaration grammar**:
```
basdec ::= basis basbind               (* basis bindings *)
        |  open basid ...              (* open basis *)
        |  local basdec in basdec end  (* local scope *)
        |  structure strbind           (* structure aliases *)
        |  signature sigbind           (* signature aliases *)
        |  functor fctbind             (* functor aliases *)
        |  ann "..." in basdec end     (* annotations *)
        |  file.mlb                    (* MLB import *)
        |  file.sml                    (* SML import *)
        |  basdec ; basdec             (* sequence *)
```

**Basis expression grammar**:
```
basexp ::= bas basdec end              (* basis definition *)
        |  basid                       (* basis identifier *)
        |  let basdec in basexp end    (* local basis *)
```

## Common Elaboration Flags

The lexer and parser respect several elaboration flags from [Control.Elaborate](../control/):

| Flag | Default | Purpose |
|------|---------|---------|
| `allowLineComments` | `false` | Enable `//` comments |
| `allowExtendedNumConsts` | `false` | Enable binary literals (`0b...`) |
| `allowExtendedTextConsts` | `false` | Enable Unicode escapes (`\u...`) |
| `allowOptBar` | `false` | Allow `|` before first pattern |
| `allowOptSemicolon` | `true` | Allow trailing `;` in sequences |
| `allowRecordPunExps` | `false` | Enable record punning `{x}` |

**Setting flags**:
```bash
# Enable extended features
mpl -default-ann 'allowLineComments true' \
    -default-ann 'allowExtendedNumConsts true' \
    foo.mlb
```

## Integration with Elaboration

The front-end produces an AST that is consumed by the [elaborate](../elaborate/) phase:

**Data flow**:
1. **Front-end output**: `Ast.Program.t` or `Ast.Basdec.t`
2. **Elaboration input**: Processes AST with type environment
3. **Elaboration output**: [CoreML](../core-ml/) IR with explicit types

**Key transformations in elaboration**:
- Type inference: Infer types for all expressions
- Overload resolution: Resolve polymorphic operators like `+`, `*`
- Module elaboration: Expand structures, signatures, functors
- Scope resolution: Bind variables to definitions
- Pattern compilation: Prepare for [match-compile](../match-compile/)

## Error Reporting

The front-end maintains precise source location information:

**Region tracking**:
- Each AST node has a `Region.t` indicating source span
- Regions include `left` and `right` `SourcePos.t` positions
- Used for error messages in elaboration and later phases

**Error categories**:
- **Lexical errors**: Invalid characters, malformed constants, unclosed strings/comments
- **Parse errors**: Syntax errors (missing keywords, unexpected tokens)
- **MLB errors**: Undefined path variables, cyclic imports, missing files

**Example error**:
```
Error: foo.sml 12.15-12.20
  Syntax error: expected 'in' but found 'end'
```

## Development Guide

### Modifying the Lexer

To add new lexical features (e.g., new literal syntax):

1. **Edit lexer specification**: Modify [ml.lex](ml.lex) or [mlb.lex](mlb.lex)
   - Add new regular expressions for token patterns
   - Add actions to generate tokens
   - Update token declarations

2. **Update token definitions**: Modify grammar file's `%term` section
   - Add new token types in [ml.grm](ml.grm) or [mlb.grm](mlb.grm)

3. **Regenerate lexer**: ML-Lex will regenerate `.lex.sml` on next build
   ```bash
   make compiler
   ```

4. **Test thoroughly**: Ensure backward compatibility and edge cases

**Example** (adding a new token):
```sml
(* In ml.lex *)
<INITIAL> "newkeyword" => (tok (Tokens.NEWKEYWORD, yytext, source, yypos));

(* In ml.grm *)
%term ... | NEWKEYWORD
```

### Modifying the Parser

To add new syntactic constructs:

1. **Edit grammar specification**: Modify [ml.grm](ml.grm) or [mlb.grm](mlb.grm)
   - Add new non-terminals to `%nonterm` section
   - Add production rules with semantic actions
   - Update precedence/associativity if needed

2. **Update AST structures**: Modify [ast](../ast/) to represent new constructs
   - Add constructors to relevant datatypes
   - Implement layout/pretty-printing functions

3. **Regenerate parser**: ML-Yacc will regenerate `.grm.sml` on next build
   ```bash
   make compiler
   ```

4. **Update elaboration**: Modify [elaborate](../elaborate/) to handle new AST nodes

**Example** (adding new expression syntax):
```sml
(* In ml.grm *)
%nonterm myexp of Ast.Exp.t

%%

myexp : KEYWORD exp END
        => (Ast.Exp.makeRegion'
            (Ast.Exp.MyNew exp, KEYWORDleft, ENDright))

exp : ... | myexp => (myexp)
```

### Adding MLB Path Variables

Path variables are configured at compile time:

**Via command line**:
```bash
mpl -mlb-path-var 'MYLIB /path/to/mylib' foo.mlb
```

**Via MLB path map file**:
```bash
mpl -mlb-path-map mypath.map foo.mlb
```

**Path map format**:
```
MYLIB /path/to/mylib
OTHER_LIB /other/path
```

**Using in MLB files**:
```sml
$(MYLIB)/mylib.mlb
```

### Debugging Lexer/Parser Issues

**Enable tracing**:
```bash
# Keep AST output for inspection
mpl -keep ast foo.mlb

# Examine AST
cat foo.mlb.ast
```

**Common issues**:
- **Shift/reduce conflicts**: Check grammar ambiguity, add precedence declarations
- **Reduce/reduce conflicts**: Restructure grammar to eliminate ambiguity
- **Lexer state errors**: Ensure proper state transitions in `.lex` file
- **Region bogus errors**: Check that all AST constructors propagate regions

**Useful tools**:
- ML-Yacc verbose output: Check for conflicts in generated parser
- Manual testing: Write small test cases that exercise new features

## Performance Considerations

**Lexer performance**:
- Lexer is generally fast (linear in input size)
- Nested comment tracking adds minimal overhead
- String processing is most expensive (escape handling)

**Parser performance**:
- LR parsing is efficient (linear in typical cases)
- Lookahead is limited to 30 tokens
- Grammar ambiguities can cause exponential blowup (avoided in MLton grammar)

**MLB processing**:
- File caching prevents redundant parsing
- Promise-based lazy evaluation defers work
- Path resolution is linear in path map size

## Examples

### Example: Parsing a Simple SML File

**Input file** (`hello.sml`):
```sml
val () = print "Hello, world!\n"
```

**Parsing**:
```sml
val ast = FrontEnd.lexAndParseFile "hello.sml"
(* Returns: Ast.Program.T [Dec.Val (...)] *)
```

### Example: Parsing an MLB File

**Input file** (`hello.mlb`):
```sml
$(SML_LIB)/basis/basis.mlb
hello.sml
```

**Parsing**:
```sml
val basdec = MLBFrontEnd.lexAndParseString (File.read "hello.mlb")
(* Returns: Ast.Basdec.Seq [
     Ast.Basdec.MLB (...),  (* basis.mlb *)
     Ast.Basdec.Prog (...)  (* hello.sml *)
   ]
*)
```

### Example: Line Directives

**Input** (`generated.sml`):
```sml
(*#line 1:1 "original.sml"*)
val x = 42
val y = "error here"  (* error reports original.sml:3 *)
(*#line 10:1 "generated.sml"*)
val z = true
```

Line directives map locations in generated code back to original sources.

## Common Issues

### Issue: Parse Error with No Specific Location

**Symptom**: Error reports bogus location or end-of-file

**Cause**: Parser reached EOF unexpectedly (unclosed `let`, `struct`, etc.)

**Fix**: Check for missing `end` keywords, unmatched parentheses

### Issue: Lexical Error in String Constant

**Symptom**: "Invalid escape sequence" or "Unclosed string"

**Cause**: Malformed escape sequences, missing closing quote

**Fix**:
- Use `\\` for backslash, `\"` for quote
- Enable `-allow-extended-text-consts` for Unicode escapes

### Issue: MLB Path Variable Undefined

**Symptom**: "Undefined MLB path variable: FOO"

**Cause**: Path variable not configured

**Fix**:
```bash
mpl -mlb-path-var 'FOO /path/to/foo' program.mlb
```

### Issue: Cyclic MLB Import

**Symptom**: "Basis forms a cycle with file.mlb"

**Cause**: MLB file imports itself directly or indirectly

**Fix**: Restructure MLB files to eliminate circular dependencies

### Issue: Shift/Reduce Conflict in Modified Grammar

**Symptom**: Parser behaves unexpectedly, ambiguous parses

**Cause**: Grammar has multiple valid parse trees for some input

**Fix**: Add precedence declarations in `%left`, `%right`, or restructure grammar

## See Also

- [AST Structures](../ast/) - Abstract syntax tree definitions
- [Elaborate Phase](../elaborate/) - Type inference and module elaboration
- [Control System](../control/) - Compiler flags and elaboration options
- [Main Compilation Driver](../main/) - Integration of front-end with pipeline
- [ML Basis System](../../doc/mlb.txt) - ML Basis file format reference (if available)

## References

- **Standard ML Definition** (Revised): Formal specification of SML syntax and semantics
- **ML-Lex User's Guide**: Lexer generator documentation
- **ML-Yacc User's Guide**: Parser generator documentation
- **SML/NJ Sources**: Original lexer/parser heavily modified for MLton
