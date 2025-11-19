# MLton Abstract Syntax Tree (AST)

The Abstract Syntax Tree represents Standard ML source programs after parsing, before type elaboration.

## Overview

The AST is produced by the [front-end parser](../front-end/) and consumed by [elaboration](../elaborate/). It preserves the syntactic structure of source code, including:

- Infix operators (not yet resolved)
- Nested modules (structures, signatures, functors)
- Type annotations
- Pattern syntax
- Syntactic sugar (lists, record punning, etc.)

**Pipeline position**: Source ML → **AST** → Elaboration → CoreML

## Key Concepts

### AST Structure

The AST is organized into several layers:

**Core Language** ([ast-core.sig](ast-core.sig), [ast-core.fun](ast-core.fun)):
- Expressions (`Exp`)
- Patterns (`Pat`)
- Declarations (`Dec`)
- Types (`Type`)

**Modules** ([ast-modules.sig](ast-modules.sig), [ast-modules.fun](ast-modules.fun)):
- Structure expressions (`StrExp`)
- Signature expressions (`SigExp`)
- Functor expressions (`FunExp`)
- Module declarations (`ModDec`)

**Programs and MLBs** ([ast-programs.sig](ast-programs.sig), [ast-programs.fun](ast-programs.fun), [ast-mlbs.sig](ast-mlbs.sig), [ast-mlbs.fun](ast-mlbs.fun)):
- Top-level programs
- ML Basis file contents

**Atoms** ([ast-atoms.sig](ast-atoms.sig), [ast-atoms.fun](ast-atoms.fun)):
- Identifiers (variables, constructors, type names)
- Constants
- Fixity declarations

### Expressions

**Expression nodes** ([ast-core.sig](ast-core.sig)):

```sml
datatype Exp.node =
   Andalso of Exp * Exp
 | App of {func: Exp, arg: Exp, inline: InlineAttr, wasInfix: bool}
 | Case of Exp * Match
 | Const of Const
 | Constraint of Exp * Type
 | FlatApp of Exp vector * InlineAttr   (* not yet resolved *)
 | Fn of Match * InlineAttr
 | Handle of Exp * Match
 | If of Exp * Exp * Exp
 | Let of Dec * Exp
 | List of Exp vector
 | Orelse of Exp * Exp
 | Paren of Exp
 | Prim of PrimKind                     (* _prim declarations *)
 | Raise of Exp
 | Record of (Field * Region * Exp) vector
 | Seq of Exp vector
 | Tuple of Exp vector
 | Var of {fixop: Fixop, name: Longvid}
 | Vector of Exp vector
 | While of {test: Exp, expr: Exp}
```

**Key features**:
- **FlatApp**: Sequences like `f x y` (not yet resolved to nested applications)
- **wasInfix**: Tracks whether application was written infix
- **inline**: Inlining attributes from `_inline` annotations
- **Prim**: Primitive operations via `_prim` keyword
- **Fixop**: Tracks `op` keyword usage

### Patterns

**Pattern nodes** ([ast-core.sig](ast-core.sig)):

```sml
datatype Pat.node =
   App of {con: Longcon, arg: Pat, wasInfix: bool}
 | Const of Const
 | Constraint of Pat * Type
 | FlatApp of Pat vector
 | Layered of {constraint: Type option, fixop: Fixop,
               pat: Pat, var: Var}
 | List of Pat vector
 | Or of Pat vector
 | Paren of Pat
 | Record of {flexible: bool,
              items: (Field * Region * Item) vector}
 | Tuple of Pat vector
 | Var of {fixop: Fixop, name: Longvid}
 | Vector of Pat vector
 | Wild
```

**Pattern features**:
- **FlatApp**: Infix constructor applications (before precedence parsing)
- **Layered**: As-patterns (`x as p`)
- **Or**: Or-patterns (`p1 | p2`)
- **flexible**: Record patterns with `...` (e.g., `{a, ...}`)
- **wasInfix**: Tracks infix constructor syntax

**Record pattern items**:
```sml
datatype Pat.Item =
   Field of Pat
 | Vid of Vid * Type option * Pat option
```

Allows record punning: `{x}` means `{x = x}`

### Declarations

**Declaration nodes** ([ast-core.sig](ast-core.sig)):

```sml
datatype Dec.node =
   Abstype of {body: Dec, datBind: DatBind}
 | Datatype of {datatypes: DatBind, withtypes: TypBind}
 | Exception of Exn Bind
 | Fix of Fixity * Vid vector
 | Fun of {funs: {lambda: Match, var: Var} vector,
           tyvars: Tyvar vector}
 | Local of Dec * Dec
 | Open of Longstrid vector
 | Overload of Priority * Var * Type vector * Var vector
 | SeqDec of Dec vector
 | Type of TypBind
 | Val of {tyvars: Tyvar vector,
           valbinds: {exp: Exp, pat: Pat} vector,
           rvalbinds: {lambda: Match, var: Var} vector}
```

**Key features**:
- **Fun**: Function declarations (may have type variables)
- **Val**: Value bindings (separate recursive `rvalbinds` and non-recursive `valbinds`)
- **Fix**: Fixity declarations (`infix`, `infixr`, `nonfix`)
- **Datatype**: Algebraic datatype declarations
- **Overload**: Overloaded operators (extension)

### Types

**Type nodes** ([ast-core.sig](ast-core.sig)):

```sml
datatype Type.node =
   App of Tycon * Type vector
 | Con of Tycon
 | Paren of Type
 | Record of (Field * Region * Type) vector
 | Tuple of Type vector
 | Var of Tyvar
```

**Examples**:
```sml
int                → Con (Tycon "int")
'a list            → App (Tycon "list", [Var (Tyvar "a")])
int * bool         → Tuple [Con "int", Con "bool"]
{a: int, b: bool}  → Record [(Field "a", int), (Field "b", bool)]
```

### Modules

**Structure expressions** ([ast-modules.sig](ast-modules.sig)):

```sml
datatype StrExp.node =
   App of Fctid * StrExp              (* Functor application *)
 | Constrained of StrExp * SigExp     (* Structure ascription *)
 | Let of Dec * StrExp
 | Struct of Dec
 | Var of Longstrid
```

**Signature expressions**:

```sml
datatype SigExp.node =
   Spec of Spec
 | Var of Sigid
 | Where of SigExp * {tyvars: Tyvar vector,
                      longtycon: Longtycon,
                      ty: Type} vector
```

**Functor declarations**:

```sml
datatype FunctorDec =
   T of {arg: (Strid * SigExp) option,
         body: StrExp,
         name: Fctid,
         result: SigExp option}
```

### Identifiers

**Long identifiers** ([longid.sig](longid.sig), [longid.fun](longid.fun)):

```sml
(* Qualified paths *)
Longvid = A.B.C.x        (* value/variable *)
Longcon = A.B.C.Con      (* constructor *)
Longtycon = A.B.C.t      (* type constructor *)
Longstrid = A.B.C        (* structure *)
```

**Simple identifiers** ([ast-id.sig](ast-id.sig), [ast-id.fun](ast-id.fun)):

```sml
Vid    (* value identifier *)
Con    (* constructor *)
Tycon  (* type constructor *)
Tyvar  (* type variable *)
Var    (* variable *)
Strid  (* structure identifier *)
Sigid  (* signature identifier *)
Fctid  (* functor identifier *)
```

### Primitives

**PrimKind** ([ast-core.sig](ast-core.sig)) represents `_prim` declarations:

```sml
datatype PrimKind =
   Import of {attributes, name, ty}   (* _import *)
 | Export of {attributes, name, ty}   (* _export *)
 | Prim of {name, ty}                 (* _prim *)
 | Symbol of {attributes, name, ty}   (* _symbol *)
 | Address of {attributes, name, ty}  (* _address *)
 | Const of {name, ty}                (* _const *)
 | BuildConst of {name, ty}           (* _build_const *)
 | CommandLineConst of {name, ty, value} (* _command_line_const *)
```

**Attributes**:
- `ImportExportAttribute`: `cdecl`, `stdcall`, `impure`, `pure`, `inline`, etc.
- `SymbolAttribute`: `alloc`, `external`, `private`, `public`

### Fixity

**Fixity** ([ast-core.sig](ast-core.sig)) tracks operator precedence:

```sml
datatype Fixity =
   Infix of int option    (* Left-associative, optional precedence *)
 | Infixr of int option   (* Right-associative *)
 | Nonfix                 (* Not an operator *)
```

**Example**:
```sml
infix 6 + -
infixr 5 ::
infix 4 = <>
```

**FlatApp**: Before precedence parsing, infix applications are flat:
```sml
(* Source *)
a + b * c

(* FlatApp in AST *)
FlatApp [Var "a", Var "+", Var "b", Var "*", Var "c"]

(* After precedence parsing (in elaboration) *)
App (Var "+", Tuple [Var "a",
                     App (Var "*", Tuple [Var "b", Var "c"])])
```

## File Organization

| File | Lines | Purpose |
|------|-------|---------|
| [ast.sig](ast.sig) | ~20 | Main AST signature |
| [ast.fun](ast.fun) | ~10 | AST functor composition |
| [ast-core.sig](ast-core.sig) | ~400 | Core language AST |
| [ast-core.fun](ast-core.fun) | ~700 | Core language implementation |
| [ast-modules.sig](ast-modules.sig) | ~150 | Module system AST |
| [ast-modules.fun](ast-modules.fun) | ~450 | Module implementation |
| [ast-programs.sig](ast-programs.sig) | ~25 | Top-level programs |
| [ast-programs.fun](ast-programs.fun) | ~130 | Program implementation |
| [ast-mlbs.sig](ast-mlbs.sig) | ~60 | ML Basis files |
| [ast-mlbs.fun](ast-mlbs.fun) | ~140 | MLB implementation |
| [ast-atoms.sig](ast-atoms.sig) | ~200 | Identifiers and constants |
| [ast-atoms.fun](ast-atoms.fun) | ~450 | Atoms implementation |
| [ast-const.sig](ast-const.sig) | ~30 | Constants |
| [ast-const.fun](ast-const.fun) | ~25 | Constant implementation |
| [ast-id.sig](ast-id.sig) | ~30 | Simple identifiers |
| [ast-id.fun](ast-id.fun) | ~60 | Identifier implementation |
| [longid.sig](longid.sig) | ~35 | Long (qualified) identifiers |
| [longid.fun](longid.fun) | ~65 | Long identifier implementation |

## AST vs. CoreML

### Key Differences

**AST** (before elaboration):
- Infix operators not resolved (FlatApp)
- Types are syntactic (not checked)
- Modules present (structures, signatures, functors)
- Overloading not resolved
- No type inference results

**CoreML** (after elaboration):
- Infix resolved to nested applications
- All types checked and explicit
- Modules still present
- Overloading resolved
- Type variables bound

**Example**:
```sml
(* Source *)
val x = 1 + 2

(* AST *)
Val {
  pat = Var "x",
  exp = FlatApp [Const 1, Var "+", Const 2]
}

(* CoreML after elaboration *)
Val {
  var = x,
  ty = int,
  exp = PrimApp {
    prim = Int_add,
    args = [Const 1, Const 2]
  }
}
```

## Development Guide

### Understanding the AST

To understand the AST:

1. **Read core signature** ([ast-core.sig](ast-core.sig)) for expression/pattern/declaration structure
2. **Read modules signature** ([ast-modules.sig](ast-modules.sig)) for module system
3. **Examine parser output** - compile with `-keep ast` to see AST
4. **Trace through elaboration** to see how AST becomes CoreML

### Modifying the AST

**When adding syntax**:

1. **Extend lexer** ([front-end/ml.lex](../front-end/ml.lex)) with new tokens
2. **Extend parser** ([front-end/ml.grm](../front-end/ml.grm)) with new grammar
3. **Add AST node**: Extend appropriate `datatype node` in [ast-core.sig](ast-core.sig)
4. **Add layout**: Pretty-print new node in [ast-core.fun](ast-core.fun)
5. **Update elaboration**: Handle new node in [elaborate](../elaborate/)

**Example: Adding new expression form**:

```sml
(* 1. Add to ast-core.sig *)
datatype Exp.node =
   ...
 | MyNewExp of {field1: Type, field2: Exp}

(* 2. Add layout in ast-core.fun *)
fun layoutExp exp =
   case Exp.node exp of
      ...
    | MyNewExp {field1, field2} =>
        seq [str "mynew", tuple [Type.layout field1,
                                  layoutExp field2]]

(* 3. Handle in elaborate-core.fun *)
fun elaborateExp (exp: Ast.Exp.t) =
   case Ast.Exp.node exp of
      ...
    | Ast.Exp.MyNewExp {field1, field2} =>
        (* elaborate to CoreML *)
```

### Viewing AST Output

**Generate AST**:
```bash
# Not directly supported, but can view via elaboration diagnostics
mpl -diag-pass elaborate program.mlb
```

**Trace parsing**:
```bash
# Use parser directly (requires ML-Yacc debug mode)
# Usually debugging is done by examining elaboration errors
```

## Common Patterns

### Record Punning

**Source**:
```sml
{x, y}        (* punned *)
```

**AST**:
```sml
Record {
  items = [
    (Field "x", region, Pat.Item.Vid (Vid "x", NONE, NONE)),
    (Field "y", region, Pat.Item.Vid (Vid "y", NONE, NONE))
  ]
}

(* Expands to {x = x, y = y} during elaboration *)
```

### Infix Resolution

**Source**:
```sml
a + b * c
```

**AST (before resolution)**:
```sml
FlatApp [Var "a", Var "+", Var "b", Var "*", Var "c"]
```

**During elaboration**:
1. Look up fixity: `+ : infix 6`, `* : infix 7`
2. Higher precedence binds tighter: `*` before `+`
3. Resolve to `a + (b * c)`

**CoreML (after resolution)**:
```sml
App {
  func = Var "+",
  arg = Tuple [Var "a",
               App {func = Var "*",
                    arg = Tuple [Var "b", Var "c"]}]
}
```

### As-Patterns (Layered)

**Source**:
```sml
x as (y, z)
```

**AST**:
```sml
Layered {
  var = x,
  constraint = NONE,
  fixop = Fixop.None,
  pat = Tuple [Var "y", Var "z"]
}
```

## See Also

- [Front-end](../front-end/) - Parser that produces AST
- [Elaborate](../elaborate/) - Type inference that consumes AST
- [CoreML](../core-ml/) - Typed IR after elaboration

## References

- **Standard ML**: Definition of Standard ML (Revised) - syntax and semantics
- **Abstract Syntax Trees**: Classic compiler intermediate representation
- **ML-Yacc**: Parser generator used to build AST from parse trees
