# 1ML TOY INTERPRETER [![Gitter](https://badges.gitter.im/1ml-prime/community.svg)](https://gitter.im/1ml-prime/community) [![Build Status](https://travis-ci.org/1ml-prime/1ml.svg?branch=1ml-prime)](https://travis-ci.org/1ml-prime/1ml)

(c) 2014 Andreas Rossberg <rossberg@mpi-sws.org>

## Introduction

This provides a simple proof-of-concept interpreter for a small version of 1ML,
an ML dialect based on first-class modules, as described in the paper

<dl>
  <dt><a href="https://people.mpi-sws.org/~rossberg/1ml/">1ML - core and modules as one (F-ing first-class modules)</a></dt>
  <dd>(submitted to ICFP 2015)</dd>
</dl>

The interpreter has been written mainly for the purpose of experimenting with
1ML type checking.

A few caveats to keep in mind:

- There are some minor differences to the language described in the paper.

- The language is very basic, and misses lots of features you would expect from
  a real ML, especially datatypes.

- Error messages are rather basic, and types are always output in internal F-ing
  notation, with no attempt to abbreviate.

- It is slow and probably buggy. In particular, type checking is implemented
  very naively, as a direct transliteration of the rules, with no worries about
  efficiency. Type inference is best considered work in progress. :)

- There is no documentation other than this file, and no sample code to speak
  of. See [prelude.1ml](prelude.1ml) for some simple library code, and
  [paper.1ml](paper.1ml) for the examples from the paper.

Nevertheless, feedback on bugs or other comments are highly welcome
(<rossberg@mpi-sws.org>).

### Building

You need OCaml 4.02 (or higher) and Make to build 1ML. Then just invoke:

```bash
make
```

This creates an executable `1ml` in the source directory.

### Running

The synopsis for running the interpreter is:

```bash
1ml [option] [file ...]
```

It evaluates the 1ML source files in the order they are given. If no file names
are provided, or one of the arguments is `-`, you will enter an interactive
prompt, in which can evaluate 1ML definitions (bindings, not expressions!)
directly.

In order to have some basic types pre-defined, invoke the interpreter with the
prelude:

```bash
1ml prelude.1ml -           # interactive
1ml prelude.1ml myfile.1ml  # batch
```

There are a number of command line options, which can be displayed with `-help`.
Most of them are only for internal debugging purposes.

## The Language

In 1ML, the traditional distinction between ML core and module layer has been
completely eliminated. Every expression conceptually is a "module".

### Kernel Syntax

The BNF below gives the kernel syntax for 1ML. Additional syntax is defined as
derived in the next section. A program consists of a sequence of bindings.

```
(types)
T ::=
    E                       (projection from a value/module)
    type                    (the type/kind of small types)
    { D }                   (record/structure type)
    (X : T) ~> E            (impure function/functor/constructor type)
    (X : T) -> E            (pure function/functor/constructor type)
    '(X : T) -> E           (implicit function/functor/constructor type)
    (= E)                   (singleton/alias type)
    T with .Xs = E          (type refinement)
    wrap T                  (impredicative wrapped type)
    _                       (type wildcard)
    T as T                  (layered type (both operands have to be equal))

(declarations)
D ::=
    X : T                   (field/member declaration)
    ... T                   (type/signature inclusion)
    D , D                   (sequencing)
                            (empty)

(expressions)
E ::=
    X                       (variable)
    123                     (integer literal)
    'c'                     (character literal)
    "text"                  (text literal)
    type T                  (type value)
    { B }                   (record/structure)
    E.X                     (projection)
    fun (X : T) => E        (function/functor/constructor)
    fun '(X : T) => E       (implicit function/functor/constructor)
    E E                     (application)
    if E then E else E : T  (conditional)
    E :# T                  (impredicative wrapping)
    E #: T                  (unwrapping)
    rec (X : T) => E        (recursion)
    E :@ T                  (recursive type roll)
    E @: T                  (recursive type unroll)
    import "path"           (static import of module from file at path)

(bindings)
B ::=
    X = E                   (field/member definition)
    ... E                   (record/structure inclusion)
    B , B                   (sequencing)
                            (empty)
    type_error E            (type error assertion - elaboration of E must fail)
```

Comments are written in one of the following form:

```1ml
(; ... ;)
;; ...                  (extends to end of line)
;;;; ...                (extends to end of file)
```

### Extended Syntax

A number of derived forms (syntactic sugar) are defined on top of the basic
syntax. Note that in many cases, their expansions are defined recursively or in
terms of other derived form.

```
(types)
T ::= ...
    (T1, ..., Tn)                   ~> {_1 : T1, ..., _n : Tn}
    T1 ~> T2                        ~> (_ : T1) ~> T2
    T1 -> T2                        ~> (_ : T1) -> T2
    P ~> T                          ~> ($ : TP) ~> let P in T  [2]
    P -> T                          ~> ($ : TP) -> let P in T  [2]
    T with .Xs A1 ... An = E        ~> T with .Xs = fun A1 ... An => E
    T with type .Xs A1 ... An = T'  ~> T with .Xs = fun A1 ... An => type T'
    let B in T                      ~> (let B in type T)
    rec P => T                      ~> (rec P => type T)

(arguments)
A ::=
    (X : T)
    '(X : type)
    '(type X)                       ~> '(X : type)
    'X                              ~> '(X : type)
    X                               ~> (X : _)  or  (X : type)  [1]
    P

(declarations)
D ::= ...
    X A1 ... An : T                 ~> X : A1 -> ... -> An -> T
    X A1 ... An = E                 ~> X : A1 -> ... -> An -> (= E)
    type X A1 ... An                ~> X : A1 -> ... -> An -> type
    type X A1 ... An = T            ~> X : A1 -> ... -> An -> (= type T)
    type (A1 SYM A2) A3 ... An      ~> type (SYM) A1 ... An
    type (A1 SYM A2) A3 ... An = T  ~> type (SYM) A1 ... An = T
    let B in T                      ~> ...let B in T
    local B in D                    ~> ...let B in {D}  [4]
    X                               ~> X : (= X)

(expressions)
E ::= ...
    (E1, ..., En)                   ~> {_1 = E1, ..., _n = En}
    E.i                             ~> E._i
    fun P => E                      ~> fun ($ : TP) => let P in E  [2]
    fun A1 ... An => E              ~> fun A1 => ... => fun An => E
    (.X)                            ~> fun Y => Y.X
    (SYM)                           ~> SYM
    E1 SYM E2                       ~> (SYM) (E1, E2)  [3]
    if E1 then E2 else E3           ~> if E1 then E2 else E3 : _
    E1 || E2                        ~> if E1 then true else E2
    E1 && E2                        ~> if E1 then E2 else false
    E : T                           ~> (fun (X : T) => X) E
    E :> T                          ~> E :# T #: T
    let B in E                      ~> {B , X = E}.X
    rec P => E                      ~> rec (X : TP) => let P = X in E  [2]
    E1 ; E2                         ~> let _ = E1 in E2

(patterns)
P ::=
    _                               ~> (; empty ;)
    X                               ~> X = $
    {X1 = P1, ..., Xn = Pn}         ~> P1 = $.X1, ..., Pn = $.Xn
    {..., X, ...}                   ~> {..., X = X, ...}
    {..., X : T, ...}               ~> {..., X = X : T, ...}
    {..., X : T = E, ...}           ~> {..., X = E : T, ...}
    {..., type X A1 ... An, ...}    ~> {..., X : A1 -> ... -> An -> type, ...}
    (P1, ..., Pn)                   ~> {_1 = P1, ..., _n = Pn}
    (type X A1 ... An)              ~> (X : A1 -> ... -> An -> type)
    (type (A1 SYM A2) A3 ... An)    ~> (type (SYM) A1 ... An)
    P :# T                          ~> ...let $ = $ #: T in {P = $}
    P :@ T                          ~> ...let $ = $ @: T in {P = $}
    P : T                           ~> P = $ : T
    P1 as P2                        ~> P1, P2

(bindings)
B ::= ...
    X                               ~> X = X
    P = E                           ~> ...let $ = E in {P}
    X A1 ... An = E                 ~> X = fun A1 ... An => E
    X A1 ... An : T = E             ~> X = fun A1 ... An => E : T
    X A1 ... An :> T = E            ~> X = fun A1 ... An => E :> T
    (A1 SYM A2) A3 ... An = E       ~> (SYM) A1 ... An = E
    (A1 SYM A2) A3 ... An : T = E   ~> (SYM) A1 ... An : T = E
    (A1 SYM A2) A3 ... An :> T = E  ~> (SYM) A1 ... An :> T = E
    type X A1 ... An = T            ~> X = fun A1 ... An => type T
    type (A1 SYM A2) A3 ... An = T  ~> type (SYM) A1 ... An = T
    let B in E                      ~> ...let B in E
    local B1 in B2                  ~> ...let B1 in {B2}  [4]
    do E                            ~> ...let _ = E in {}
    import "path"                   ~> ...import "path"
```

Note [1]: The expansion of an argument `X` to `(X : type)` is only used for the
arguments to a `type` declaration or binding, in all other places it is treated
as `(X : _)`.

Note [2]: The type `TP` is derived from the pattern `P` as follows:

```
_                               ~> _
X                               ~> _
{ X1 = P1, ..., Xn = Pn }       ~> { X1 : TP1, ..., Xn : TPn }
wrap P : T                      ~> T as wrap TP
@T P                            ~> T as (rec _ => type TP)
P : T                           ~> TP as T
P1 as P2                        ~> TP1 as TP2
```

Note [3]: There is currently no precedence rules for infix operators, they are
all left-associative with the same precedence.

Note [4]: The bindings after `local-in` extend to end of structure (or file).

### Layout sensitive token insertion

To reduce noise, semicolons (`;`), commas (`,`), and `in` tokens are
automatically inserted in certain contexts based on layout.

The exact rules for token insertion should be _considered experimental_ at this
point and they will likely be changed in incompatible ways (stricter in some
ways; less strict in other ways) in the future.

### Notes on Specific Constructs

Much of the syntax should hopefully is self-explanatory if you are familiar with
ML. Here are a few less obvious bits.

#### Evaluating Expressions and Printing

Programs consist of bindings, and the prompt expects bindings, too. To evaluate
an expression for its side effect, the `do` notation can be used:

```1ml
do Int.print (3 + 5)
```

Here, `Int.print` is defined in [prelude.1ml](prelude.1ml). The prompt does not
currently print results itself.

#### Type Definitions

Types can be defined in a familiar style using the respective sugar:

```1ml
type t = int              ;; t = type int
type pair x y = (x, y)    ;; pair = fun (x : type) => fun (y : type) => (x, y)

type MONAD (m : type -> type) = {
  return 'a : a ~> m a
  bind 'a 'b : m a -> (a ~> m b) ~> m b
}

type SIG = {
  type t                       ;; t : type
  type u a                     ;; u : (a : type) -> type
  type v = int                 ;; v : (= type int)
  type w a (c : type -> type) = (u a, c t)
  ;; w : (a : type) -> (c : (_ : type) -> type) -> (= type {_1: u a, _2: c t})
}
```

#### Implicit Polymorphism

Types are generalised Damas/Milner-style at bindings:

```1ml
pair x y = {fst = x, snd = y}
p = pair 5 "foo"
```

The type of pair can be declared as follows:

```1ml
pair 'a 'b : a -> b -> {fst : a, snd : b}
```

which is just sugar for

```1ml
pair : 'a -> 'b -> a -> b -> {fst : a, snd : b}
```

or even more explicitly,

```1ml
pair : '(a : type) -> '(b : type) -> a -> b -> {fst : a, snd : b}
```

Generalisation can also take place as part of subtyping (a.k.a. signature
matching), e.g. in the following example:

```1ml
f (id 'a : a -> a) = (id 5, id "")
p = f (fun x => x)
```

This also shows that implicit functions naturally are first-class values.

#### Recursive Types

There are no datatype definitions, recursive types have to be defined
explicitly, and sometimes require explicit annotations.

```1ml
type stream = rec t => {hd : int, tl : () ~> opt t} ;; creates rec type
single x : stream = {hd = x, tl = fun () => none}  ;; b : t rolls value into t
{hd = n} = single 5                    ;; pattern match rec value
do Int.print n                         ;; or:
do Int.print (single 7).hd             ;; access rec value
```

#### Recursive Functions

The `rec` expression form allows defining recursive functions:

```1ml
count = rec self => fun i =>
  if i <> 0 then self (i - 1)

repeat = rec self => fun x : stream =>
  {hd = x, tl = fun () => some (self x)}
```

Mutual recursion is also expressible:

```1ml
{even, odd} = rec (self : {even : int ~> stream, odd : int ~> stream}) => {
  even x : stream = {hd = x, tl = fun () => some (self.odd (x + 1))}
  odd x : stream = {hd = x, tl = fun () => some (self.even (x + 1))}
}
```

Unfortunately, there is no nicer syntax for recursive declarations yet.

#### Impredicative Polymorphism

By default, polymorphism is predicative in 1ML. That is, abstract types can only
be matched by "small" types (types not containing abstract types themselves).
However, it is possible to inject large types into the small universe
explicitly, using `wrap` types. From [prelude.1ml](prelude.1ml):

```1ml
type OPT = {
  type opt a
  none 'a : opt a
  some 'a : a -> opt a
  caseopt 'a 'b : opt a -> b -> (a ~> b) ~> b
}
Opt :> OPT = {
  ;; Church encoding; it requires the abstract type opt a to be implemented
  ;; with a polymorphic (i.e., large) type. Thus, wrap the type.
  type opt a = wrap (b : type) -> b -> (a ~> b) ~> b
  none (b : type) (n : b) (s : _ ~> b) = n
  some x (b : type) (n : b) (s : _ ~> b) = s x
  caseopt (xo : opt _) = xo _
}
```

Values of type `T` can be implicitly wrapped to `wrap T` and, in case `wrap T`
contains no abstract types, can be implicitly unwrapped to `T`. In case `wrap T`
contains abstract types, unwrapping must be done explicitly.

---

<p align="center"><img width="25%" src="logo.svg"></p>
