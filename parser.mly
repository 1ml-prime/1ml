/*
 * (c) 2014 Andreas Rossberg
 */

%{
open Source
open Syntax

let position_to_pos position =
  { file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  { left = position_to_pos position1;
    right = position_to_pos position2
  }

let at () =
  positions_to_region (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
let ati i =
  positions_to_region (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)

let parse_error s = raise (Source.Error (Source.nowhere_region, s))
%}

%token HOLE PRIMITIVE
%token FUN REC LET IN DO WRAP UNWRAP TYPE ELLIPSIS
%token IF THEN ELSE LOGICAL_OR LOGICAL_AND AS
%token EQUAL COLON SEAL ARROW SARROW DARROW
%token WITH
%token LPAR RPAR
%token LBRACE RBRACE
%token DOT AT TICK
%token COMMA SEMI
%token TYPE_ERROR
%token LOCAL
%token IMPORT

%token EOF

%token<string> NAME
%token<string> SYM
%token<string> TEXT
%token<char> CHAR
%token<int> NUM

%start prog sigs
%type<Syntax.exp> prog
%type<Syntax.typ> sigs

%%

label :
  | name
    { $1 }
  | sym
    { $1 }
  | NUM
    { index($1)@@at() }
;
sym :
  | SYM
    { $1@@at() }
;
name :
  | NAME
    { $1@@at() }
  | LPAR sym RPAR
    { $2 }
;
namelist :
  | name
    { $1::[] }
  | name DOT namelist
    { $1::$3 }
;

head :
  | name
    { $1 }
  | HOLE
    { "_"@@at() }
;

infixtyp :
  | TYPE typparam sym typparam
    { ($3, [$2; $4]) }
  | TYPE LPAR typparam sym typparam RPAR typparamlist
    { ($4, $3::$5::$7) }
;
typdec :
  | infixtyp
    { $1 }
  | TYPE name typparamlist
    { ($2, $3) }
;
typpat :
  | infixtyp
    { $1 }
  | TYPE head typparamlist
    { ($2, $3) }
;

implparam :
  | TICK LPAR head COLON TYPE RPAR
    { let b, _ = headP($3) in (b, TypT@@ati 5, Impl@@ati 1)@@at() }
  | TICK LPAR TYPE head RPAR
    { let b, _ = headP($4) in (b, TypT@@ati 3, Impl@@ati 1)@@at() }
  | TICK head
    { let b, _ = headP($2) in (b, TypT@@ati 1, Impl@@ati 1)@@at() }
;
annparam :
  | LPAR head COLON typ RPAR
    { let b, _ = headP($2) in (b, $4, Expl@@ati 3)@@at() }
  | LPAR typpat RPAR
    { let (head, typparams) = $2 in
      let b, _ = headP(head) in
      (b, funT(typparams, TypT@@at(), Pure@@ati 2)@@ati 2,
        Expl@@ati 2)@@at() }
  | implparam
    { $1 }
;
explparam :
  | atpat
    { let b, t = (defaultP $1).it in (b, t, Expl@@at())@@at() }
;
param :
  | explparam
    { $1 }
  | implparam
    { $1 }
;
paramlist :
  |
    { [] }
  | param paramlist
    { $1::$2 }
;
typparam :
  | param
    { typParam $1 }
;
typparamlist :
  | paramlist
    { typParamList $1 }
;
implparamlist :
  |
    { [] }
  | implparam implparamlist
    { $1::$2 }
;
oneexplparam :
  | implparam implparamlist explparam
    { ($1::$2) @ [$3] }
  | explparam
    { [$1] }
;
arrow :
  | SARROW
    { Impure@@at() }
  | ARROW
    { Pure@@at() }
;

attyp :
  | PRIMITIVE TEXT
    { PrimT($2)@@at() }
  | TYPE
    { TypT@@at() }
  | LBRACE dec RBRACE
    { strT($2)@@at() }
  | LPAR RPAR
    { strT(EmptyD@@at())@@at() }
  | LPAR typlist RPAR
    { match $2 with [t] -> t | ts -> tupT(ts)@@at() }
  | LPAR EQUAL exp RPAR
    { EqT($3)@@at() }
;
apptyp :
  | attyp
    { $1 }
  | pathexp
    { PathT($1)@@at() }
;
withtyp :
  | apptyp
    { $1 }
  | withtyp WITH LPAR namelist typparamlist EQUAL exp RPAR
    { WithT($1, $4, funE($5, $7)@@span[ati 5; ati 7])@@at() }
  | withtyp WITH LPAR TYPE namelist typparamlist EQUAL typ RPAR
    { WithT($1, $5, funE($6, TypE($8)@@ati 8)@@span[ati 6; ati 8])@@at() }
;
typ :
  | withtyp
    { $1 }
  | annparam arrow typ
    { funT([$1], $3, $2)@@at() }
  | withtyp arrow typ
    { let b, _ = headP("_"@@ati 1) in
      funT([(b, $1, Expl@@ati 2)@@ati 1], $3, $2)@@at() }
  | WRAP typ
    { WrapT($2)@@at() }
  | REC atpat DARROW typ
    { recT(defaultTP $2, $4)@@at() }
  | LET bind IN typ
    { letT($2, $4)@@at() }
;
typlist :
  | typ
    { $1::[] }
  | typ COMMA typlist
    { $1::$3 }
;

optannot :
  |
    { None }
  | COLON typ
    { Some $2 }
;

opttypdef :
  |
    { TypT@@at() }
  | EQUAL typ
    { EqT(TypE($2)@@ati 2)@@ati 2 }
;

atdec :
  | name typparamlist COLON typ
    { VarD($1, funT($2, $4, Pure@@ati 2)@@span[ati 2; ati 4])@@at() }
  | name typparamlist EQUAL exp
    { VarD($1, funT($2, EqT($4)@@ati 4, Pure@@ati 3)@@span[ati 2; ati 4])
        @@at() }
  | name
    { VarD($1, funT([], EqT(VarE($1)@@ati 1)@@ati 1, Pure@@ati 1)@@ati 1)@@at() }
  | typdec opttypdef
    { VarD(fst $1, funT(snd $1, $2, Pure@@at())@@at())@@at() }
  | ELLIPSIS typ
    { InclD($2)@@at() }
  | LET bind IN typ
    { InclD(letT($2, $4)@@at())@@at() }
/*
  | LPAR dec RPAR
    { $2 }
*/
;
dec :
  |
    { EmptyD@@at() }
  | atdec
    { $1 }
  | atdec COMMA dec
    { seqD($1, $3)@@at() }
  | LOCAL bind IN dec
    { letD($2, $4)@@at() }
;

dotpathexp :
  | atpathexp
    { $1 }
  | dotpathexp DOT label
    { DotE($1, $3)@@at() }
  | dotpathexp DOT AT attyp
    { unrollE($1, $4)@@at() }
  | dotpathexp DOT AT name
    { unrollE($1, PathT(VarE($4)@@ati 4)@@ati 4)@@at() }
;
atpathexp :
  | name
    { VarE($1)@@at() }
  | HOLE
    { TypE(HoleT@@at())@@at() }
;
apppathexp :
  | dotpathexp
    { $1 }
  | apppathexp dotexp
    { appE($1, $2)@@at() }
  | AT attyp atexp
    { rollE($3, $2)@@at() }
  | AT name atexp
    { rollE($3, PathT(VarE($2)@@ati 2)@@ati 2)@@at() }
;
infpathexp :
  | apppathexp
    { $1 }
  | sym apppathexp
    { appE(VarE($1)@@ati(1), $2)@@at() }
  | infpathexp sym apppathexp
    { appE(appE(VarE($2)@@ati(2), $1)@@at(), $3)@@at() }
;
pathexp :
  | infpathexp
    { $1 }
;

dotexp :
  | atexp
    { $1 }
  | dotexp DOT label
    { DotE($1, $3)@@at() }
  | dotexp DOT AT attyp
    { unrollE($1, $4)@@at() }
  | dotexp DOT AT name
    { unrollE($1, PathT(VarE($4)@@ati 4)@@ati 4)@@at() }
;
atexp :
  | name
    { VarE($1)@@at() }
  | HOLE
    { TypE(HoleT@@at())@@at() }
  | PRIMITIVE TEXT
    { match Prim.fun_of_string $2 with
      | Some f -> PrimE(Prim.FunV f)@@at()
      | None -> parse_error ("unknown primitive \"" ^ $2 ^ "\"") }
  | NUM
    { PrimE(Prim.IntV($1))@@at() }
  | CHAR
    { PrimE(Prim.CharV($1))@@at() }
  | TEXT
    { PrimE(Prim.TextV($1))@@at() }
  | LBRACE bind RBRACE
    { strE($2)@@at() }
  | LPAR RPAR
    { strE(EmptyB@@at())@@at() }
  | LPAR explist RPAR
    { match $2 with [e] -> e | es -> tupE(es)@@at() }
  | LPAR DOT label RPAR
    { dotopE($3)@@at() }
  | IMPORT TEXT
    { ImportE($2@@ati 2)@@at() }
;
appexp :
  | dotexp
    { $1 }
  | appexp dotexp
    { appE($1, $2)@@at() }
  | AT attyp atexp
    { rollE($3, $2)@@at() }
  | AT name atexp
    { rollE($3, PathT(VarE($2)@@ati 2)@@ati 2)@@at() }
;
infexp :
  | appexp
    { $1 }
  | sym appexp
    { appE(VarE($1)@@ati(1), $2)@@at() }
  | infexp sym appexp
    { appE(appE(VarE($2)@@ati(2), $1)@@at(), $3)@@at() }
  | infexp LOGICAL_OR appexp
    { orE($1, $3)@@at() }
  | infexp LOGICAL_AND appexp
    { andE($1, $3)@@at() }
;
annexp :
  | infexp optannot
    { opt annotE($1, $2)@@at() }
  | TYPE typ
    { TypE($2)@@at() }
  | annexp SEAL typ
    { sealE($1, $3)@@at() }
  | WRAP infexp COLON typ
    { wrapE($2, $4)@@at() }
  | UNWRAP infexp COLON typ
    { unwrapE($2, $4)@@at() }
;
inexp :
  | annexp
    { $1 }
  | FUN param paramlist DARROW exp
    { funE($2::$3, $5)@@at() }
  | IF exp THEN exp ELSE infexp optannot
    { ifE($2, $4, $6, match $7 with None -> HoleT@@ati 1 | Some t -> t)@@at() }
  | REC atpat DARROW exp
    { recE(defaultP $2, $4)@@at() }
;
exp :
  | LET bind IN exp
    { letE($2, $4)@@at() }
  | inexp SEMI exp
    { seqE($1, $3)@@at() }
  | inexp
    { $1 }
;

explist :
  | exp
    { $1::[] }
  | exp COMMA explist
    { $1::$3 }
;

funbind :
  | head param paramlist
    { ($1, $2::$3) }
  | oneexplparam sym oneexplparam
    { ($2, $1 @ $3) }
  | LPAR oneexplparam sym oneexplparam RPAR paramlist
    { ($3, $2 @ $4 @ $6) }
;

atbind :
  | funbind optannot EQUAL exp
    { let (head, params) = $1 in
      VarB(head, funE(params,
        opt annotE($4, $2)@@span[ati 2; ati 4])@@at())@@at() }
  | funbind SEAL typ EQUAL exp
    { let (head, params) = $1 in
      VarB(head, funE(params, sealE($5, $3)@@span[ati 3; ati 5])@@at())@@at() }
  | head SEAL typ EQUAL exp
    { VarB($1, sealE($5, $3)@@span[ati 3; ati 5])@@at() }
  | pat EQUAL exp
    { patB($1, $3)@@at() }
  | name
    { VarB($1, VarE($1.it@@at())@@at())@@at() }
  | typpat EQUAL typ
    { VarB(fst $1, funE(snd $1, TypE($3)@@ati 3)@@at())@@at() }
  | ELLIPSIS exp
    { InclB($2)@@at() }
  | DO exp
    { doB($2)@@at() }
  | TYPE_ERROR exp
    { TypeErrorB($2)@@at() }
  | LET bind IN exp
    { InclB(letE($2, $4)@@at())@@at() }
  | IMPORT TEXT
    { InclB(ImportE($2@@ati 2)@@at())@@at() }
/*
  | LPAR bind RPAR
    { $2 }
*/
;
bind :
  |
    { EmptyB@@at() }
  | atbind
    { $1 }
  | atbind COMMA bind
    { seqB($1, $3)@@at() }
  | LOCAL bind IN bind
    { letB($2, $4)@@at() }
;

atpat :
  | head
    { headP $1@@at() }
  | LBRACE decon RBRACE
    { strP($2, at())@@at() }
  | LPAR RPAR
    { strP([], at())@@at() }
  | LPAR head typparam typparamlist COLON typ RPAR
    { annotP(headP($2)@@$2.at, funT($3::$4, $6, Pure@@at())@@at())@@at() }
  | LPAR patlist RPAR
    { match $2 with [p] -> p | ps -> tupP(ps, at())@@at() }
  | LPAR typpat RPAR
    { let (head, typparams) = $2 in
      annotP(headP(head)@@head.at,
        funT(typparams, TypT@@at(), Pure@@at())@@at())@@at() }
;
apppat :
  | atpat
    { $1 }
  | AT attyp atpat
    { rollP($3, $2)@@at() }
  | AT name atpat
    { rollP($3, PathT(VarE($2)@@ati 2)@@ati 2)@@at() }
;
annpat :
  | apppat optannot
    { opt annotP($1, $2)@@at() }
  | WRAP apppat COLON typ
    { wrapP($2, $4)@@at() }
;
pat :
  | annpat
    { $1 }
  | annpat AS annpat
    { asP($1, $3)@@at() }
;
patlist :
  | pat
    { $1::[] }
  | pat COMMA patlist
    { $1::$3 }
;

atdecon :
  | name optannot EQUAL pat
    { ($1, opt annotP($4, $2)@@span[ati 2; ati 4])@@at() }
  | name optannot
    { ($1, opt annotP(varP($1)@@ati 1, $2)@@at())@@at() }
  | name typparam typparamlist COLON typ
    { ($1, annotP(varP($1)@@$1.at,
       funT($2::$3, $5, Pure@@at())@@at())@@at())@@at() }
  | typdec
    { let (name, typparams) = $1 in
      (name, annotP(varP(name)@@name.at,
       funT(typparams, TypT@@at(), Pure@@at())@@at())@@at())@@at() }
/*
  | LPAR decon RPAR
    { $2 }
*/
;
decon :
  |
    { [] }
  | atdecon
    { [$1] }
  | atdecon COMMA decon
    { $1::$3 }
;

prog :
  | bind EOF
    { strE($1)@@at() }
;

sigs :
  | dec EOF
    { strT($1)@@at() }
;

%%

