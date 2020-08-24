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
%token FUN REC AND LET IN DO WRAP TYPE ELLIPSIS
%token IF THEN ELSE LOGICAL_OR LOGICAL_AND AS
%token EQUAL COLON SEAL ARROW SARROW DARROW
%token WITH
%token LPAR RPAR
%token LBRACE RBRACE
%token DOT TICK
%token COMMA SEMI
%token TYPE_CHECK TYPE_ERROR
%token LOCAL
%token IMPORT
%token WRAP_OP UNWRAP_OP
%token ROLL_OP UNROLL_OP

%token EOF

%token<string> WORD
%token<string> SYM
%token<string> TEXT
%token<char> CHAR
%token<int> NUM

%start prog sigs
%type<Syntax.exp> prog
%type<Syntax.typ> sigs

%%

word :
  | WORD
    { $1@@at() }
;

hole :
  | HOLE
    { "_"@@at() }
;

sym :
  | SYM
    { $1@@at() }
;
psym :
  | LPAR sym RPAR
    { $2 }
;

pname :
  | word
    { $1 }
  | psym
    { $1 }
;

name :
  | word
    { $1 }
  | psym
    { $1 }
  | sym
    { $1 }
;

phead :
  | word
    { $1 }
  | psym
    { $1 }
  | hole
    { $1 }
;

head :
  | word
    { $1 }
  | psym
    { $1 }
  | hole
    { $1 }
  | sym
    { $1 }
;

names :
  | name
    { [$1] }
  | name names
    { $1::$2 }
;

label :
  | name
    { $1 }
  | NUM
    { index($1)@@at() }
;

path :
  | label
    { $1::[] }
  | label DOT path
    { $1::$3 }
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
  | TYPE pname typparamlist
    { ($2, $3) }
;
typpat :
  | infixtyp
    { $1 }
  | TYPE phead typparamlist
    { ($2, $3) }
;

implparam :
  | TICK LPAR head COLON TYPE RPAR
    { (headB($3)@@ati 3, TypT@@ati 5, Impl@@ati 1)@@at() }
  | TICK LPAR TYPE phead RPAR
    { (headB($4)@@ati 4, TypT@@ati 3, Impl@@ati 1)@@at() }
  | TICK head
    { (headB($2)@@ati 2, TypT@@ati 1, Impl@@ati 1)@@at() }
;
annparam :
  | LPAR head annot RPAR
    { (headB($2)@@ati 2, $3, Expl@@ati 3)@@at() }
  | LPAR typpat optannot RPAR
    { let (h, tps) = $2 in
      (headB(h)@@h.at,
       funT(tps,
         Lib.Option.value $3 ~default:(TypT@@at()), Pure@@ati 2)@@ati 2,
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
    { StrT($2)@@at() }
  | LPAR RPAR
    { StrT(EmptyD@@at())@@at() }
  | LPAR typlist RPAR
    { match $2 with [t] -> t | ts -> tupT(ts)@@at() }
  | LPAR EQUAL exp RPAR
    { EqT($3)@@at() }
;
withtyp :
  | infpathexp
    { pathT($1)@@at() }
  | withtyp WITH LPAR path typparamlist EQUAL exp RPAR
    { WithT($1, $4, funE($5, $7)@@span[ati 5; ati 7])@@at() }
  | withtyp WITH LPAR TYPE path typparamlist typdef RPAR
    { WithT($1, $5, funE($6, $7)@@span[ati 6; ati 7])@@at() }
;
typ :
  | withtyp
    { $1 }
  | annparam arrow typ
    { funT([$1], $3, $2)@@at() }
  | withtyp arrow typ
    { funT([(headB("_"@@ati 1)@@ati 1, $1, Expl@@ati 2)@@ati 1], $3, $2)@@at() }
  | WRAP typ
    { WrapT($2)@@at() }
  | REC atpat bindanns_opt DARROW typ
    { recT(defaultTP $2, pathT($3(typE($5)@@ati 5))@@at())@@at() }
  | LET bind IN typ
    { letT($2, $4)@@at() }
  | FUN typparam typparamlist bindanns_opt DARROW typ
    { PathT(funE($2::$3, $4(typE($6)@@ati 6))@@at())@@at() }
;
typlist :
  | typ
    { $1::[] }
  | typ COMMA typlist
    { $1::$3 }
;

annot :
  | COLON typ
    { $2 }
;
optannot :
  |
    { None }
  | annot
    { Some $1 }
;

typdef :
  | EQUAL typ
    { typE($2)@@ati 2 }
;
opttypdef :
  |
    { None }
  | typdef
    { Some $1 }
;

atdec :
  | names implparamlist annot
    { let t = funT($2, $3, Pure@@ati 2)@@span[ati 2; ati 3] in
      ($1 |> List.map (fun n -> VarD(n, t)@@at()) |> seqDs)@@at() }
  | names implparamlist EQUAL exp
    { let t = funT($2, EqT($4)@@ati 4, Pure@@ati 3)@@span[ati 2; ati 4] in
      ($1 |> List.map (fun n -> VarD(n, t)@@at()) |> seqDs)@@at() }
  | name
    { VarD($1, EqT(VarE($1)@@at())@@at())@@at() }
  | typdec optannot opttypdef
    { VarD(fst $1,
        funT(snd $1,
          (match $2, $3 with
           | None, None -> TypT@@at()
           | Some t2, None -> t2
           | None, Some te3 -> EqT(te3)@@ati 3
           | Some t2, Some te3 ->
             EqT(annotE(te3, t2)@@span[ati 2; ati 3])@@span[ati 2; ati 3]),
          Pure@@at())@@at())@@at() }
  | ELLIPSIS typ
    { inclD($2)@@at() }
  | LET bind IN typ
    { inclD(letT($2, $4)@@at())@@at() }
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
    { dotE($1, $3)@@at() }
;
atpathexp :
  | pname
    { VarE($1)@@at() }
  | HOLE
    { typE(HoleT@@at())@@at() }
;
apppathexp :
  | dotpathexp
    { $1 }
  | apppathexp dotpathexp
    { appE($1, $2)@@at() }
  | apppathexp attyp
    { appE($1, typE($2)@@ati 2)@@at() }
;
infpathexp :
  | apppathexp_or_typ
    { $1 }
  | sym apppathexp_or_typ
    { appE(VarE($1)@@ati(1), $2)@@at() }
  | infpathexp sym apppathexp_or_typ
    { appE(appE(VarE($2)@@ati(2), $1)@@at(), $3)@@at() }
;
apppathexp_or_typ :
  | apppathexp
    { $1 }
  | attyp
    { typE($1)@@at() }
;

dotexp :
  | atexp
    { $1 }
  | dotexp DOT label
    { dotE($1, $3)@@at() }
;
atexp :
  | atpathexp
    { $1 }
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
    { StrE($2)@@at() }
  | LPAR RPAR
    { StrE(EmptyB@@at())@@at() }
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
annexp_op :
  | COLON
    { annotE }
  | SEAL
    { sealE }
  | ROLL_OP
    { rollE }
  | UNROLL_OP
    { unrollE }
  | WRAP_OP
    { wrapE }
  | UNWRAP_OP
    { unwrapE }
;
annexp :
  | infexp
    { $1 }
  | TYPE typ
    { typE($2)@@at() }
  | annexp annexp_op typ
    { $2($1, $3)@@at() }
;
exp :
  | LET bind IN exp
    { letE($2, $4)@@at() }
  | IF exp THEN exp ELSE exp
    { ifE($2, $4, $6)@@at() }
  | annexp SEMI exp
    { seqE($1, $3)@@at() }
  | annexp
    { $1 }
  | FUN param paramlist bindanns_opt DARROW exp
    { funE($2::$3, $4($6))@@at() }
  | REC atpat bindanns_opt DARROW exp
    { recE(defaultP $2, $3($5))@@at() }
;

explist :
  | exp
    { $1::[] }
  | exp COMMA explist
    { $1::$3 }
;

funbind :
  | phead param paramlist
    { ($1, $2::$3) }
  | oneexplparam sym oneexplparam
    { ($2, $1 @ $3) }
  | LPAR oneexplparam sym oneexplparam RPAR paramlist
    { ($3, $2 @ $4 @ $6) }
;

bindann_op :
  | COLON
    { annotE }
  | SEAL
    { sealE }
  | ROLL_OP
    { rollE }
  | UNROLL_OP
    { unrollE }
  | WRAP_OP
    { wrapE }
  | UNWRAP_OP
    { unwrapE }
;
bindann :
  | bindann_op typ
    { fun e -> $1(e, $2)@@span[ati 2; e.at] }
;
bindanns :
  | bindann
    { $1 }
  | bindanns bindann
    { fun e -> $2 ($1 e) }
;
bindanns_opt :
  |
    { fun e -> e }
  | bindanns
    { $1 }
;

atbind :
  | funbind bindanns_opt EQUAL exp
    { let (h, ps) = $1 in VarB(h, funE(ps, $2($4))@@at())@@at() }
  | bindpat bindanns_opt EQUAL exp
    { patB($1, $2($4))@@at() }
  | name
    { VarB($1, VarE($1.it@@at())@@at())@@at() }
  | typpat bindanns_opt typdef
    { VarB(fst $1, funE(snd $1, $2($3))@@at())@@at() }
  | ELLIPSIS exp
    { inclB($2)@@at() }
  | DO exp
    { doB($2)@@at() }
  | TYPE_CHECK exp
    { TypeAssertB(true, $2)@@at() }
  | TYPE_ERROR exp
    { TypeAssertB(false, $2)@@at() }
  | LET bind IN exp
    { inclB(letE($2, $4)@@at())@@at() }
  | IMPORT TEXT
    { InclB(ImportE($2@@ati 2)@@at())@@at() }
/*
  | LPAR bind RPAR
    { $2 }
*/
;
atbinds :
  | atbind
    { $1 }
  | atbind AND atbinds
    { seqB($1, $3)@@at() }
;
recbind :
  | atbind
    { $1 }
  | REC atbinds
    { recB($2)@@at() }
;
bind :
  |
    { EmptyB@@at() }
  | recbind
    { $1 }
  | recbind COMMA bind
    { seqB($1, $3)@@at() }
  | LOCAL bind IN bind
    { letB($2, $4)@@at() }
;

bindpat :
  | atpat
    { $1 }
  | sym
    { headP $1@@at() }
;

atpat :
  | phead
    { headP $1@@at() }
  | LBRACE decon RBRACE
    { strP($2, at())@@at() }
  | LPAR RPAR
    { strP([], at())@@at() }
  | LPAR sym annot RPAR
    { annotP(headP($2)@@$2.at, $3)@@at() }
  | LPAR head typparam typparamlist annot RPAR
    { annotP(headP($2)@@$2.at, funT($3::$4, $5, Pure@@at())@@at())@@at() }
  | LPAR patlist RPAR
    { match $2 with [p] -> p | ps -> tupP(ps, at())@@at() }
  | LPAR typpat optannot RPAR
    { let (h, tps) = $2 in
      annotP(headP(h)@@h.at,
        funT(tps,
          Lib.Option.value $3 ~default:(TypT@@at()),
          Pure@@at())@@at())@@at() }
;
annpat_op :
  | COLON
    { annotP }
  | ROLL_OP
    { rollP }
  | WRAP_OP
    { wrapP }
;
annpat :
  | atpat
    { $1 }
  | annpat annpat_op typ
    { $2($1, $3)@@at() }
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
  | names optannot EQUAL pat
    { let p = opt annotP($4, $2)@@span[ati 2; ati 4] in
      $1 |> List.map (fun n -> (n, p)@@at()) }
  | names implparam implparamlist annot
    { let t = funT($2::$3, $4, Pure@@at())@@at() in
      $1 |> List.map (fun n -> (n, annotP(varP(n)@@n.at, t)@@at())@@at()) }
  | names annot
    { $1 |> List.map (fun n -> (n, annotP(varP(n)@@n.at, $2)@@at())@@at()) }
  | name
    { [($1, varP($1)@@at())@@at()] }
  | typdec optannot
    { let (n, tps) = $1 in
      [(n,
        annotP(varP(n)@@n.at,
          funT(tps,
            Lib.Option.value $2 ~default:(TypT@@at()),
            Pure@@at())@@at())@@at())@@at()] }
/*
  | LPAR decon RPAR
    { $2 }
*/
;
decon :
  |
    { [] }
  | atdecon
    { $1 }
  | atdecon COMMA decon
    { $1@$3 }
;

prog :
  | bind EOF
    { StrE($1)@@at() }
;

sigs :
  | dec EOF
    { StrT($1)@@at() }
;

%%

