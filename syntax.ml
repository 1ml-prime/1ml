(*
 * (c) 2014 Andreas Rossberg
 *)

open Source

type var = (string, unit) phrase
type path = (string, unit) phrase

type impl = (impl', unit) phrase
and impl' =
  | Impl
  | Expl

type eff = (eff', unit) phrase
and eff' =
  | Pure
  | Impure

type typ = (typ', unit) phrase
and typ' =
  | PathT of exp
  | PrimT of string
  | TypT
  | HoleT
  | StrT of dec
  | FunT of var * typ * typ * eff * impl
  | WrapT of typ
  | EqT of exp
  | AsT of typ * typ
  | WithT of typ * var list * exp

and dec = (dec', unit) phrase
and dec' =
  | EmptyD
  | SeqD of dec * dec
  | VarD of var * typ
  | InclD of typ

and exp = (exp', unit) phrase
and exp' =
  | VarE of var
  | PrimE of Prim.const
  | TypE of typ
  | PathE of exp
  | StrE of bind
  | FunE of var * typ * exp * impl
  | WrapE of var * typ
  | RollE of var * typ
  | IfE of var * exp * exp
  | DotE of exp * var
  | AppE of var * var
  | UnwrapE of var * typ
  | UnrollE of var * typ
  | RecE of var * typ * exp
  | ImportE of path
  | AnnotE of exp * typ

and bind = (bind', unit) phrase
and bind' =
  | EmptyB
  | SeqB of bind * bind
  | VarB of var * exp
  | InclB of exp
  | TypeAssertB of bool * exp


let uniq_count = ref 0
let uniq_var () = incr uniq_count; "%_" ^ string_of_int !uniq_count
let is_uniq n = Lib.String.is_prefix "%_" n.it
let rec every_var pr b =
  match b.it with
  | EmptyB -> true
  | SeqB(b1, b2) -> every_var pr b1 && every_var pr b2
  | VarB(v, _) -> pr v
  | InclB(_) -> false
  | TypeAssertB(_) -> true

let index n = "_" ^ string_of_int n

(* Helpers *)

let typParam param =
  match param.it with
  | (b, {it = HoleT; at}, i) -> (b, TypT@@at, i)@@param.at
  | _ -> param

let typParamList paramList =
  List.map typParam paramList

let seqD(l, r) =
  match l.it, r.it with
  | EmptyD, _ -> r.it
  | _, EmptyD -> l.it
  | _ -> SeqD(l, r)

let seqB(l, r) =
  match l.it, r.it with
  | EmptyB, _ -> r.it
  | _, EmptyB -> l.it
  | _ -> SeqB(l, r)

let opt fn (e, t) =
  match t with
  | None -> e.it
  | Some t -> fn(e, t)

let typE(t) =
  match t.it with
  | PathT(e) -> PathE(e)
  | _ -> TypE(t)

let pathT(e) =
  match e.it with
  | TypE(t) -> t.it
  | _ -> PathT(e)

let inclD(t) =
  match t.it with
  | StrT(d) -> d.it
  | _ -> InclD(t)

let inclB(e) =
  match e.it with
  | StrE(b) -> b.it
  | _ -> InclB(e)

let dotE(e, l) =
  match e.it with
  | StrE({it = VarB(x, e)}) when x.it = l.it -> e.it
  | _ -> DotE(e, l)

(* Sugar *)

let asVarB(e, pr) =
  match e.it with
  | VarE(x) -> EmptyB@@e.at, x
  | DotE({it=StrE(b)}, l) when pr b ->
    b, l
  | _ ->
    let x = uniq_var()@@e.at in
    VarB(x, e)@@e.at, x

let letE(b1, e) =
  let b2, x = asVarB(e, fun _ -> true) in
  let r = span[b1.at; b2.at] in
  dotE(StrE(seqB(b1, b2)@@r)@@r, x)

let asVarE(e, k) =
  let b, x = asVarB(e, every_var is_uniq) in
  letE(b, k x)

let letT(b, t) = pathT(letE(b, typE(t)@@t.at)@@span[b.at; t.at])
let letD(b, d) = inclD(letT(b, StrT(d)@@d.at)@@span[b.at; d.at])
let letB(b, b') = inclB(letE(b, StrE(b')@@b'.at)@@span[b.at; b'.at])

let rec tupT(ts) = StrT(tupT' 1 ts)
and tupT' n = function
  | [] -> EmptyD@@nowhere_region
  | t::ts ->
    let d = tupT' (n + 1) ts in
    seqD(VarD((index n)@@t.at, t)@@t.at, d)@@
      (match d.it with EmptyD -> t.at | _ -> span[t.at; d.at])

let rec tupE(es) = StrE(tupE' 1 es)
and tupE' n = function
  | [] -> EmptyB@@nowhere_region
  | e::es ->
    let b = tupE' (n + 1) es in
    seqB(VarB((index n)@@e.at, e)@@e.at, b)@@
      (match b.it with EmptyB -> e.at | _ -> span[e.at; b.at])

let rec funT(ps, t, f) = (funT'(ps, t, f)).it
and funT'(ps, t, f) =
  match ps with
  | [] -> t
  | p::ps' ->
    let (b, t1, i) = p.it in
    let t2 = funT'(ps', t, f) in
    let x, t2' =
      match b.it with
      | EmptyB -> "_"@@p.at, t2
      | VarB(x, {it = VarE({it = "$"})}) -> x, t2
      | _ -> "$"@@p.at, letT(b, t2)@@span[p.at; t2.at]
    in FunT(x, t1, t2', f.it@@f.at, i)@@span[p.at; t.at]

let rec funE(ps, e) = (funE'(ps, e)).it
and funE'(ps, e) =
  match ps with
  | [] -> e
  | p::ps' ->
    let (b, t, i) = p.it in
    let e' = funE'(ps', e) in
    let x, e'' =
      match b.it with
      | EmptyB -> "_"@@p.at, e'
      | VarB(x, {it = VarE({it = "$"})}) -> x, e'
      | _ -> "$"@@p.at, letE(b, e')@@span[p.at; e.at]
    in FunE(x, t, e'', i)@@span[p.at; e.at]

let doB(e) =
  let b, _ = asVarB(e, fun _ -> true) in
  letB(b, EmptyB@@e.at)

let seqE(l, r) = asVarE(l, fun _ -> r)

let ifE(e1, e2, e3) =
  let at = span[e1.at; e3.at] in
  let ifE = asVarE(e1, fun x -> IfE(x, e2, e3)@@at) in
  match e3.it with
  | AnnotE(_, t) -> AnnotE(ifE@@at, t)
  | _ -> ifE

let orE(e1, e2) =
  ifE(e1, PrimE(Prim.BoolV(true))@@e1.at, e2)
let andE(e1, e2) =
  ifE(e1, e2, PrimE(Prim.BoolV(false))@@e1.at)

let appE(e1, e2) =
  asVarE(e1, fun x1 ->
  asVarE(e2, fun x2 ->
  AppE(x1, x2)@@span[e1.at; e2.at])@@span[e1.at; e2.at])

let wrapE(e, t) =
  asVarE(e, fun x -> WrapE(x, t)@@span[e.at; t.at])

let unwrapE(e, t) =
  asVarE(e, fun x -> UnwrapE(x, t)@@span[e.at; t.at])

let rollE(e, t) =
  asVarE(e, fun x -> RollE(x, t)@@span[e.at; t.at])

let unrollE(e, t) =
  asVarE(e, fun x -> UnrollE(x, t)@@span[e.at; t.at])

let annotE(e, t) = AnnotE(e, t)

let sealE(e, t) =
  (* TODO: clone t! *)
  unwrapE(wrapE(e, WrapT(t)@@t.at)@@span[e.at; t.at], WrapT(t)@@t.at)

let dotopE(x) =
  FunE("x"@@x.at, HoleT@@x.at, DotE(VarE("x"@@x.at)@@x.at, x)@@x.at, Expl@@x.at)

type pat = {bind: bind; infer: typ option; annot: typ option}

let recT(p, t2) =
  let b, t1 = p.it in
  let e = typE(t2)@@t2.at in
  let e' =
    match b.it with
    | VarB(x, {it = VarE({it = "$"})}) -> RecE(x, t1, e)
    | EmptyB -> RecE("_"@@b.at, t1, e)
    | _ -> RecE("$"@@b.at, t1, letE(b, e)@@span[b.at; e.at])
  in PathT(e'@@span[p.at; t2.at])

let recE(p, e) =
  let b, t = p.it in
  match b.it with
  | VarB(x, {it = VarE({it = "$"})}) -> RecE(x, t, e)
  | EmptyB -> RecE("_"@@b.at, t, e)
  | _ -> RecE("$"@@b.at, t, letE(b, e)@@span[b.at; e.at])

let patB(p, e) =
  let e' =
    match p.it.annot with
    | None -> e
    | Some t -> annotE(e, t)@@span[e.at; t.at]
  in
  let b =
    match p.it.bind.it with
    | EmptyB -> doB(e')
    | VarB(x, {it = VarE({it = "$"})}) -> VarB(x, e')
    | _ -> letB(VarB("$"@@e.at, e')@@e.at, p.it.bind)
  in
  match p.it.infer with
  | None -> b
  | Some t ->
    letB(VarB(uniq_var()@@p.at, annotE(e, t)@@span[e.at; t.at])@@p.at, b@@p.at)

let asTopt(to1, to2) =
  match to1, to2 with
  | None, None -> None
  | None, some | some, None -> some
  | Some t1, Some t2 -> Some(AsT(t1, t2)@@span[t1.at; t2.at])

let defaultP p =
  (p.it.bind,
   match asTopt(p.it.infer, p.it.annot) with
   | None -> HoleT@@p.at
   | Some t -> t)@@p.at

let defaultTP p =
  (p.it.bind,
   match asTopt(p.it.infer, p.it.annot) with
   | None -> TypT@@p.at
   | Some t -> t)@@p.at

let varB x = VarB(x, VarE("$"@@x.at)@@x.at)
let varP x = {bind = varB(x)@@x.at; infer = None; annot = None}

let headB id = if id.it = "_" then EmptyB else varB(id)
let headP id = {bind = headB(id)@@id.at; infer = None; annot = None}

let asP(p1, p2) =
  {bind = seqB(p1.it.bind.it@@p1.at, p2.it.bind.it@@p2.at)@@span[p1.at; p2.at];
   infer = asTopt(p1.it.infer, p2.it.infer);
   annot = asTopt(p1.it.annot, p2.it.annot)}

let annotP(p, t2) =
  {p.it with
    bind = p.it.bind.it@@span[p.at; t2.at];
    annot = asTopt(p.it.annot, Some t2)}

let wrapP(p, t2) =
  {bind =
    letB(
      VarB("$"@@t2.at, UnwrapE("$"@@t2.at, t2)@@t2.at)@@t2.at,
      patB(p, VarE("$"@@t2.at)@@t2.at)@@span[p.at; t2.at]
    )@@span[p.at; t2.at];
   infer = None;
   annot = None}

let strP(xps, region) =
  match xps with
  | [] ->
    {bind = EmptyB@@region;
     infer = Some (StrT(EmptyD@@region)@@region);
     annot = None}
  | xp::_ ->
    let b, d =
      List.fold_right (fun xp (b, d) ->
        let x, p = xp.it in
        let _, t = (defaultP p).it in
        seqB(patB(p, DotE(VarE("$"@@xp.at)@@xp.at, x)@@xp.at)@@xp.at, b)
          @@span[b.at; p.at],
        seqD(VarD(x, t.it@@p.at)@@xp.at, d)@@span[d.at; p.at]
      ) xps (EmptyB@@xp.at, EmptyD@@xp.at)
    in {bind = b; infer = Some (StrT(d)@@d.at); annot = None}

let rec tupP' n = function
  | [] -> []
  | p::ps -> (((index n)@@p.at, p)@@p.at) :: tupP' (n + 1) ps
let tupP(ps, region) = strP(tupP' 1 ps, region)

let rollP(p, t2) =
  {bind =
    letB(
      VarB("$"@@t2.at, UnrollE("$"@@t2.at, t2)@@t2.at)@@t2.at,
      patB(p, VarE("$"@@t2.at)@@t2.at)@@span[p.at; t2.at]
    )@@span[p.at; t2.at];
   infer = None;
   annot = Some t2}


(* *)

let seqDs = function
  | [] -> EmptyD
  | d::ds -> (List.fold_left (fun s d -> seqD(s, d)@@d.at) d ds).it


(* String conversion *)

let node label = function
  | [] -> label
  | args -> label ^ "(" ^ String.concat ", " args ^ ")"

let label_of_impl i =
  match i.it with
  | Expl -> "Expl"
  | Impl -> "Impl"

let label_of_eff p =
  match p.it with
  | Pure -> "P"
  | Impure -> "I"

let label_of_typ t =
  match t.it with
  | PathT _ -> "PathT"
  | PrimT _ -> "PrimT"
  | TypT -> "TypT"
  | HoleT -> "HoleT"
  | StrT _ -> "StrT"
  | FunT _ -> "FunT"
  | WrapT _ -> "WrapT"
  | EqT _ -> "EqT"
  | AsT _ -> "AsT"
  | WithT _ -> "WithT"

let label_of_dec d =
  match d.it with
  | EmptyD -> "EmptyD"
  | SeqD _ -> "SeqD"
  | VarD _ -> "VarD"
  | InclD _ -> "InclD"

let label_of_exp e =
  match e.it with
  | VarE _ -> "VarE"
  | PrimE _ -> "PrimE"
  | TypE _ -> "TypE"
  | PathE _ -> "PathE"
  | StrE _ -> "StrE"
  | FunE _ -> "FunE"
  | WrapE _ -> "WrapE"
  | RollE _ -> "RollE"
  | IfE _ -> "IfE"
  | DotE _ -> "DotE"
  | AppE _ -> "AppE"
  | UnwrapE _ -> "UnwrapE"
  | UnrollE _ -> "UnrollE"
  | RecE _ -> "RecE"
  | ImportE _ -> "ImportE"
  | AnnotE _ -> "AnnotE"

let label_of_bind b =
  match b.it with
  | EmptyB -> "EmptyB"
  | SeqB _ -> "SeqB"
  | VarB _ -> "VarB"
  | InclB _ -> "InclB"
  | TypeAssertB _ -> "TypeAssertB"


let string_of_var x = "\"" ^ x.it ^ "\""

let string_of_impl i = label_of_impl i
let string_of_eff p = label_of_eff p

let rec string_of_typ t =
  let node' = node (label_of_typ t) in
  match t.it with
  | PathT(e) -> node' [string_of_exp e]
  | PrimT(n) -> node' ["\"" ^ n ^ "\""]
  | TypT -> node' []
  | HoleT -> node' []
  | StrT(d) -> node' [string_of_dec d]
  | FunT(x, t1, t2, p, i) ->
    node' [string_of_var x; string_of_typ t1; string_of_typ t2;
      string_of_eff p; string_of_impl i]
  | WrapT(t) -> node' [string_of_typ t]
  | EqT(e) -> node' [string_of_exp e]
  | AsT(t1, t2) -> node' [string_of_typ t1; string_of_typ t2]
  | WithT(t, xs, e) ->
    node' ([string_of_typ t] @ List.map string_of_var xs @ [string_of_exp e])

and string_of_dec d =
  let node' = node (label_of_dec d) in
  match d.it with
  | EmptyD -> node' []
  | SeqD(d1, d2) -> node' [string_of_dec d1; string_of_dec d2]
  | VarD(x, t) -> node' [string_of_var x; string_of_typ t]
  | InclD(t) -> node' [string_of_typ t]

and string_of_exp e =
  let node' = node (label_of_exp e) in
  match e.it with
  | VarE(x) -> node' [string_of_var x]
  | PrimE(c) -> node' [Prim.string_of_const c]
  | TypE(t) -> node' [string_of_typ t]
  | PathE(e) -> node' [string_of_exp e]
  | StrE(b) -> node' [string_of_bind b]
  | FunE(x, t, e, i) ->
    node' [string_of_var x; string_of_typ t; string_of_exp e; string_of_impl i]
  | WrapE(x, t) -> node' [string_of_var x; string_of_typ t]
  | RollE(x, t) -> node' [string_of_var x; string_of_typ t]
  | IfE(x, e1, e2) ->
    node' [string_of_var x; string_of_exp e1; string_of_exp e2]
  | DotE(e, x) -> node' [string_of_exp e; string_of_var x]
  | AppE(x1, x2) -> node' [string_of_var x1; string_of_var x2]
  | UnwrapE(x, t) -> node' [string_of_var x; string_of_typ t]
  | UnrollE(x, t) -> node' [string_of_var x; string_of_typ t]
  | RecE(x, t, e) -> node' [string_of_var x; string_of_typ t; string_of_exp e]
  | ImportE(p) -> node' ["\"" ^ String.escaped p.it ^ "\""]
  | AnnotE(e, t) -> node' [string_of_exp e; string_of_typ t]

and string_of_bind b =
  let node' = node (label_of_bind b) in
  match b.it with
  | EmptyB -> node' []
  | SeqB(b1, b2) -> node' [string_of_bind b1; string_of_bind b2]
  | VarB(x, e) -> node' [string_of_var x; string_of_exp e]
  | InclB(e) -> node' [string_of_exp e]
  | TypeAssertB(b, e) -> node' [string_of_bool b; string_of_exp e]

(* Import *)

let rec imports_typ typ =
  match typ.it with
  | PathT exp -> imports_exp exp
  | PrimT _ -> []
  | TypT -> []
  | HoleT -> []
  | StrT dec -> imports_dec dec
  | FunT(_, typ1, typ2, _, _) -> imports_typ typ1 @ imports_typ typ2
  | WrapT typ -> imports_typ typ
  | EqT exp -> imports_exp exp
  | AsT(typ1, typ2) -> imports_typ typ1 @ imports_typ typ2
  | WithT(typ, _, exp) -> imports_typ typ @ imports_exp exp

and imports_dec dec =
  match dec.it with
  | EmptyD -> []
  | SeqD(dec1, dec2) -> imports_dec dec1 @ imports_dec dec2
  | VarD(_, typ) -> imports_typ typ
  | InclD typ -> imports_typ typ

and imports_exp exp =
  match exp.it with
  | VarE _ -> []
  | PrimE _ -> []
  | TypE typ -> imports_typ typ
  | PathE exp -> imports_exp exp
  | StrE bind -> imports_bind bind
  | FunE(_, typ, exp, _) -> imports_typ typ @ imports_exp exp
  | WrapE(_, typ) -> imports_typ typ
  | RollE(_, typ) -> imports_typ typ
  | IfE(_, exp1, exp2) -> imports_exp exp1 @ imports_exp exp2
  | DotE(exp, _) -> imports_exp exp
  | AppE _ -> []
  | UnwrapE(_, typ) -> imports_typ typ
  | UnrollE(_, typ) -> imports_typ typ
  | RecE(_, typ, exp) -> imports_typ typ @ imports_exp exp
  | ImportE path -> [path]
  | AnnotE(exp, typ) -> imports_exp exp @ imports_typ typ

and imports_bind bind =
  match bind.it with
  | EmptyB -> []
  | SeqB(bind1, bind2) -> imports_bind bind1 @ imports_bind bind2
  | VarB(_, exp) -> imports_exp exp
  | InclB exp -> imports_exp exp
  | TypeAssertB(_, exp) -> imports_exp exp

(* rec ... and ... *)

let rec extract_sig_exp e =
  (match e.it with
  | StrE(b) -> StrT(extract_sig_bind true b)
  | TypE _ -> TypT
  | PathE _ -> TypT
  | FunE(v, td, e, i) ->
    let tc = extract_sig_exp e in
    let p =
      if i.it == Impl then Pure else
        match e.it with
        | FunE _ -> Pure
        | TypE _ -> Pure
        | PathE _ -> Pure
        | _ -> Impure in
    FunT(v, td, tc, p@@e.at, i)
  | AnnotE(e, t) -> t.it
  | _ -> HoleT)@@e.at

and extract_sig_bind tail b =
  (match b.it with
  | EmptyB -> EmptyD
  | SeqB(bl, br) -> seqD(extract_sig_bind false bl, extract_sig_bind tail br)
  | VarB(v, e) -> VarD(v, extract_sig_exp e)
  | InclB(e) ->
    if tail then EmptyD else error b.at "... only allowed at end inside rec"
  | TypeAssertB _ -> EmptyD)@@b.at

let rec extract_subst p d =
  match d.it with
  | EmptyD -> []
  | SeqD(dl, dr) -> extract_subst p dl @ extract_subst p dr
  | VarD(v, _) -> [(v.it, DotE(p, v))]
  | InclD(_) -> assert false

let rec map_bind fn b =
  (match b.it with
  | EmptyB -> EmptyB
  | SeqB(bl, br) -> seqB(map_bind fn bl, map_bind fn br)
  | VarB(v, e) -> VarB(v, fn e)
  | InclB(e) -> assert false
  | TypeAssertB(p, e) -> TypeAssertB(p, fn e))@@b.at

let rec generalize_bind p d =
  (match d.it with
  | EmptyD -> EmptyB
  | SeqD(dl, dr) -> seqB(generalize_bind p dl, generalize_bind p dr)
  | VarD(v, t) -> VarB(v, generalize_exp (DotE(p, v)@@v.at) t)
  | InclD(_) -> assert false)@@d.at

and generalize_exp p t =
  (match t.it with
  | StrT(d) -> StrE(seqB(InclB(p)@@t.at, generalize_bind p d)@@t.at)
  | _ -> p.it)@@t.at

type 'a subst = (string * 'a) list

let subst_exp_var (s: 'a subst) (v: var) =
  try List.assoc v.it s with Not_found -> VarE(v)

let drop_subst v s = List.filter (fun (v', _) -> v.it <> v') s

let rec subst_exp_typ s t =
  (match t.it with
  | PathT(e) -> PathT(subst_exp_exp s e)
  | PrimT(s) -> PrimT(s)
  | TypT -> TypT
  | HoleT -> HoleT
  | StrT(d) ->
    let _, d = subst_exp_dec true s d in
    StrT(d)
  | FunT(v, td, tr, e, i) ->
    FunT(v, subst_exp_typ s td, subst_exp_typ (drop_subst v s) tr, e, i)
  | WrapT(t) -> WrapT(subst_exp_typ s t)
  | EqT(e) -> EqT(subst_exp_exp s e)
  | AsT(tl, tr) -> AsT(subst_exp_typ s tl, subst_exp_typ s tr)
  | WithT(t, vs, e) -> WithT(subst_exp_typ s t, vs, subst_exp_exp s e)
  )@@t.at

and subst_exp_dec tail s d =
  match d.it with
  | EmptyD -> s, EmptyD@@d.at
  | SeqD(dl, dr) ->
    let s, dl = subst_exp_dec false s dl in
    let s, dr = subst_exp_dec tail s dr in
    s, SeqD(dl, dr)@@d.at
  | VarD(v, t) ->
    drop_subst v s, VarD(v, subst_exp_typ s t)@@d.at
  | InclD(t) ->
    if tail then s, InclD(subst_exp_typ s t)@@d.at
    else error d.at "... only allowed at end inside rec"

and subst_exp_exp s e =
  (match e.it with
  | VarE(v) -> subst_exp_var s v
  | PrimE(s) -> PrimE(s)
  | TypE(t) -> TypE(subst_exp_typ s t)
  | PathE(e) -> PathE(subst_exp_exp s e)
  | StrE(b) ->
    let _, b = subst_exp_bind true s b in
    StrE(b)
  | FunE(v, t, e, i) ->
    FunE(v, subst_exp_typ s t, subst_exp_exp (drop_subst v s) e, i)
  | WrapE(v, t) -> wrapE(subst_exp_var s v@@e.at, subst_exp_typ s t)
  | RollE(v, t) -> rollE(subst_exp_var s v@@e.at, subst_exp_typ s t)
  | IfE(v, ec, ea) ->
    ifE(subst_exp_var s v@@e.at, subst_exp_exp s ec, subst_exp_exp s ea)
  | DotE(e, v) -> DotE(subst_exp_exp s e, v)
  | AppE(vf, va) -> appE(subst_exp_var s vf@@e.at, subst_exp_var s va@@e.at)
  | UnwrapE(v, t) -> unwrapE(subst_exp_var s v@@e.at, subst_exp_typ s t)
  | UnrollE(v, t) -> unrollE(subst_exp_var s v@@e.at, subst_exp_typ s t)
  | RecE(v, t, e) -> RecE(v, subst_exp_typ s t, subst_exp_exp ((v.it, VarE(v))::s) e)
  | ImportE(p) -> ImportE(p)
  | AnnotE(e, t) -> AnnotE(subst_exp_exp s e, subst_exp_typ s t)
  )@@e.at

and subst_exp_bind tail s b =
  match b.it with
  | EmptyB -> s, EmptyB@@b.at
  | SeqB(bl, br) ->
    let s, bl = subst_exp_bind false s bl in
    let s, br = subst_exp_bind tail s br in
    s, SeqB(bl, br)@@b.at
  | VarB(v, e) ->
    drop_subst v s, VarB(v, subst_exp_exp s e)@@b.at
  | InclB(e) ->
    if tail then s, InclB(subst_exp_exp s e)@@b.at
    else error b.at "... only allowed at end inside rec"
  | TypeAssertB(p, e) ->
    s, TypeAssertB(p, subst_exp_exp s e)@@b.at

let recB(b) =
  let d = extract_sig_bind false b in
  let r = uniq_var()@@b.at in
  let rv = VarE(r)@@b.at in
  let s = extract_subst rv d in
  let b' = b |> map_bind (subst_exp_exp s) in
  letB(VarB(r, RecE(r, StrT(d)@@b.at, StrE(b')@@b.at)@@b.at)@@b.at,
       generalize_bind rv d)
