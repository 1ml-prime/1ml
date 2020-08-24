(*
 * (c) 2014 Andreas Rossberg
 *)

open Types
open Env
open Erase


(* Errors *)

type error =
  | Missing
  | Mismatch of typ * typ
  | Struct of lab * error
  | FunEffect of eff * eff
  | FunParam of error
  | FunResult of error
  | Type of error
  | Wrap of error
  | Left of error
  | Right of error

exception Sub of error

let quote x = "`" ^ x ^ "'"

let rec string_of_error = function
  | Missing -> "missing"
  | Mismatch(t1, t2) ->
    "mismatch (" ^ string_of_typ_sort t1 ^ " vs " ^ string_of_typ_sort t2 ^ ")"
    ^ " (trying " ^ string_of_typ t1 ^ " < " ^ string_of_typ t2 ^ ")"
  | Struct(l, e) -> "field " ^ quote l ^ ": " ^ string_of_error e
  | FunEffect(p1, p2) -> "mismatch (" ^
    string_of_eff_sort p1 ^ " vs " ^ string_of_eff_sort p2 ^ " function)"
  | FunParam e -> "function parameter type: " ^ string_of_error e
  | FunResult e -> "function result type: " ^ string_of_error e
  | Type e -> "type" ^ string_of_error e
  | Wrap e -> "wrapped" ^ string_of_error e
  | Left e -> ": " ^ string_of_error e
  | Right e -> " on right-hand side: " ^ string_of_error e

(* *)

let rec is_transparent_typ env = function
  | VarT(a, k) -> Env.mem_typ a env
  | AppT(t, ts) ->
    is_transparent_typ env t && List.for_all (is_transparent_typ env) ts
  | PrimT(_)
  | StrT(_)
  | FunT(_, _, _, _)
  | TypT(_)
  | WrapT(_)
  | LamT(_, _)
  | TupT(_)
  | DotT(_, _)
  | RecT(_, _)
  | InferT(_) -> true

let rec infer_bind isCon env t1 =
  match freshen_typ env t1 with
  | VarT(_, _) -> None
  | PrimT(_) -> None
  | StrT(tr) ->
    tr
    |> Lib.Option.traverse (fun (l, t) ->
       infer_bind isCon env t |> Lib.Option.map (fun i -> (l, i)))
    |> Lib.Option.map (fun ir ->
       (StrT(ir |> List.map (fun (l, (t, _, _)) -> (l, t))),
        ir |> List.map (fun (_, (_, zs, _)) -> zs) |> List.concat,
        IL.TupE(ir |> List.map (fun (l, (_, _, e)) -> (l, e)))))
  | FunT(aksd, td, ExT(aksc, tc), e) ->
    (match e with
    | Explicit Impure ->
      None
    | Explicit Pure | Implicit ->
      infer_bind true (add_typs aksc (add_typs aksd env)) tc
      |> Lib.Option.map (fun (tc, zs, ec) ->
         (FunT(aksd, td, ExT(aksc, tc), e),
          zs,
          let ec = IL.genE(erase_bind aksc, ec) in
          IL.genE(erase_bind aksd, IL.LamE("_", erase_typ td, ec)))))
  | TypT(s) ->
    let ExT(aks, t) = freshen_extyp env s in
    if is_transparent_typ (add_typs aks env) t then
      Some (t1, [], IL.LamE("_", erase_extyp s, IL.TupE[]))
    else if not isCon && is_small_typ t1 then
      let t, zs = guess_typ (Env.domain_typ env) BaseK in
      let s = ExT([], t) in
      Some (TypT(s), zs, IL.LamE("_", erase_extyp s, IL.TupE[]))
    else
      None
  | WrapT(_) -> None
  | LamT(_, _) -> None
  | AppT(_, _) -> None
  | TupT(_) -> None
  | DotT(_, _) -> None
  | RecT(_, _) -> None
  | InferT(_) -> None

(* Materialization *)

let rec materialize_typ = function
  | StrT(r) -> IL.TupE(map_row materialize_typ r)
  | TypT(s) -> IL.LamE("_", erase_extyp s, IL.TupE[])
  | RecT(ak, t) as t' ->
    IL.instE(IL.genE(erase_bind [ak], materialize_typ t), [erase_typ t'])
  | FunT(aks, t1, ExT([], t2), Explicit Pure) ->
    IL.genE(erase_bind aks, IL.LamE("_", erase_typ t1, materialize_typ t2))
  | _ -> assert false


(* Lifting *)

let lift bottom t env zs =
  let dom = domain_typ env in
  List.filter (fun z ->
    match !z with
    | Det _ -> false
    | Undet u ->
      u.vars <- VarSet.inter u.vars dom;
      u.level < bottom || occurs_typ u t
  ) zs


module IdSet = Set.Make(struct type t = int let compare = compare end)
let warned_already = ref IdSet.empty

let lift_warn bottom at t env zs =
  let dom = domain_typ env in
  List.filter (fun z ->
    match !z with
    | Det _ -> false
    | Undet u ->
      let local = VarSet.diff u.vars dom in
      if not (VarSet.is_empty local) then (
        u.vars <- VarSet.inter u.vars dom;
        if not (IdSet.mem u.id !warned_already) && occurs_typ u t then (
          let names = String.concat ", " (VarSet.elements local) in
          Source.warn at (
            "undetermined type " ^ string_of_typ (InferT z) ^
            " local to type " ^ names ^ " in type " ^ string_of_typ t
          )
        );
        warned_already := IdSet.add u.id !warned_already
      );
      u.level < bottom || occurs_typ u t
  ) zs


(* Subtyping *)

let extract_bind oneway env tr1 l t2 =
  try List.assoc l tr1, [], fun f l x -> IL.AppE(f, IL.DotE(x, l)) with
  | Not_found ->
    match if oneway then infer_bind false env t2 else None with
    | None -> raise (Sub (Struct(l, Missing)))
    | Some (t, zs, e) -> t, zs, fun f _ _ -> IL.AppE(f, e)

let resolve_typ z t =
  Trace.sub (lazy ("[resolve_typ] z = " ^ string_of_norm_typ (InferT(z))));
  Trace.sub (lazy ("[resolve_typ] t = " ^ string_of_norm_typ t));
  resolve_typ z t

let unify_typ t1 t2 =
  Trace.sub (lazy ("[unify_typ] t1 = " ^ string_of_norm_typ t1));
  Trace.sub (lazy ("[unify_typ] t2 = " ^ string_of_norm_typ t2));
  unify_typ t1 t2

let rec psubst (p, t) =
  match p with
  | VarT(a, k) -> a, t
  | AppT(p', ts) -> psubst (p', LamT(List.map unvarT ts, t))
  | _ -> assert false

let rec var_of = function
  | VarT(a, _) -> a
  | AppT(p', _) -> var_of p'
  | _ -> assert false

let subst_of_match su = List.map (fun (p, t) -> var_of p, t) su

let string_of_match su =
  su
  |> List.map (fun (p, t) ->
      string_of_norm_typ t ^ " / " ^ string_of_norm_typ p)
  |> String.concat ", "

let rec sub_typ oneway env t1 t2 ps =
  let lift = lift (level ()) t1 in
  Trace.sub (lazy ("[sub_typ] t1 = " ^ string_of_norm_typ t1));
  Trace.sub (lazy ("[sub_typ] t2 = " ^ string_of_norm_typ t2));
  Trace.sub (lazy ("[sub_typ] ps = " ^
    String.concat ", " (List.map string_of_norm_typ ps)));
  let su', zs', t2, ps =
    if ps <> [] then
      let su, zs' = match_typ oneway env t1 t2 ps in
  Trace.sub (lazy ("[sub_typ] su = " ^ string_of_match su));
      let t2 = subst_typ (List.map psubst su) t2 in
      let ps = List.filter (fun p -> not (List.mem_assoc p su)) ps in
      if ps <> [] then
        Trace.sub (lazy ("[sub_typ] unmatched ps = " ^
          String.concat ", " (List.map string_of_norm_typ ps)));
      su, zs', t2, ps
    else
      [], [], t2, ps
  in
  let e1 = IL.VarE("x") in
  let t1' = norm_typ t1 in
  let t2' = freshen_typ env (norm_typ t2) in
  let su, zs, e =
    match (if oneway then try_rec_from_typ t1' else None),
          (if oneway then try_rec_from_typ t2' else None) with
    | Some (unroll_t, roll_t), None when not (is_undet t2') ->
       let ts, zs, f = sub_typ oneway env unroll_t t2' ps in
       ts, lift env zs, IL.AppE(f, IL.UnrollE(e1))
    | None, Some (unroll_t, roll_t) when not (is_undet t1') ->
       let ts, zs, f = sub_typ oneway env t1' unroll_t ps in
       ts, lift env zs, IL.RollE(IL.AppE(f, e1), erase_typ roll_t)
    | _ ->
    match t1', t2' with
    | t1, FunT(aks21, t21, ExT(aks22, t22), Implicit) ->
      assert (aks22 = []);
      let su, zs, f = sub_typ oneway (add_typs aks21 env) t1 t22 ps in
      List.map (fun (p, t) -> (p, LamT(aks21, t))) su, lift env zs,
      IL.genE(erase_bind aks21, (IL.LamE("y", erase_typ t21, IL.AppE(f, e1))))

    | FunT(aks11, t11, ExT(aks12, t12), Implicit), t2 ->
      assert (aks12 = []);
      let ts1, zs1 = guess_typs (Env.domain_typ env) aks11 in
      let t1' = subst_typ (subst aks11 ts1) t12 in
      let su, zs2, f = sub_typ oneway env t1' t2 ps in
      su, zs1 @ zs2,
      IL.AppE(f, IL.AppE(IL.instE(e1, List.map erase_typ ts1),
        materialize_typ (subst_typ (subst aks11 ts1) t11)))

    | TypT(s1), TypT(s2) ->
      (match s1, s2, ps with
      | ExT(aks1, t), ExT([], p1), p2::ps' when p1 = p2 ->
        if aks1 <> [] || not (!undecidable_flag || is_small_typ t) then
          raise (Sub (Mismatch (t1, t2)));
        [(p1, t)], [], e1
      | _ ->
        let zs = try equal_extyp env s1 s2 with Sub e -> raise (Sub (Type e)) in
        [], zs, e1
      )

    | StrT(tr1), StrT(tr2) ->
      let su, zs, fs = sub_row oneway env tr1 tr2 ps in
      su, zs,
      IL.TupE(List.map2 (fun (l, _) f -> l, f l e1) tr2 fs)

    | TupT(tr1), TupT(tr2) ->
      let zs = equal_row env tr1 tr2 ps in
      [], zs, e1

    | FunT(aks1, t11, s1, Explicit p1), FunT(aks2, t21, s2, Explicit p2) ->
      if p1 = Impure && p2 = Pure then raise (Sub (FunEffect(p1, p2)));
      let env' = add_typs aks2 env in
      let su1, zs1, f1 =
        try sub_typ oneway env' t21 t11 (varTs aks1) with Sub e ->
          raise (Sub (FunParam e)) in
      let ps' = List.map (fun p -> AppT(p, varTs aks2)) ps in
      let su2, zs2, f2 =
        try sub_extyp oneway env' (subst_extyp (subst_of_match su1) s1) s2 ps'
        with Sub e -> raise (Sub (FunResult e)) in
      List.map (fun (p, t) -> (p, LamT(aks2, t))) su2, lift env (zs1 @ zs2),
      IL.genE(erase_bind aks2, (IL.LamE("y", erase_typ t21,
        IL.AppE(f2,
          IL.AppE(IL.instE(e1, List.map (fun (_, t) -> erase_typ t) su1),
            IL.AppE(f1, IL.VarE("y")))))))

    | WrapT(s1), WrapT(s2) ->
      let _, zs, f =
        try sub_extyp oneway env s1 s2 []
        with Sub e -> raise (Sub (Wrap e)) in
      [], zs, IL.TupE["wrap", IL.AppE(f, IL.DotE(e1, "wrap"))]

    | t1, WrapT(s2) when oneway && not (is_undet t1) ->
      let _, zs, f =
        try sub_extyp oneway env (ExT([], t1)) s2 []
        with Sub e -> raise (Sub (Wrap e)) in
      [], zs, IL.TupE["wrap", IL.AppE(f, e1)]

    | WrapT(ExT([], _) as s1), t2 when oneway && not (is_undet t2) ->
      let _, zs, f =
        try sub_extyp oneway env s1 (ExT([], t2)) []
        with Sub e -> raise (Sub (Wrap e)) in
      [], zs, IL.DotE(e1, "wrap")

    | AppT(t1', ts1), AppT(t2', ts2) ->
      (try
        let zs1 = equal_typ env t1' t2' in
        let zs2 = List.concat (List.map2 (equal_typ env) ts1 ts2) in
        [], zs1 @ zs2, e1
      with Sub e ->
        raise (Sub (Mismatch(t1, t2)))
      )

    | DotT(t1', l1), DotT(t2', l2) ->
      if l1 <> l2 then raise (Sub (Mismatch(t1, t2))) else
      let zs = try equal_typ env t1' t2' with Sub e ->
        raise (Sub (Mismatch(t1, t2))) in
      [], zs, e1

    | LamT(aks1, t1'), LamT(aks2, t2') ->
      if List.length aks1 <> List.length aks2 ||
         List.exists2 (fun ak1 ak2 -> snd ak1 <> snd ak2) aks1 aks2 then
        raise (Sub (Mismatch(t1, t2)));
      let zs = try
          equal_typ (add_typs aks2 env)
            (subst_typ (subst aks1 (varTs aks2)) t1') t2'
        with Sub e ->
          raise (Sub (Mismatch(t1, t2)))
      in [], lift env zs, e1

    | RecT(ak1, t1'), RecT(ak2, t2') ->
      if snd ak1 <> snd ak2 then
        raise (Sub (Mismatch(t1, t2)));
      let zs = try
          equal_typ (add_typs [ak2] env)
            (subst_typ (subst [ak1] (varTs [ak2])) t1') t2'
        with Sub e ->
          raise (Sub (Mismatch(t1, t2)))
      in [], lift env zs, e1

    | InferT(z) as t1, (TypT(_) as t2) ->
      let t11, zs1 = guess_typ (Env.domain_typ env) BaseK in
      let t1' = TypT(ExT([], t11)) in
      let su, zs2, f = sub_typ oneway env t1' t2 ps in
      if not (resolve_typ z t1') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2, IL.AppE(f, e1)

    | InferT(z) as t1, (StrT(tr2) as t2) ->
      (* TODO: row polymorphism *)
      let tzsr = map_row (fun _ -> guess_typ (Env.domain_typ env) BaseK) tr2 in
      let t1' = StrT(map_row fst tzsr) in
      let zs1 = List.concat (List.map snd (List.map snd tzsr)) in
      let su, zs2, f = sub_typ oneway env t1' t2 ps in
      if not (resolve_typ z t1') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2, IL.AppE(f, e1)

    | InferT(z) as t1, (FunT([], t21, ExT([], t22), Explicit Impure) as t2) ->
      let t11, zs1 = guess_typ (Env.domain_typ env) BaseK in
      let t12, zs2 = guess_typ (Env.domain_typ env) BaseK in
      let t1' = FunT([], t11, ExT([], t12), Explicit Impure) in
      let su, zs3, f = sub_typ oneway env t1' t2 ps in
      if not (resolve_typ z t1') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2 @ zs3, IL.AppE(f, e1)

    | InferT(z) as t1, t2 ->
      if not (resolve_typ z t2) then raise (Sub (Mismatch(t1, t2)));
      [], [], e1

    | TypT(_) as t1, (InferT(z) as t2) ->
      let t21, zs1 = guess_typ (Env.domain_typ env) BaseK in
      let t2' = TypT(ExT([], t21)) in
      let su, zs2, f = sub_typ oneway env t1 t2' ps in
      if not (resolve_typ z t2') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2, IL.AppE(f, e1)

    | StrT(tr1) as t1, (InferT(z) as t2) ->
      (* TODO: row polymorphism *)
      let tzsr = map_row (fun _ -> guess_typ (Env.domain_typ env) BaseK) tr1 in
      let t2' = StrT(map_row fst tzsr) in
      let zs1 = List.concat (List.map snd (List.map snd tzsr)) in
      let su, zs2, f = sub_typ oneway env t1 t2' ps in
      if not (resolve_typ z t2') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2, IL.AppE(f, e1)

    | FunT([], t11, ExT([], t12), Explicit p) as t1, (InferT(z) as t2) ->
      let t21, zs1 = guess_typ (Env.domain_typ env) BaseK in
      let t22, zs2 = guess_typ (Env.domain_typ env) BaseK in
      let t2' = FunT([], t21, ExT([], t22), Explicit Impure) in
      let su, zs3, f = sub_typ oneway env t1 t2' ps in
      if not (resolve_typ z t2') then raise (Sub (Mismatch(t1, t2)));
      su, zs1 @ zs2 @ zs3, IL.AppE(f, e1)

    | t1, (InferT(z) as t2) ->
      if not (resolve_typ z t1) then raise (Sub (Mismatch(t1, t2)));
      [], [], e1

    | t1', t2' when unify_typ t1' t2' ->
      [], [], e1

    | _ -> raise (Sub (Mismatch(t1, t2)))
  in
  let su = su' @ su in
  Trace.sub (lazy ("[sub_typ] done t1 = " ^ string_of_norm_typ t1));
  Trace.sub (lazy ("[sub_typ] done t2 = " ^ string_of_norm_typ t2));
  Trace.sub (lazy ("[sub_typ] done su = " ^ string_of_match su));
  Trace.sub (lazy ("[sub_typ] done x -> " ^ IL.string_of_exp e));
  su, zs' @ zs, IL.LamE("x", erase_typ t1, e)

and sub_extyp oneway env s1 s2 ps =
  Trace.sub (lazy ("[sub_extyp] s1 = " ^ string_of_norm_extyp s1));
  Trace.sub (lazy ("[sub_extyp] s2 = " ^ string_of_norm_extyp s2));
  let ExT(aks2, t2) = freshen_extyp env s2 in
  let ExT(aks1, t1) = freshen_extyp (add_typs aks2 env) s1 in
  match aks1, aks2 with
  | [], [] ->
    sub_typ oneway env t1 t2 ps
  | _ ->
    let lift = lift (level ()) t1 in
    let su, zs, f =
      sub_typ oneway (add_typs aks1 env) t1 t2 (varTs aks2) in
    [], lift env zs,
    IL.LamE("x", erase_extyp s1,
      IL.openE(IL.VarE("x"), List.map fst aks1, "y",
        IL.packE(List.map (fun (_, t) -> erase_typ t) su,
          IL.AppE(f, IL.VarE("y")), erase_extyp s2)))

and sub_row oneway env tr1 tr2 ps =
  match tr2 with
  | [] ->
    [], [], []
  | (l, t2)::tr2' ->
    Trace.sub (lazy ("[sub_row] l = " ^ l));
    let t1, zs, app = extract_bind oneway env tr1 l t2 in
    let su1, zs1, f =
      try sub_typ oneway env t1 t2 ps with
      | Sub e -> raise (Sub (Struct(l, e))) in
    let ps = List.filter (fun p -> List.mem_assoc p su1) ps in
    let su2, zs2, fs =
      sub_row oneway env tr1 (subst_row (subst_of_match su1) tr2') ps in
    su1 @ su2, zs @ zs1 @ zs2, app f::fs

and equal_typ env t1 t2 =
  Trace.sub (lazy ("[equal_typ] t1 = " ^ string_of_norm_typ t1));
  Trace.sub (lazy ("[equal_typ] t2 = " ^ string_of_norm_typ t2));
  let _, zs1, _ =
    try sub_typ false env t1 t2 [] with Sub e -> raise (Sub (Left e)) in
  let _, zs2, _ =
    try sub_typ false env t2 t1 [] with Sub e -> raise (Sub (Right e)) in
  zs1 @ zs2

and equal_extyp env s1 s2 =
  Trace.sub (lazy ("[equal_extyp] s1 = " ^ string_of_norm_extyp s1));
  Trace.sub (lazy ("[equal_extyp] s2 = " ^ string_of_norm_extyp s2));
  let _, zs1, _ =
    try sub_extyp false env s1 s2 [] with Sub e -> raise (Sub (Left e)) in
  let _, zs2, _ =
    try sub_extyp false env s2 s1 [] with Sub e -> raise (Sub (Right e)) in
  zs1 @ zs2

and equal_row env tr1 tr2 ps =
  let _, zs1, _ =
    try sub_row false env tr1 tr2 ps with Sub e -> raise (Sub (Left e)) in
  let _, zs2, _ =
    try sub_row false env tr2 tr1 ps with Sub e -> raise (Sub (Right e)) in
  zs1 @ zs2

and match_typ oneway env t1 t2 ps =
  let lift = lift (level ()) t1 in
  let t2 = norm_typ t2 in
  Trace.sub (lazy ("[match_typ] t1 = " ^ string_of_norm_typ t1));
  Trace.sub (lazy ("[match_typ] t2 = " ^ string_of_typ t2));
  Trace.sub (lazy ("[match_typ] ps = " ^
    String.concat ", " (List.map string_of_norm_typ ps)));
  if not (has_typs t2) then [], [] else
  match norm_typ t1, freshen_typ env t2 with
  | t1, FunT(aks21, t21, ExT(aks22, t22), Implicit) ->
    assert (aks22 = []);
    let su, zs = match_typ oneway (add_typs aks21 env) t1 t22 ps in
    List.map (fun (p, t) -> (p, LamT(aks21, t))) su, lift env zs

  | FunT(aks11, t11, ExT(aks12, t12), Implicit), t2 ->
    assert (aks12 = []);
    let ts1, zs1 = guess_typs (Env.domain_typ env) aks11 in
    let t1' = subst_typ (subst aks11 ts1) t12 in
    let su, zs2 = match_typ oneway env t1' t2 ps in
    su, zs1 @ zs2

  | TypT(s1), TypT(s2) ->
    (match s1, s2 with
    | ExT(aks1, t), ExT([], p) when List.mem p ps ->
      if aks1 <> [] || not (!undecidable_flag || is_small_typ t) then
         raise (Sub (Mismatch (t1, t2)));
      [(p, t)], []
    | _ ->
      [], [])

  | StrT(tr1), StrT(tr2) ->
    match_row oneway env tr1 tr2 ps

  | FunT(aks1, t11, s1, Explicit p1), FunT(aks2, t21, s2, Explicit p2) ->
    if p1 = Impure && p2 = Pure then raise (Sub (FunEffect(p1, p2)));
    if p1 <> Pure || p2 <> Pure then
      [], []
    else
      let env' = add_typs aks2 env in
      let su1, zs1, f1 =
        try sub_typ oneway env' t21 t11 (varTs aks1) with Sub e ->
          raise (Sub (FunParam e)) in
      let ps' = List.map (fun p -> AppT(p, varTs aks2)) ps in
      let su, zs2 =
        try match_extyp
              oneway env' (subst_extyp (subst_of_match su1) s1) s2 ps'
        with Sub e -> raise (Sub (FunResult e)) in
      List.map (function (AppT(p, _), t) -> (p, LamT(aks2, t))
                       | _ -> assert false) su, lift env (zs1 @ zs2)

  | _ ->
    [], []

and match_extyp oneway env s1 s2 ps =
  let ExT(aks2, t2) = freshen_extyp env s2 in
  let ExT(aks1, t1) = freshen_extyp (add_typs aks2 env) s1 in
  match aks1, aks2 with
  | [], [] ->
    match_typ oneway env t1 t2 ps
  | _ ->
    [], []

and match_row oneway env tr1 tr2 ps =
  match tr2 with
  | [] ->
    [], []
  | (l, t2)::tr2' ->
    let t1, zs, app = extract_bind oneway env tr1 l t2 in
    let su1, zs1 =
      try match_typ oneway env t1 t2 ps with
      | Sub e -> raise (Sub (Struct(l, e)))
    in
    let su2, zs2 =
      match_row oneway env tr1 (subst_row (List.map psubst su1) tr2') ps in
    su1 @ su2, zs @ zs1 @ zs2

let sub_typ = sub_typ true
let sub_extyp = sub_extyp true
