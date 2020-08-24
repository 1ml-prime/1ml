(*
 * (c) 2014 Andreas Rossberg
 *)

(* Materialization *)

val materialize_typ : Types.typ -> Fomega.exp


(* Lifting *)

val lift :
  int -> Types.typ -> Env.env -> Types.infer ref list -> Types.infer ref list
val lift_warn :
  int -> Source.region -> Types.typ -> Env.env -> Types.infer ref list ->
    Types.infer ref list


(* Subtyping *)

type error
exception Sub of error

val string_of_error : error -> string

val sub_typ :
  Env.env -> Types.typ -> Types.typ -> Types.typ list ->
    (Types.typ * Types.typ) list * Types.infer ref list * Fomega.exp (* raise Sub *)
val sub_extyp :
  Env.env -> Types.extyp -> Types.extyp -> Types.typ list ->
    (Types.typ * Types.typ) list * Types.infer ref list * Fomega.exp (* raise Sub *)

val equal_typ :
  Env.env -> Types.typ -> Types.typ -> Types.infer ref list (* raise Sub *)
val equal_extyp :
  Env.env -> Types.extyp -> Types.extyp -> Types.infer ref list (* raise Sub *)

val subst_of_match: (Types.typ * Types.typ) list -> (Types.var * Types.typ) list

(* String conversion *)

val string_of_match: (Types.typ * Types.typ) list -> string
