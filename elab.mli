(*
 * (c) 2014 Andreas Rossberg
 *)

(* Elaboration *)

val elab : Env.env -> Syntax.exp -> Types.extyp * Types.eff * Fomega.exp
val elab_sig : Env.env -> Syntax.typ -> Types.extyp


(* Flags *)

val verify_flag : bool ref
val verify_fomega_flag : bool ref
