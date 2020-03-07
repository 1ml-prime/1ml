(*
 * (c) 2014 Andreas Rossberg
 *)

val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raises Source.Error *)

module Offside : sig
  type state
  val init : unit -> state
  val token : state
              -> (Lexing.lexbuf -> Parser.token)
              -> (Lexing.lexbuf -> Parser.token)
end
