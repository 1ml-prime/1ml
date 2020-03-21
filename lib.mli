(*
 * (c) 2014 Andreas Rossberg
 *)

module List :
sig
  val last : 'a list -> 'a (* raise Failure *)
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list

  val for_alli : (int -> 'a -> bool) -> 'a list -> bool

  val merge_nodup : 'a list -> 'a list -> 'a list
end

module String :
sig
  val is_prefix : string -> string -> bool
  val is_suffix : string -> string -> bool
  val drop : string -> int -> string
  val drop_last : string -> int -> string
  val split : string -> int -> string * string
  val index_from_opt : string -> int -> char -> int option
  val split_on_char : char -> string -> string list
end

module Filename :
sig
  val canonic : string -> string

  val replace_ext : before: string -> after: string -> path: string -> string
end

module Sys :
sig
  val file_exists_at : string -> bool
  val directory_exists_at : string -> bool
end
