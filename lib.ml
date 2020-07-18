(*
 * (c) 2014 Andreas Rossberg
 *)

module List =
struct
  include List

  let rec last = function
    | x::[] -> x
    | x::xs -> last xs
    | [] -> failwith "last"

  let rec take n xs =
    match n, xs with
    | 0, _ -> []
    | n, x::xs' when n > 0 -> x :: take (n - 1) xs'
    | _ -> failwith "drop"

  let rec drop n xs =
    match n, xs with
    | 0, _ -> xs
    | n, x::xs' when n > 0 -> drop (n - 1) xs'
    | _ -> failwith "drop"

  let for_alli p xs = List.for_all (fun b -> b) (List.mapi p xs)

  let rec insert_nodup x = function
    | [] -> [x]
    | x'::xs' as xs ->
      match compare x x' with
      | 0 -> xs
      | n when n < 0 -> x::xs
      | n -> x' :: insert_nodup x xs'

  let merge_nodup xs1 xs2 = List.fold_right insert_nodup xs1 xs2
end

module String =
struct
  include String

  let is_prefix s1 s2 =
    String.length s1 <= String.length s2 &&
    s1 = String.sub s2 0 (String.length s1)

  let is_suffix s1 s2 =
    let n1 = String.length s1 in
    let n2 = String.length s2 in
    n1 <= n2 && s1 = String.sub s2 (n2 - n1) n1

  let drop s n = String.sub s n (String.length s - n)

  let drop_last s n = String.sub s 0 (String.length s - n)

  let split s n = (String.sub s 0 n, drop s n)

  let index_from_opt s i c =
    try Some (String.index_from s i c) with Not_found -> None

  let split_on_char c s =
    let rec loop ss i =
      match index_from_opt s i c with
      | None -> String.sub s i (String.length s - i) :: ss
      | Some j -> loop (String.sub s i (j - i) :: ss) (j + 1) in
    loop [] 0 |> List.rev
end

module Filename =
struct
  let canonic path =
    let rec loop p s =
      match p, s with
      |       _,      [] -> p
      |    _::_,  "."::s -> loop p s
      |   ["."], ".."::s -> loop [".."] s
      | ".."::_, ".."::s -> loop (".."::p) s
      |    _::p, ".."::s -> loop p s
      |       _,    n::s -> loop (n::p) s in
    loop [] (String.split_on_char '/' path) |> List.rev |> String.concat "/"

  let replace_ext ~before ~after ~path =
    String.drop_last path (String.length before) ^ after
end

module Sys =
struct
  open Sys

  let file_exists_at path = Sys.file_exists path && not (Sys.is_directory path)

  let directory_exists_at path = Sys.file_exists path && Sys.is_directory path
end

module Option =
struct
  let value o ~default =
    match o with
    | Some v -> v
    | None -> default

  let map xy = function
    | None -> None
    | Some x -> Some (xy x)

  let bind xO xyO =
    match xO with
    | None -> None
    | Some x -> xyO x

  let traverse xyO xs =
    let rec loop ys = function
      | [] -> Some ys
      | x::xs -> bind (xyO x) @@ fun y -> loop (y::ys) xs in
    loop [] xs |> map List.rev

  let orelse alt = function
    | None -> alt ()
    | some -> some
end
