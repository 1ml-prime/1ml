(*
 * (c) 2014 Andreas Rossberg
 *)

type typ =
  | BoolT
  | IntT
  | CharT
  | TextT
  | VarT

type const =
  | BoolV of bool
  | IntV of int
  | CharV of char
  | TextV of string
  | FunV of func

and func =
  { name : string;
    typ : typ list * typ list;
    fn : const list -> const list
  }

let typ_of_string = function
  | "bool" -> Some BoolT
  | "int" -> Some IntT
  | "char" -> Some CharT
  | "text" -> Some TextT
  | _ -> None

let string_of_typ = function
  | BoolT -> "bool"
  | IntT -> "int"
  | CharT -> "char"
  | TextT -> "text"
  | VarT -> assert false

let typ_of_const = function
  | BoolV _ -> BoolT
  | IntV _ -> IntT
  | CharV _ -> CharT
  | TextV _ -> TextT
  | FunV _ -> assert false

let string_of_const = function
  | BoolV(b) -> string_of_bool b
  | IntV(i) -> string_of_int i
  | CharV(c) -> "'" ^ Char.escaped c ^ "'"
  | TextV(t) -> "\"" ^ String.escaped t ^ "\""
  | FunV(f) -> "(prim " ^ f.name ^ ")"

let is_poly {typ = ts1, ts2} = List.mem VarT ts1 || List.mem VarT ts2

let typs = [BoolT; IntT; CharT; TextT]

type 'a def =
  | VoidD: unit def
  | BoolD: bool def
  | IntD: int def
  | CharD: char def
  | TextD: string def
  | VarD: const def
  | ProdD: 'a def * 'b def -> ('a * 'b) def

let (&) l r = ProdD (l, r)

let rec typs_of: type a. a def -> typ list = function
  | VoidD -> []
  | BoolD -> [BoolT]
  | IntD -> [IntT]
  | CharD -> [CharT]
  | TextD -> [TextT]
  | VarD -> [VarT]
  | ProdD (l, r) -> typs_of l @ typs_of r

let rec inj: type a. a def -> a -> const list = function
  | VoidD -> fun () -> []
  | BoolD -> fun v -> [BoolV v]
  | IntD -> fun v -> [IntV v]
  | CharD -> fun v -> [CharV v]
  | TextD -> fun v -> [TextV v]
  | VarD -> fun v -> [v]
  | ProdD (lD, rD) -> fun (l, r) -> inj lD l @ inj rD r

let rec prj: type a. a def -> const list -> a = function
  | VoidD -> (function [] -> () | _ -> failwith "unit")
  | BoolD -> (function [BoolV v] -> v | _ -> failwith "bool")
  | IntD -> (function [IntV v] -> v | _ -> failwith "int")
  | CharD -> (function [CharV v] -> v | _ -> failwith "char")
  | TextD -> (function [TextV v] -> v | _ -> failwith "text")
  | VarD -> (function [v] -> v | _ -> failwith "var")
  | ProdD (lD, rD) ->
    let lN = List.length (typs_of lD) in
    fun vs -> (prj lD (Lib.List.take lN vs), prj rD (Lib.List.drop lN vs))

let def name inD outD fn = {
    name = name;
    typ = typs_of inD, typs_of outD;
    fn = fun vs -> inj outD (fn (prj inD vs))
  }

let funs =
  [
    def "==" (VarD & VarD) BoolD (function (x1, x2) -> x1 = x2);
    def "<>" (VarD & VarD) BoolD (function (x1, x2) -> x1 <> x2);

    def "true" VoidD BoolD (fun () -> true);
    def "false" VoidD BoolD (fun () -> false);

    def "Int.+" (IntD & IntD) IntD (fun (i1, i2) -> i1 + i2);
    def "Int.-" (IntD & IntD) IntD (fun (i1, i2) -> i1 - i2);
    def "Int.*" (IntD & IntD) IntD (fun (i1, i2) -> i1 * i2);
    def "Int./" (IntD & IntD) IntD (fun (i1, i2) -> i1 / i2);
    def "Int.%" (IntD & IntD) IntD (fun (i1, i2) -> i1 mod i2);

    def "Int.<" (IntD & IntD) BoolD (fun (i1, i2) -> i1 < i2);
    def "Int.>" (IntD & IntD) BoolD (fun (i1, i2) -> i1 > i2);
    def "Int.<=" (IntD & IntD) BoolD (fun (i1, i2) -> i1 <= i2);
    def "Int.>=" (IntD & IntD) BoolD (fun (i1, i2) -> i1 >= i2);

    def "Int.print" IntD VoidD (fun i -> print_int i; flush_all ());

    def "Char.toInt" CharD IntD Char.code;
    def "Char.fromInt" IntD CharD Char.chr;

    def "Char.print" CharD VoidD (fun c -> print_char c; flush_all ());

    def "Text.++" (TextD & TextD) TextD (fun (t1, t2) -> t1 ^ t2);

    def "Text.<" (TextD & TextD) BoolD (fun (i1, i2) -> i1 < i2);
    def "Text.>" (TextD & TextD) BoolD (fun (i1, i2) -> i1 > i2);
    def "Text.<=" (TextD & TextD) BoolD (fun (i1, i2) -> i1 <= i2);
    def "Text.>=" (TextD & TextD) BoolD (fun (i1, i2) -> i1 >= i2);

    def "Text.length" TextD IntD String.length;
    def "Text.sub" (TextD & IntD) CharD (fun (t, i) -> t.[i]);
    def "Text.fromChar" CharD TextD (String.make 1);

    def "Text.print" TextD VoidD (fun t -> print_string t; flush_all ());
  ]

let fun_of_string name =
  try Some (List.find (fun f -> name = f.name) funs) with Not_found -> None
