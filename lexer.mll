(*
 * (c) 2014 Andreas Rossberg
 *)

{
open Parser

type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}

let column_pos pos =
  pos.Lexing.pos_cnum - pos.Lexing.pos_bol

let convert_pos pos =
  { Source.file = pos.Lexing.pos_fname;
    Source.line = pos.Lexing.pos_lnum;
    Source.column = column_pos pos
  }

let region lexbuf =
  let left = convert_pos (Lexing.lexeme_start_p lexbuf) in
  let right = convert_pos (Lexing.lexeme_end_p lexbuf) in 
  {Source.left = left; Source.right = right}

let error lexbuf m = raise (Source.Error (region lexbuf, m))
let error_nest start lexbuf m =
  lexbuf.Lexing.lex_start_p <- start;
  error lexbuf m

let convert_num s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := !n*10 + Char.code s.[i] - Char.code '0'
  done;
  !n

let convert_escape = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | '\\' -> '\\'
  | '\'' -> '\''
  | '\"' -> '\"'
  | _ -> assert false

let convert_char s =
  if s.[0] <> '\\' then s.[0] else convert_escape s.[1]

let convert_text s =
  let b = Buffer.create (String.length s) in
  let i = ref 1 in
  while !i < String.length s - 1 do
    Buffer.add_char b
      (if s.[!i] <> '\\' then s.[!i] else (incr i; convert_escape s.[!i]));
    incr i
  done;
  Buffer.contents b

module Offside = struct
  type 'a monad =
    | Monad of ((Lexing.lexbuf -> Parser.token)
                -> Lexing.lexbuf
                -> (Parser.token * int) option
                -> 'a result * (Parser.token * int) option)
  and 'a result =
    | Emit of Parser.token * 'a monad
    | Return of 'a

  let return value = Monad (fun _ _ tco -> (Return value, tco))
  let unit = return ()

  let rec (>>=) (Monad xM) xyM =
    Monad (fun get_token lexbuf tco ->
      match xM get_token lexbuf tco with
      | (Emit (token, xM), tco) -> (Emit (token, xM >>= xyM), tco)
      | (Return x, tco) -> let (Monad yM) = xyM x in yM get_token lexbuf tco)

  let get =
    Monad (fun get_token lexbuf tco ->
      (Return (match tco with
              | Some tc -> tc
              | None ->
                let token = get_token lexbuf in
                let column = column_pos (Lexing.lexeme_start_p lexbuf) in
                (token, column)),
       None))
  let unget (token, column) =
    Monad (fun get_token lexbuf tco ->
      match tco with
      | Some _ -> failwith "unget"
      | None -> (Return (), Some (token, column)))

  let error message = Monad (fun _ lexbuf _ -> error lexbuf message)

  let emit token = Monad (fun _ _ tco -> (Emit (token, unit), tco))
  let emit_if bool token = if bool then emit token else unit

  let rec inside_braces break insert indent (token, column) =
    match token with
    | EOF | RBRACE -> if token = break then emit token else error "unexpected"
    | COMMA ->
      if column < indent - 2 then error "offside" else
        emit token >>= fun () ->
        get >>= inside_braces break false indent
    | _ ->
      if column < indent then error "offside" else
        emit_if (column = indent && insert) COMMA >>= fun () ->
        nest token >>= fun () ->
        get >>= inside_braces break (token <> LOCAL) indent

  and inside_local insert indent (token, column) =
    match token with
    | IN ->
      emit token
    | COMMA ->
      if column < indent - 2 then error "offside" else
        emit token >>= fun () ->
        get >>= inside_local false indent
    | _ ->
      if column < indent then
        unget (token, column) >>= fun () ->
        emit IN
      else
        emit_if (column = indent && insert) COMMA >>= fun () ->
        nest token >>= fun () ->
        get >>= inside_local (token <> LOCAL) indent

  and inside_let insert indent (token, column) =
    match token with
    | IN ->
      emit token >>= fun () ->
      get >>= fun (token, column) ->
      inside_in false column (token, column)
    | COMMA ->
      if column < indent - 2 then error "offside" else
        emit token >>= fun () ->
        get >>= inside_let false indent
    | _ ->
      if column < indent then
        emit IN >>= fun () ->
        inside_in false column (token, column)
      else
        emit_if (column = indent && insert) COMMA >>= fun () ->
        nest token >>= fun () ->
        get >>= inside_let (token <> LOCAL) indent

  and inside_in insert indent (token, column) =
    match token with
    | RBRACE | COMMA | IN | EOF | RPAR -> unget (token, column)
    | SEMI ->
      if column < indent - 2 then error "offside" else
        emit token >>= fun () ->
        get >>= inside_in false indent
    | LET when column = indent ->
      emit_if (column = indent && insert) SEMI >>= fun () ->
      emit token >>= fun () ->
      get >>= fun (token, column) ->
      inside_let false column (token, column)
    | THEN | ELSE ->
      if column < indent then error "offside" else
        emit token >>= fun () ->
        get >>= inside_in true indent
    | _ ->
      if column < indent then unget (token, column) else
        emit_if (column = indent && insert) SEMI >>= fun () ->
        nest token >>= fun () ->
        get >>= inside_in true indent

  and inside_parens (token, column) =
    match token with
    | RPAR -> emit token
    | _ -> nest token >>= fun () -> get >>= inside_parens

  and nest token =
    emit token >>= fun () ->
    match token with
    | LBRACE ->
      get >>= fun (token, column) ->
      inside_braces RBRACE false column (token, column)
    | LPAR ->
      get >>= inside_parens
    | LET ->
      get >>= fun (token, column) -> inside_let false column (token, column)
    | LOCAL ->
      get >>= fun (token, column) -> inside_local false column (token, column)
    | EQUAL | DARROW | DO ->
      get >>= fun (token, column) -> inside_in false column (token, column)
    | _ ->
      unit

  type state = (unit monad * (Parser.token * int) option) ref

  let init () =
    ref ((get >>= fun (token, column) ->
          inside_braces EOF false column (token, column)),
         None)

  let token state get_token lexbuf =
    let (Monad uM, tco) = !state in
    match uM get_token lexbuf tco with
    | (Emit (token, continue), tco) -> state := (continue, tco); token
    | (Return (), _) -> failwith "return"
end
}

let space = [' ''\t']
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let symbol = ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`']
let tick = '\''
let escape = ['n''t''\\''\'''\"']
let character = [^'"''\\''\n'] | '\\'escape

let num = digit+
let name = (letter | '_') (letter | digit | '_' | tick)*
let text = '"'character*'"'
let char = '\''character '\''

let eol = '\r'?'\n'

rule token = parse
  | "_" { HOLE }
  | "&&" { LOGICAL_AND }
  | "as" { AS }
  | "do" { DO }
  | "else" { ELSE }
  | "type_error" { TYPE_ERROR }
  | "fun" { FUN }
  | "if" { IF }
  | "in" { IN }
  | "..." { ELLIPSIS }
  | "let" { LET }
  | "||" { LOGICAL_OR }
  | "wrap" { WRAP }
  | "local" { LOCAL }
  | "primitive" { PRIMITIVE }
  | "rec" { REC }
  | "then" { THEN }
  | "type" { TYPE }
  | "unwrap" { UNWRAP }
  | "with" { WITH }
  | "@" { AT }
  | "=" { EQUAL }
  | ":" { COLON }
  | ":>" { SEAL }
  | "->" { ARROW }
  | "=>" { DARROW }
  | "." { DOT }
  | "'" { TICK }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | ";" { SEMI }
  | name as s { NAME s }
  | symbol* as s { SYM s }
  | num as s { NUM (convert_num s) }
  | char as s { CHAR (convert_char s) }
  | '\''character(eol|eof) { error lexbuf "unclosed char literal" }
  | '\''character '\\'_
    { error_nest (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape" }
  | text as s { TEXT (convert_text s) }
  | '"'character*(eol|eof) { error lexbuf "unclosed text literal" }
  | '"'character*'\\'_
    { error_nest (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape" }
  | ";;;;"_*eof { EOF }
  | ";;"[^'\n''\r']*eof { EOF }
  | ";;"[^'\n''\r']*eol { Lexing.new_line lexbuf; token lexbuf }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | space { token lexbuf }
  | eol { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { error lexbuf "illegal character" }

and comment start = parse
  | ";)" { () }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; comment start lexbuf }
  | eol { Lexing.new_line lexbuf; comment start lexbuf }
  | eof { error_nest start lexbuf "unclosed comment" }
  | _ { comment start lexbuf }
