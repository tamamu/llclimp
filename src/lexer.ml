open Parser

let enil = [%sedlex.regexp? "nil"]
let et = [%sedlex.regexp? 't']
let edigit = [%sedlex.regexp? '0'..'9']
let einitial = [%sedlex.regexp? alphabetic | '*' | '+' | '/' | '-' | '#' | '$' | '%' | '&' | lu | ll | lt | lm | lo | nl]
let erest = [%sedlex.regexp? mn | mc | pc | edigit | cf]
let esymbol = [%sedlex.regexp? Plus einitial, Star (einitial | erest)]
let einteger = [%sedlex.regexp? Plus edigit]
let efloat = [%sedlex.regexp? (Opt edigit, '.', einteger | Plus edigit, '.', Opt einteger)]
let estring = [%sedlex.regexp? '"', Star any, '"']
let elparen = [%sedlex.regexp? '(']
let erparen = [%sedlex.regexp? ')']
let ecolon = [%sedlex.regexp? ':']

let rec token buf =
  match%sedlex buf with
  | enil ->
    Some (NIL, buf)
  | et ->
    Some (T, buf)
  | esymbol ->
    let s = Sedlexing.Utf8.lexeme buf in
    Some (SYMBOL s, buf)
  | einteger ->
    let n = int_of_string (Sedlexing.Utf8.lexeme buf) in
    Some (INTEGER n, buf)
  | efloat ->
    let n = float_of_string (Sedlexing.Utf8.lexeme buf) in
    Some (FLOAT n, buf)
  | estring ->
    let s = Sedlexing.Utf8.lexeme buf in
    Some (STRING s, buf)
  | elparen ->
    Some (LEFT_PAREN, buf)
  | erparen ->
    Some (RIGHT_PAREN, buf)
  | ecolon ->
    Some (COLON, buf)
  | eof ->
    None
  | _ ->
    assert false

(*
 * let () =
 * let lexbuf = Sedlexing.Utf8.from_string "(cons \"hoge\" 3.14)" in
 * token lexbuf
 *)
