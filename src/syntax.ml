
type lexbuf = {
  stream: Sedlexing.lexbuf;
  mutable pos: Lexing.position;
}

let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
              pos_fname = file;
              pos_lnum = 1;
              pos_bol = 0;
              pos_cnum = 0;
            }
  in {pos; stream}

let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
     pos_lnum = lcp.pos_lnum + 1;
     pos_bol = lcp.pos_cnum;
    }

let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos}

let lexeme {stream} = Sedlexing.Utf8.lexeme stream

(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let {pos} = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok


let exp_digit = [%sedlex.regexp? '0'..'9']
let exp_initial = [%sedlex.regexp? alphabetic | Chars "!#$%&=-+*<>?/" | lu | ll | lt | lm | lo | nl]
let exp_rest = [%sedlex.regexp? exp_initial | mn | mc | pc | cf | '.' | exp_digit]
let exp_symbol = [%sedlex.regexp? exp_initial, Star exp_rest]
let exp_integer = [%sedlex.regexp? Plus exp_digit]
let exp_float = [%sedlex.regexp? (Opt exp_digit, '.', exp_integer)
                             | (Plus exp_digit, '.', Opt exp_integer)]
let exp_string = [%sedlex.regexp? '"', Star any, '"']

let rec lex lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
    update lexbuf; new_line lexbuf;
    lex lexbuf

  | white_space ->
    update lexbuf;
    lex lexbuf

  | exp_symbol ->
    update lexbuf;
    Parser.Symbol (lexeme lexbuf)

  | exp_integer ->
    update lexbuf;
    Parser.Integer (int_of_string @@ lexeme lexbuf)

  | exp_float ->
    update lexbuf;
    Parser.Float (float_of_string @@ lexeme lexbuf)

  | exp_string ->
    update lexbuf;
    Parser.String (lexeme lexbuf)

  | ';', Star (Compl '\n'), '\n' ->
    update lexbuf; new_line lexbuf;
    lex lexbuf

  | eof ->
    update lexbuf;
    Parser.Eof

  | '(' -> update lexbuf; Parser.LParen
  | ')' -> update lexbuf; Parser.RParen

  | _ ->
    update lexbuf;
    raise_ParseError lexbuf


let parse f lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
  try
    parser lexer
  with
  | Parser.Error
  | Sedlexing.MalFormed
  | Sedlexing.InvalidCodepoint _
    -> raise_ParseError lexbuf

let parse_program lexbuf =
  parse Parser.main lexbuf


let parse_statement lexbuf =
  parse Parser.main_statement lexbuf
