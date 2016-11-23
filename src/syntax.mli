
type lexbuf

val create_lexbuf:
  ?file:string -> Sedlexing.lexbuf -> lexbuf

val parse_program:
  lexbuf -> Ast.statement list

val parse_statement:
  lexbuf -> Ast.statement option


exception ParseError of (string * int * int * string)

val string_of_ParseError: (string * int * int * string) -> string
