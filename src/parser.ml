type token =
  | NIL
  | T
  | SYMBOL of string
  | KEYWORD of string
  | INTEGER of int
  | FLOAT of float
  | STRING of string
  | LEFT_PAREN
  | RIGHT_PAREN
  | COLON
