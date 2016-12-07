
type atom =

  | Nil

  | T

  | Quote

  | Integer of int

  | Float of float

  | String of string

  | Symbol of string

[@@deriving show]

type statement =
  |Sexp of statement list
  |Atom of atom
[@@deriving show]
