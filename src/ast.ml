
type atom =

  | Nil

  | T

  | Integer of int

  | Float of float

  | String of string

  | Symbol of string

[@@deriving show]

type statement =
  |Sexp of atom list
  |Atom of atom
[@@deriving show]
