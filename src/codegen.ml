
open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module_context "llclimp"
let builder = builder context
let symbol_tbl:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let bit_type = i1_type context
let integer_type = integer_type context
let address_type = pointer_type integer_type
let float_type = float_type context
let character_type = i8_type context
let cell_type = named_struct_type context "cell";;
struct_set_body cell_type [address_type address_type] false

let rec codegen_expr = function
  | Ast.Integer n -> const_int integer_type n
  | Ast.Float n -> const_float float_type n
  | Ast.String s -> const_string s
  | Ast.Nil -> const_int bit_type 0
  | Ast.T -> const_int bit_type 1
  | Ast.Symbol name ->
    (try Hashtbl.find symbol_tbl name with
     | Not_found -> raise (Error "unknown symbol"))
  | Ast.Sexp (vals) ->
    let head = vals.hd in
    if head == "quote" then Array.map codegen_expr vals.tl
    else match lookup_function head the_module with
      | Some head -> head
      | None -> raise (Error "unknown function")
let params = params head in
let args = vals.tl in
if Array.length params == Array.length args then () else
  raise (Error "incorrect # arguments passed");
let args = Array.map codegen_expr args in
build_call head args "calltmp" builder
