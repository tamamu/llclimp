
open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "llclimp"
let builder = builder context
let symbol_tbl:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let bit_type = i1_type context
let integer_type = i64_type context
let address_type = pointer_type integer_type
let float_type = float_type context
let character_type = i8_type context
let cell_type = named_struct_type context "cell";;
struct_set_body cell_type [|address_type; address_type|] false;

let pointers = Array.make 2 address_type in
let ft = function_type cell_type pointers in
let the_function = declare_function "cons" ft the_module in
let car = param the_function 0 in
let cdr = param the_function 1 in
let bb = append_block context "entry" the_function in
position_at_end bb builder;
try
  let ret_val = const_named_struct cell_type [|car; cdr|] in
  let _ = build_ret ret_val builder in
  Llvm_analysis.assert_valid_function the_function;
with e->
  delete_function the_function;
  raise e


let rec codegen_expr = function
  | Ast.Integer n -> const_int integer_type n
  | Ast.Float n -> const_float float_type n
  | Ast.String s -> const_string context s
  | Ast.Nil -> const_int bit_type 0
  | Ast.T -> const_int bit_type 1
  | Ast.Symbol name ->
    (try Hashtbl.find symbol_tbl name with
     | Not_found -> raise (Error "unknown symbol"))
  | _ -> raise (Error "unknown symbol")

let rec codegen_sexp = function
  | Ast.Sexp vals ->
    try
      let head = match List.hd vals with
        |Ast.Sexp sexp -> codegen_sexp sexp
        |Ast.Atom atom -> atom
      in
      if head == Ast.Quote then
        try
          let rest = List.tl vals in
          List.map codegen rest
        with
          Failure "tl" -> raise (Error "wrong number of args to QUOTE")
      else
        let args = Array.of_list (try List.tl vals with Failure "tl" -> []) in
        let callee = match lookup_function head the_module with
          | Some head -> head
          | None -> raise (Error "unknown function")
        in let params = params callee in
        if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
        let args = Array.map codegen args in
        build_call callee args "calltmp" builder
    with
      Failure "hd" -> const_int bit_type 0

let rec codegen = function
  | Ast.Sexp stmt -> codegen_sexp stmt
  | Ast.Atom atom -> codegen_expr atom

