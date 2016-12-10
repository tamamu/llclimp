
open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "llclimp"
let builder = builder context
let symbol_tbl:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let bit_type = i1_type context
let integer_type = i32_type context
(** work only 64-bit architecture *)
let address_type = i64_type context
let float_type = float_type context
let character_type = i8_type context
let cell_type = named_struct_type context "cell";;

struct_set_body cell_type [|address_type; address_type|] false;;

let gen_cons a_module context =
  let arg_type = Array.make 2 address_type in
  let ft = function_type address_type arg_type in
  let the_function = declare_function "cons" ft a_module in
  let p0 = param the_function 0 in
  let p1 = param the_function 1 in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    (** struct cell *cell; *)
    let cell = build_alloca cell_type "cell" builder in
    (** cell->car = p0 *)
    let car = build_struct_gep cell 0 "car" builder in
    let _ = build_store p0 car builder in
    (** cell->cdr = p1 *)
    let cdr = build_struct_gep cell 1 "cdr" builder in
    let _ = build_store p1 cdr builder in
    (** return (int)cell *)
    let addr = build_ptrtoint cell address_type "addr" builder in
    let ret_val = addr in
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
  with e->
    delete_function the_function;
    raise e;;

let gen_car a_module context =
  let arg_type = Array.make 1 address_type in
  let ft = function_type address_type arg_type in
  let the_function = declare_function "car" ft a_module in
  let p0 = param the_function 0 in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    let ptr = build_inttoptr p0 (pointer_type cell_type) "consptr" builder in
    let cons = build_struct_gep ptr 0 "cons" builder in
    let car = build_load cons "car" builder in
    let ret_val = car in
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
  with e->
    delete_function the_function;
    raise e

let gen_retint a_module context =
  let arg_type = Array.make 1 address_type in
  let ft = function_type integer_type arg_type in
  let the_function = declare_function "retint" ft a_module in
  let p0 = param the_function 0 in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    let ptr = build_inttoptr p0 (pointer_type integer_type) "vptr" builder in
    let v = build_load ptr "v" builder in
    let ret_val = v in
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
  with e->
    delete_function the_function;
    raise e

let gen_main a_module context =
  let arg_type = [||] in
  let ft = function_type (void_type context) arg_type in
  let main_function = declare_function "main" ft a_module in
  let bb = append_block context "entry" main_function in
  bb;;

gen_cons the_module context;;
gen_car the_module context;;
gen_retint the_module context;;
let main_bb = gen_main the_module context in
position_at_end main_bb builder;;

let rec codegen = function
    Ast.Sexp vals ->
    (try
       let head = match (List.hd vals) with
           Ast.Sexp sexp -> raise (Error "Not implemented: Call the S-expression")
         | Ast.Atom atom -> atom
       in
       match head with
         Ast.Quote -> raise (Error "Not implemented: QUOTE")
       | Ast.Symbol name ->
         let args = Array.of_list (try List.tl vals with Failure "tl" -> []) in
         let callee = match lookup_function name the_module with
           | Some func -> func
           | None -> raise (Error "unknown function")
         in let params = params callee in
         if Array.length params == Array.length args then () else
           raise (Error "incorrect # arguments passed");
         let args = Array.map codegen args in
         build_call callee args "calltmp" builder
       | _ -> raise (Error "Expect any function")
     with
       Failure "hd" -> const_int bit_type 0)
  | Ast.Atom atom -> codegen_expr atom

and codegen_expr = function
  | Ast.Integer n -> build_ptrtoint (define_global ".int" (const_int integer_type n) the_module) address_type ".intp" builder
  | Ast.Float n -> build_ptrtoint (define_global ".float" (const_float float_type n) the_module) address_type ".floatp" builder
  | Ast.String s -> const_string context s
  | Ast.Nil -> const_int bit_type 0
  | Ast.T -> const_int bit_type 1
  | Ast.Symbol name ->
    (try Hashtbl.find symbol_tbl name with
     | Not_found -> raise (Error "unknown symbol"))
  | _ -> raise (Error "unknown symbol")


