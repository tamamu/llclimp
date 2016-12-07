
open Llvm

let rec main_loop () : unit =
  Printf.printf "ready> %!";
  match
    let lexbuf = Syntax.create_lexbuf @@
      Sedlexing.Utf8.from_channel stdin in
    Syntax.parse_statement lexbuf
  with
  | Some statement -> begin
      print_endline @@ Ast.show_statement statement;
      dump_value (Codegen.codegen_expr statement);
      main_loop ()
    end

  | exception Syntax.ParseError e -> begin
      print_endline @@ Syntax.string_of_ParseError e;
      main_loop ()
    end

  | None -> print_newline ()
