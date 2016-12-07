
open Llvm

let main () =
  Toplevel.main_loop ();
  dump_module Codegen.the_module
;;

main ()
