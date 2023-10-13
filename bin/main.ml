open Transformer
open Printf

let _ =
  let start_time = Sys.time () in
  let filename = Sys.argv.(1) in
  let file = open_in filename in
  let src = Parser.main Lexer.token (Lexing.from_channel file) in
  let src = Source.fixvars Util.MS.empty Util.SS.empty src in
  (* print_endline "Source ----------------------"; *)
  (* print_endline @@ Source.show_expr src; *)
  let tgt = Trans.trans_top src in
  (* print_endline "Translated ------------------"; *)
  (* print_endline @@ Target.print_tgt tgt; *)
  (* print_endline @@ Target.show_expr tgt; *)
  let top_var = Id.gen Target.TyList in
  (* let top_ty = Target.TyUnit in *)
  let cps = Target.beta @@ (Target.cps_trans_top top_var tgt) in
  (* print_endline "CPS -------------------------"; *)
  (* print_endline @@ Target.print_tgt cps; *)
  (* print_endline @@ Target.show_expr cps; *)
  let hes = Trans.toHES top_var cps in
  (* print_endline "HFL -------------------------"; *)
  (* print_endline @@ Hfl.print_hes hes; *)
  close_in file;
  let filename = filename ^ ".in" in
  let file = open_out filename in
  output_string file (Hfl.print_hes hes);
  close_out file;
  let end_time = Sys.time () in
  printf "Execution time: %f sec\n" (end_time -. start_time);