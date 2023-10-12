open Transformer
let main () =
  print_endline "Hello!";
  let src = Parser.main Lexer.token (Lexing.from_channel stdin) in
  let src = Source.fixvars Util.MS.empty src in
  print_endline "Source ----------------------";
  print_endline @@ Source.show_expr src;
  let tgt = Target.to_typed @@ Target.beta @@ Trans.trans src in
  print_endline "Translated ------------------";
  print_endline @@ Target.print_tgt tgt;
  (* print_endline @@ Target.show_expr tgt; *)
  let top_var = Id.gen Target.TyList in
  let cps = Target.beta @@ (Target.cps_trans_top top_var tgt) in
  print_endline "CPS -------------------------";
  print_endline @@ Target.print_tgt cps;
  let hes = Trans.toHES top_var cps in
  print_endline "HFL -------------------------";
  print_endline @@ Hfl.print_hes hes

let _ = main()