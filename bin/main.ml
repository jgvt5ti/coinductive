open Transformer
let repl () =
  print_endline "Hello!";
  let src = Parser.main Lexer.token (Lexing.from_channel stdin) in
  let src = Source.fixvars Util.MS.empty src in
  print_endline @@ Source.show_expr src;
  let tgt = Trans.trans src in
  print_endline @@ Target.show_expr tgt;
  let hes = Trans.toHES tgt in
  print_endline @@ Hfl.print_hes hes

let _ = repl()