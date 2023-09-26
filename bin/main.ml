open Transformer

let repl () =
  print_endline "Hello!";
  let prg = Parser.main Lexer.token (Lexing.from_channel stdin) in
  print_endline (Source.show_expr prg)

let _ = repl()