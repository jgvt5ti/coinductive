{
  open Parser
}

let space = ['\t' '\n' '\r' ' ']
let newline = ['\n']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let alphanum = ['0'-'9' 'a'-'z' 'A'-'Z' '_' '\'']

rule token = parse
  | space     { token lexbuf }
  | newline   { Lexing.new_line lexbuf; token lexbuf }
  | '\\'      { LAMBDA }
  | '.'       { DOT }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | ';'       { SEMICOLON }
  | '+'       { PLUS   }
  | '-'       { MINUS  }
  | '*'       { MULT   }
  | '/'       { DIV    }
  | "fix"     { FIX    }
  | "prj"     { PRJ    }
  | "inj"     { INJ    }
  | "case"    { CASE   }
  | digit digit*   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | lower alphanum*          { IDENT (Lexing.lexeme lexbuf) }
  | eof       { EOF }