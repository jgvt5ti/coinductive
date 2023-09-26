%{
  open Source
%}

%token <int> INT
%token <string> IDENT
%token LPAREN "(" RPAREN  ")"
%token LBRACK "[" RBRACK  "]"
%token PLUS "+" MINUS "-" MULT "*" DIV "/"
%token LAMBDA "\\" DOT "." SEMICOLON ";" EOF
%token FIX "fix" PRJ "prj" INJ "inj" CASE "case"

%start<Source.expr> main

%%

main:
| expr EOF { $1 }

expr:
| abs_expr { $1 }

abs_expr:
| "\\"  IDENT "." struct_expr    { mk_abs $2 $4 }
| "fix" IDENT "." struct_expr    { mk_fix $2 $4 }
| struct_expr { $1 }

struct_expr:
| "[" list_fields "]"            { mk_tuple $2  }
| "inj" INT "(" arith_expr ")"   { mk_inj $2 $4 }
| "prj" INT "(" arith_expr ")"   { mk_prj $2 $4 }
| "case" arith_expr "(" IDENT "." arith_expr ";" IDENT "." arith_expr ")"
    {mk_case $2 $4 $6 $10}
| arith_expr { $1 }

list_fields:
    vl = separated_list(SEMICOLON, struct_expr) { vl }

arith_expr:
| app_expr                 { $1             }
| arith_expr op arith_expr { mk_op $2 $1 $3 }

app_expr:
| "(" atom atom ")" { mk_app $2 $3 }
| atom { $1 }

atom:
| INT  { mk_num   $1 }
| var  { mk_var   $1 }
| "(" expr ")" { $2 }

var:
| IDENT { $1 }

op:
| "+" { Arith.Add  }
| "-" { Arith.Sub  }
| "*" { Arith.Mul  }
| "/" { Arith.Div  }