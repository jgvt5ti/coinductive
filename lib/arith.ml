type op
  = Add
  | Sub
  | Mul
  | Div
  [@@deriving eq,ord,show,iter,map,fold,sexp]

type opl =
  | Nil
  | Cons
  | Tail
  [@@deriving eq,ord,show,iter,map,fold,sexp]

(* List -> Int *)
type size =
  | Length
  | Head
  [@@deriving eq,ord,show,iter,map,fold,sexp]

type pred =
  | Eq
  | Neq
  | Le
  | Ge
  | Lt
  | Gt
  | Eql
  | Neql
  [@@deriving eq,ord,show,iter,map,fold,sexp]

type 'a arith
  = AVar of 'a Id.t
  | Num of int
  | Op of op * 'a arith * 'a arith
  [@@deriving eq,ord,show]

let print_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let print_size = function
  | Head -> "head"
  | Length -> "size"

let print_pred = function
  | Eq  -> "="
  | Neq -> "<>"
  | Eql -> "=l"
  | Neql -> "<>l"
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"