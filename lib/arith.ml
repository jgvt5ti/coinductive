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
  [@@deriving eq,ord,show,iter,map,fold,sexp]