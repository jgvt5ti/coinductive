type binOp
  = Add
  | Sub
  | Mul
  | Div
  [@@deriving eq,ord,show]

type 'a arith
  = AVar of 'a Id.t
  | Num of int
  | Op of binOp * 'a arith * 'a arith
  [@@deriving eq,ord,show]