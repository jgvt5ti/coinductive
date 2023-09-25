type binOp
  = Add
  | Sub
  | Mul
  | Div

type 'a arith
  = AVar of 'a Id.t
  | Num of int
  | Op of binOp * 'a arith * 'a arith