type 'tyvar base
  = TyInt
  | TyVar of 'tyvar
  | TyProd of 'tyvar base * 'tyvar base
  | TySum of 'tyvar base * 'tyvar base
  | TyNu of 'tyvar * 'tyvar base

type 'tyvar ty 
  = TyBase of 'tyvar base
  | TyFun of 'tyvar ty * 'tyvar ty

type binOp
  = Add
  | Sub
  | Mul
  | Div

type 'avar arith
  = Num of int
  | AVar of 'avar
  | BinOp of binOp * 'avar arith * 'avar arith

type ('tyvar, 'avar) expr
  = Arith of 'avar arith
  | Var of 'avar
  | Pair of ('tyvar,'avar) expr * ('tyvar,'avar) expr
  | InExpr of int * ('tyvar,'avar) expr
  | Proj of int * ('tyvar,'avar) expr
  | MatchExpr of ('tyvar,'avar) expr * 'avar * ('tyvar,'avar) expr * ('tyvar,'avar) expr
  | Abs of 'tyvar Id.t * ('tyvar, 'avar) expr
  | FixExpr of 'avar * ('tyvar,'avar) expr
  | ZExpr of 'tyvar