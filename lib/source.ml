open Arith

type 'tyvar base
  = TyInt
  | TyVar of 'tyvar Id.t
  | TyProd of 'tyvar base * 'tyvar base
  | TySum of 'tyvar base * 'tyvar base
  | TyNu of 'tyvar * 'tyvar base

type 'tyvar ty 
  = TyBase of 'tyvar base
  | TyFun of 'tyvar ty * 'tyvar ty

type 'tyvar expr
  = Var of 'tyvar ty Id.t
  | Arith of unit base arith
  | Abs of 'tyvar ty Id.t * 'tyvar expr
  | App of 'tyvar expr * 'tyvar expr
  | Pair of 'tyvar expr * 'tyvar expr
  | Proj of int * 'tyvar expr
  | InExpr of int * 'tyvar expr
  | MatchExpr of 'tyvar ty Id.t * 'tyvar expr * 'tyvar expr * 'tyvar expr
  | FixExpr of 'tyvar ty Id.t * 'tyvar expr