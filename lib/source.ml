open Arith

type ty
  = TyInt
  | TyVar of (ty option) Id.t
  | TyProd of ty * ty
  | TySum of ty * ty
  | TyNu of ty Id.t * ty
  | TyFun of ty * ty
  [@@deriving eq,ord,show]

type expr
  = Var of ty Id.t
  | Num of int
  | Op of op * expr * expr
  | Abs of ty Id.t * expr
  | App of expr * expr
  | Tuple of expr list
  | Proj of int * expr
  | InExpr of int * expr
  | MatchExpr of ty Id.t * expr * expr * expr
  | FixExpr of ty Id.t * expr
  [@@deriving eq,ord,show]

let mk_id v =
  let ty_id = Id.gen ~name:"X" None in
  Id.gen ~name:v (TyVar ty_id)

let mk_var v = Var(mk_id v)
let mk_num n = Num(n)
let mk_op op a1 a2 = Op(op, a1, a2)
let mk_abs v t = Abs(mk_id v, t)
let mk_app t1 t2 = App(t1, t2)
let mk_tuple ls = Tuple (ls)
let mk_prj n t = Proj(n, t)
let mk_inj n t = InExpr(n, t)
let mk_case t v t1 t2 = MatchExpr(mk_id v, t, t1, t2)
let mk_fix v t = FixExpr(mk_id v, t)