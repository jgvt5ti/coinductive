open Arith

type ty 
  = TyInt
  | TyList
  | TyFun of ty * ty
  [@@deriving eq,ord,show]

type pat
  = NilPat
  | ConsPat of int * ty Id.t
  [@@deriving eq,ord,show]

type expr
  = Var of ty Id.t
  | Arith of ty arith
  | Nil
  | Cons of expr * expr
  | Abs of ty Id.t * expr
  | App of expr * expr
  | If0Expr of expr * expr * expr
  | MatchList of expr * (pat * expr) list
  | FixExpr of ty Id.t * expr
  [@@deriving eq,ord,show]

let rec sbstArith v a a' = match a' with
  | AVar v1 when v == v1 -> a
  | AVar v1 -> AVar v1
  | Num n -> Num n
  | Op(op, a1, a2) -> Op(op, sbstArith v a a1, sbstArith v a a2)

let rec sbst v t t' = match t' with
  | Var v1 when v == v1 -> t
  | Var v1 -> Var v1
  | Arith a' -> begin match t with
    | Arith a -> Arith (sbstArith v a a')
    | _ -> Arith a'
    end
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (sbst v t t1, sbst v t t2)
  | Abs (arg, t1) -> Abs (arg, sbst v t t1)
  | App (t1, t2) -> App (sbst v t t1, sbst v t t2)
  | If0Expr(t0, t1, t2) -> If0Expr (sbst v t t0, sbst v t t1, sbst v t t2)
  | MatchList (t0, ls) ->
    let pats = List.map (fun (pat, t1) -> (pat, sbst v t t1)) ls in
    MatchList (sbst v t t0, pats)
  | FixExpr (var, t1) -> FixExpr(var, sbst v t t1)