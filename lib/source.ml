open Arith
open Util
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
  let ty_id = Id.gen None in
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

let mk_apps t ls =
  let rec sub f ls = match ls with 
    | [] -> f
    | x :: ls' -> sub (App(f, x)) ls'
  in
  sub t ls

let rec fixvars env fixpoints t = match t with
  | Var v -> begin match MS.find_opt v.name env with
    | None -> Var v
    | Some (id) when SS.mem v.name fixpoints ->
      Var {v with name = (String.uppercase_ascii v.name); id = id }
    | Some (id) -> Var {v with id = id }
  end
  | Num n -> Num n
  | Op(op, s1, s2) -> Op(op, fixvars env fixpoints s1, fixvars env fixpoints s2)
  | Abs(v, s) ->
    let env = MS.add v.name v.id env in
    Abs(v, fixvars env fixpoints s)
  | App(s1, s2) -> App(fixvars env fixpoints s1, fixvars env fixpoints s2)
  | Tuple (ls) -> Tuple (List.map (fixvars env fixpoints) ls)
  | Proj (i, s) -> Proj(i, fixvars env fixpoints s)
  | InExpr (i, s) -> InExpr(i, fixvars env fixpoints s)
  | MatchExpr(v, s, s1, s2) ->
    let s = fixvars env fixpoints s in
    let env = MS.add v.name v.id env in
    let s1 = fixvars env fixpoints s1 in
    let s2 = fixvars env fixpoints s2 in
    MatchExpr(v, s, s1, s2)
  | FixExpr (v, s) ->
    let nv = {v with name = (String.uppercase_ascii v.name); is_fix = true} in
    let env = MS.add v.name v.id env in
    let fixpoints = SS.add v.name fixpoints in
    FixExpr(nv, fixvars env fixpoints s)