open Arith
open Id
open Util

type ty 
  = TyVar of ty option
  | TyUnit
  | TyInt
  | TyList
  | TyFun of ty * ty
  [@@deriving eq,ord,show]

type pat
  = NilPat
  | ConsPat of int * ty Id.t
  [@@deriving eq,ord,show]

type expr
  = Var of ty Id.t
  | Unit
  | Num of int
  | Op of op * expr * expr
  | Nil
  | Cons of expr * expr
  | Abs of ty Id.t * expr
  | App of expr * expr
  | If0Expr of expr * expr * expr
  | MatchList of expr * (pat * expr) list
  | FixExpr of ty Id.t * expr
  [@@deriving eq,ord,show]

let print_pat pat = match pat with
  | NilPat -> "[] -> "
  | ConsPat (n, x) -> string_of_int n ^ "::" ^ x.name ^ " -> "

let rec print_tgt t = match t with
  | Var v -> v.name
  | Unit -> "*"
  | Num n -> string_of_int n
  | Op(op, t1, t2) ->
    "(" ^ print_tgt t1 ^ print_op op ^ print_tgt t2 ^ ")"
  | Nil -> "[]"
  | Cons(t1, t2) ->
    "(" ^ print_tgt t1 ^ "::" ^ print_tgt t2 ^ ")"
  | Abs(v, t) ->
    "(\\" ^ v.name ^ ". " ^ print_tgt t ^ ")"
  | App(t1, t2) ->
    "(" ^ print_tgt t1 ^ " " ^ print_tgt t2 ^ ")"
  | If0Expr(t0, t1, t2) ->
    "(if " ^ print_tgt t0 ^ " then " ^ print_tgt t1 ^ " else " ^ print_tgt t2 ^ ")"
  | MatchList(t, pat) ->
    let pats_str = List.map (fun (pat, t) -> print_pat pat ^ print_tgt t) pat in
    let rec f strs = match strs with
    | [] -> ""
    | [s] -> s
    | s::ls -> s ^ " | " ^ f ls in
    "(match " ^ print_tgt t ^ " with (" ^ f pats_str ^ "))"
  | FixExpr(v, t) ->
    "(fix " ^ v.name ^ ". " ^ print_tgt t ^ ")"

let rec ty_of_expr t = match t with
  | Var v -> v.ty
  | Unit -> TyUnit
  | Num _ | Op _ -> TyInt
  | Nil | Cons _ -> TyList
  | Abs(v, t1) -> TyFun(v.ty, ty_of_expr t1)
  | App(t1, t2) -> (match ty_of_expr t1 with
    | TyFun(ty1, ty2) when ty1 = ty_of_expr t2 -> ty2
    | _ -> assert false
  )
  | If0Expr(t0, t1, t2) ->
    assert (ty_of_expr t0 = TyInt);
    let ty1 = ty_of_expr t1 in
    let ty2 = ty_of_expr t2 in
    if ty1 = ty2 then ty1 else assert false
  | MatchList(t0, ls) ->
    assert (ty_of_expr t0 = TyList);
    let tys = List.map (fun (_, ti) -> ty_of_expr ti) ls in
    List.hd tys
  | FixExpr(v, t1) ->
      let ty = ty_of_expr t1 in
      assert (v.ty = ty);
      ty

let rec sbst v t t' = match t' with
  | Var v1 when v.id = v1.id -> t
  | Var v1 -> Var v1
  | Unit -> Unit
  | Num n -> Num n
  | Op(op, t1, t2) -> Op(op, sbst v t t1, sbst v t t2)
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (sbst v t t1, sbst v t t2)
  | Abs (arg, t1) -> Abs (arg, sbst v t t1)
  | App (t1, t2) -> App (sbst v t t1, sbst v t t2)
  | If0Expr(t0, t1, t2) -> If0Expr (sbst v t t0, sbst v t t1, sbst v t t2)
  | MatchList (t0, ls) ->
    let pats = List.map (fun (pat, t1) -> (pat, sbst v t t1)) ls in
    MatchList (sbst v t t0, pats)
  | FixExpr (var, t1) -> FixExpr(var, sbst v t t1)

let can_reduce t = match t with
  | App(Abs(_, _), _) | If0Expr(Num _, _, _) 
  | MatchList(Nil, _) | MatchList(Cons(Num _, _), _) -> true
  | _ -> false

let beta_ t = match t with
  | App(Abs(v, t1), t2) -> sbst v t2 t1
  | If0Expr(Num 0, t1, _) -> t1
  | If0Expr(Num _, _, t2) -> t2
  | MatchList(Nil, ls) -> 
    let (_, t) = List.find (fun (pat, _) -> pat = NilPat) ls in
    t
  | MatchList(Cons(Num n, t), ls) ->
    let (pat, ti) = List.find (fun (pat, _) -> match pat with
    | ConsPat (m, _) when n = m -> true | _ -> false) ls in
    (match pat with | ConsPat (_, v) -> sbst v t ti | _ -> assert false)
  | _ -> t

let rec beta t = match t with
  | t when can_reduce t -> beta (beta_ t)
  | App(t1, t2) when can_reduce t1 -> beta @@ App(beta_ t1, t2)
  | If0Expr(t0, t1, t2) when can_reduce t0 -> beta @@ If0Expr(beta_ t0, t1, t2)
  | MatchList(t0, ls) when can_reduce t0 -> beta @@ MatchList(beta_ t0, ls)
  | If0Expr(t0, t1, t2) -> If0Expr(t0, beta t1, beta t2)
  | MatchList(t0, ls) ->
    let ls' = List.map (fun (pat, ti) -> (pat, beta ti)) ls in
    MatchList(t0, ls')
  | FixExpr(v, t0) -> 
    FixExpr(v, beta t0)
  | Abs(v, t0) -> Abs(v, beta t0)
  | App(t1, t2) -> App(beta t1, beta t2)
  | _ -> t

let cont_ty ty = TyFun(ty, TyUnit)

let rec cps_trans_ty ty = match ty with
  | TyUnit -> TyUnit
  | TyInt | TyList -> TyFun(TyFun(ty, TyUnit), TyUnit)
  | TyFun(ty1, ty2) -> 
    let t = TyFun(cps_trans_ty ty1, cps_trans_ty ty2) in
    TyFun(TyFun(t, TyUnit), TyUnit)
  | TyVar _ -> assert false

let rec cps_trans env t = match t with
  | Var v ->
    let kty = cont_ty v.ty in
    let kvar = gen kty in
    let k = Var kvar in
    if SS.mem v.name env then
      Abs(kvar, App(k, t))
    else
      Abs(kvar, App(Var {v with ty=cps_trans_ty v.ty}, k))
  | Num _ ->
    let kty = cont_ty TyInt in
    let kvar = gen kty in
    let k = Var kvar in
    Abs(kvar, App(k, t))
  | Op(_, _, _) -> t
  | Cons(_, _) -> t
  | Nil ->
    let kty = cps_trans_ty TyList in
    let kvar = gen kty in
    let k = Var kvar in
    Abs(kvar, App(k, t))
  | Abs (v, t) ->
    let env = if v.ty = TyList then SS.add v.name env else env in
    Abs(v, cps_trans env t)
  | App (Var _, _) -> t
  | App (t1, t2) ->
    App(cps_trans env t1, cps_trans env t2)
  | If0Expr (t, t1, t2) ->
    let kty = cps_trans_ty TyUnit in
    let kvar = gen kty in
    let k = Var kvar in
    let mty = cps_trans_ty TyUnit in
    let mvar = gen mty in
    let m = Var mvar in
    let body = Abs(mvar, If0Expr (m, App(cps_trans env t1, k), App(cps_trans env t2, k))) in
    Abs(kvar, App(cps_trans env t, body))
  | MatchList(t, ls) ->
    let kty = cps_trans_ty TyUnit in
    let kvar = gen kty in
    let k = Var kvar in
    let mty = cps_trans_ty TyUnit in
    let mvar = gen mty in
    let m = Var mvar in
    let f (pat, ti) = match pat with
      | NilPat -> (pat, App(cps_trans env ti, k))
    | ConsPat (_, v) ->
        let env = SS.add v.name env in
        (pat, App(cps_trans env ti, k))
    in
    let body = Abs(mvar, MatchList(m, List.map f ls)) in
    Abs(kvar, App(cps_trans env t, body))
  | FixExpr (v, t) ->
    FixExpr (v, cps_trans env t)
  | Unit -> Unit

(* t: list -> int*)
let cps_trans_top lvar t =
  let top = cps_trans SS.empty t in
  (*top: list -> (int -> * ) -> * *)
  let kvar = gen TyInt in
  let cont = Abs(kvar, Unit) in
  App(App(top, Var lvar), cont)