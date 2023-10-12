open Arith
open Id
open Util

type ty 
  = TyVar of unit Id.t
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

let rec print_ty ty = match ty with
  | TyVar v -> v.name
  | TyUnit -> "*"
  | TyInt -> "Int"
  | TyList -> "List"
  | TyFun(ty1, ty2) -> "(" ^ print_ty ty1 ^ " -> " ^ (print_ty ty2) ^ ")"

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
    "(\\" ^ v.name ^ ": " ^ (print_ty v.ty) ^ ". " ^ print_tgt t ^ ")"
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
    "(fix " ^ v.name ^ ": " ^ (print_ty v.ty) ^ ". " ^ print_tgt t ^ ")"

let rec fix_vars t = match t with
  | FixExpr(v, t) -> v.name :: fix_vars t
  | App(t1, t2) | If0Expr (_, t1, t2) -> (fix_vars t1) @ (fix_vars t2)
  | Abs(_, t) -> fix_vars t
  | MatchList(_, ls) ->
    List.concat @@ List.map (fun (_, ti) -> fix_vars ti) ls
  | _ -> []

let rec to_base t ty = match ty with
  | TyFun(t1, t2) -> 
    let v = Id.gen t1 in
    let (t', vs) = to_base (App (t, Var v)) t2 in
    (t', v :: vs)
  | _ -> (t, [])

let rec gen_constraint t = match t with
  | Var v -> v.ty, []
  | Unit -> TyUnit, []
  | Num _ -> TyInt, []
  | Nil -> TyList, []
  | Op(_, t1, t2) ->
    let (ty1, c1) = gen_constraint t1 in
    let (ty2, c2) = gen_constraint t2 in
    (TyInt, [(ty1, TyInt); (ty2, TyInt)] @ c1 @ c2)
  | Cons(t1, t2) ->
    let (ty1, c1) = gen_constraint t1 in
    let (ty2, c2) = gen_constraint t2 in
    (TyList, [(ty1, TyInt); (ty2, TyList)] @ c1 @ c2)
  | Abs(v, t1) ->
    let (ty1, c1) = gen_constraint t1 in
    (TyFun(v.ty, ty1), c1)
  | App(t1, t2) ->
    let (ty1, c1) = gen_constraint t1 in
    let (ty2, c2) = gen_constraint t2 in
    let ty = TyVar (Id.gen ()) in
    (ty, [(ty1, TyFun(ty2, ty))] @ c1 @ c2)
  | If0Expr(t0, t1, t2) ->
    let (ty0, c0) = gen_constraint t0 in
    let (ty1, c1) = gen_constraint t1 in
    let (ty2, c2) = gen_constraint t2 in
    (ty1, [(ty0, TyInt); (ty1, ty2)] @ c0 @ c1 @ c2)
  | MatchList(t0, ls) ->
    let (ty0, c0) = gen_constraint t0 in
    let pat_c pat = match pat with
      | NilPat -> []
      | ConsPat (_, v) -> [(v.ty, TyList)]
    in
    let (tys, cs) = List.split @@ List.map (fun (_, ti) -> (gen_constraint ti)) ls in
    let tybody = List.hd tys in
    let cbody = List.map (fun tyi -> (tybody, tyi)) (List.tl tys) in
    let cpats = List.concat @@ List.map (fun (pat, _) -> pat_c pat) ls in
    let cs = List.flatten cs in
    (tybody, [(ty0, TyList)] @ c0 @ cbody @ cpats @ cs)
  | FixExpr(v, t0) ->
    let (ty0, c0) = gen_constraint t0 in
    (ty0, [(ty0, v.ty)] @ c0)

let rec sbst_ty v ty ty' = match ty' with
  | TyVar v' when v = v' -> ty
  | TyVar _ | TyUnit | TyInt | TyList -> ty'
  | TyFun (ty1, ty2) -> TyFun(sbst_ty v ty ty1, sbst_ty v ty ty2)

let rec unify ls = 
  let sbst f ls = List.map (fun (ty1, ty2) -> (f ty1, f ty2)) ls in
  let compose v ty ls = List.map (fun(v', ty1) -> (v', sbst_ty v ty ty1)) ls in
  match ls with
  | [] -> []
  | (ty1, ty2) :: ls' when ty1 = ty2 -> unify ls'
  | (TyVar v, ty2) :: ls' ->
    let ls' = sbst (sbst_ty v ty2) ls' in
    let sb = unify ls' in
    (v, ty2) :: (compose v ty2 sb)
  | (ty1, TyVar v) :: ls' ->
    let ls' = sbst (sbst_ty v ty1) ls' in
    let sb = unify ls' in
    (v, ty1) :: (compose v ty1 sb)
  | (TyFun (ty11, ty12), TyFun(ty21, ty22)) :: ls' ->
    unify @@ ((ty11, ty21) :: (ty12, ty22) :: ls')
  | (ty1, ty2) :: _ ->
    print_endline "unification failed";
    print_endline (print_ty ty1);
    print_endline (print_ty ty2);
    assert false

let rec apply_sbst_ty sbst ty = match ty with
  | TyVar v -> (match List.find_opt (fun (v', _) -> v = v') sbst with
    | Some (_, ty') -> ty'
    | None -> ty)
  | TyFun (ty1, ty2) -> TyFun (apply_sbst_ty sbst ty1, apply_sbst_ty sbst ty2)
  | _ -> ty

let rec apply_sbst sbst t = match t with
  | Var v -> Var {v with ty = apply_sbst_ty sbst v.ty}
  | Unit | Num _ | Nil -> t
  | Abs(v, t1) ->
    let v = {v with ty = apply_sbst_ty sbst v.ty} in
    Abs(v, apply_sbst sbst t1)
  | App(t1, t2) ->
    App(apply_sbst sbst t1, apply_sbst sbst t2)
  | Op(op, t1, t2) ->
    Op(op, apply_sbst sbst t1, apply_sbst sbst t2)
  | Cons(t1, t2) ->
    Cons(apply_sbst sbst t1, apply_sbst sbst t2)
  | If0Expr(t0, t1, t2) ->
    If0Expr(apply_sbst sbst t0, apply_sbst sbst t1, apply_sbst sbst t2)
  | MatchList(t0, ls) ->
    let t0 = apply_sbst sbst t0 in
    let ls = List.map (fun (pat, ti) -> (pat, apply_sbst sbst ti)) ls in
    MatchList(t0, ls)
  | FixExpr(v, t1) ->
    let v = {v with ty = apply_sbst_ty sbst v.ty} in
    FixExpr(v, apply_sbst sbst t1)

let to_typed t =
  let (_, cs) = gen_constraint t in
  (* List.iter (fun (ty1, ty2) -> print_endline @@ (print_ty ty1) ^ " = " ^ (print_ty ty2)) cs; *)
  let sbst = unify cs in
  (* List.iter (fun (v, ty) -> print_endline @@ v.name ^ ": " ^ (print_ty ty)) sbst; *)
  apply_sbst sbst t

let rec ty_of_expr t = match t with
  | Var v -> v.ty
  | Unit -> TyUnit
  | Num _ | Op _ -> TyInt
  | Nil | Cons _ -> TyList
  | Abs(v, t1) -> TyFun(v.ty, ty_of_expr t1)
  | App(t1, t2) -> (match ty_of_expr t1 with
    | TyFun(ty1, ty2) when ty_of_expr t2 = ty1 -> ty2
    | _ ->
      print_endline (print_tgt t);
      print_endline @@ "t1: " ^ (print_ty (ty_of_expr t1));
      print_endline @@ "t2: " ^ (print_ty (ty_of_expr t2));
      assert false
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
  | TyUnit | TyList -> ty (* Lists always appear in argument types *)
  | TyInt -> TyFun(TyFun(TyInt, TyUnit), TyUnit)
  | TyFun(ty1, ty2) -> 
    TyFun(cps_trans_ty ty1, cps_trans_ty ty2)
  | TyVar v -> TyVar v

let rec cps_trans env t = match t with
  | Var v ->
    if SS.mem v.name env then
      t
    else
      let kty = cont_ty v.ty in
      let kvar = gen kty in
      let k = Var kvar in
      Abs(kvar, App(Var {v with ty=cps_trans_ty v.ty}, k))
  | Num _ ->
    let kty = cont_ty TyInt in
    let kvar = gen kty in
    let k = Var kvar in
    Abs(kvar, App(k, t))
  | Op(op, t1, t2) ->
    let t1' = cps_trans env t1 in
    let t2' = cps_trans env t2 in
    let x1 = gen TyInt in
    let x2 = gen TyInt in
    let kty = cont_ty TyInt in
    let kvar = gen kty in
    let k = Var kvar in
    let cont2 = Abs(x2, App(k, Op(op, Var x1, Var x2))) in
    let cont1 = Abs(x1, App(t2', cont2)) in
    Abs(kvar, App(t1', cont1))
  | Nil | Cons _ -> t
  | Abs (v, t) ->
    let v = {v with ty= cps_trans_ty v.ty} in
    let env = if v.ty = TyList then SS.add v.name env else env in
    Abs(v, cps_trans env t)
  | App (t1, t2) ->
    App(cps_trans env t1, cps_trans env t2)
  | If0Expr (t0, t1, t2) ->
    let kty = cont_ty (ty_of_expr t) in
    let kvar = Id.gen kty in
    let k = Var kvar in
    let body = If0Expr (t0, App(cps_trans env t1, k), App(cps_trans env t2, k)) in
    Abs(kvar, body)
  | MatchList(t0, ls) ->
    let kty = cont_ty (ty_of_expr t) in
    let kvar = Id.gen kty in
    let k = Var kvar in
    let f (pat, ti) = match pat with
      | NilPat -> (pat, App(cps_trans env ti, k))
    | ConsPat (_, v) ->
        let env = SS.add v.name env in
        (pat, App(cps_trans env ti, k))
    in
    Abs(kvar, MatchList(t0, List.map f ls))
  | FixExpr (v, t) ->
    let v = {v with ty= cps_trans_ty v.ty} in
    FixExpr (v, cps_trans env t)
  | Unit -> Unit

(* t: list -> int*)
let cps_trans_top lvar t =
  let top = cps_trans (SS.singleton lvar.name) t in
  (*top: list -> (int -> * ) -> * *)
  let kvar = gen TyInt in
  let cont = Abs(kvar, Unit) in
  App(App(top, Var lvar), cont)