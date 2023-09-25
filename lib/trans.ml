open Id
open Arith

module S = Source
module T = Target

let rec transTy tys = match tys with
  | S.TyBase _ -> T.TyFun (T.TyList, T.TyInt)
  | S.TyFun(s1, s2) -> T.TyFun (transTy s1, transTy s2)

let transId id =
  {id with ty = transTy id.ty}

let rec transArith = function
    AVar v ->  AVar {v with ty = T.TyInt}
  | Num n -> Num n
  | Op (op, a1, a2) -> Op (op, transArith a1, transArith a2)

let rec trans source = match source with
  | S.Var v -> T.Var (transId v)
  | S.Arith a ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Arith (transArith a)) in
    T.Abs(argvar, T.MatchList (arg, [patNil]))
  | S.Abs (v, s) -> T.Abs (transId v, trans s)
  | S.App (s1, s2) -> T.App(trans s1, trans s2)
  | S.Pair (s1, s2) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let pat0 = (T.ConsPat(0, gen T.TyList), T.App (trans s1, arg)) in
    let pas1 = (T.ConsPat(1, gen T.TyList), T.App (trans s2, arg)) in
    T.Abs(argvar, T.MatchList (arg, [pat0; pas1]))
  | S.Proj (i, s) ->
    let argvar = gen T.TyList in
    let path = T.Cons (T.Arith (Num i), T.Var argvar) in
    let body = T.App (trans s, path) in
    T.Abs(argvar, body)
  | S.InExpr (i, s) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Arith (Num i)) in
    let pat0 = (T.ConsPat(0, gen T.TyList), T.App (trans s, arg)) in
    T.Abs(argvar, T.MatchList (arg, [patNil; pat0]))
  | S.FixExpr (v, t) ->
    T.FixExpr (transId v, trans t)
  | S.MatchExpr (v, s, s0, s1) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let path = T.Cons (T.Arith (Num 0), arg) in
    let func = trans s in
    let lambda = T.Abs (argvar, T.App (func, path)) in
    let var = transId v in    
    let t0 = T.sbst var lambda (trans s0) in
    let t1 = T.sbst var lambda (trans s1) in
    If0Expr(T.App (func, T.Nil), t0, t1)