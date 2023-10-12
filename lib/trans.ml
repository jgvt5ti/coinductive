open Id
open Arith
open Util

module S = Source
module T = Target

let rec transTy tys = match tys with
  | S.TyFun(s1, s2) -> T.TyFun (transTy s1, transTy s2)
  | _ -> T.TyFun (T.TyList, T.TyInt)

let transId id =
  {id with ty = transTy id.ty}

let rec range m n = if m > n then [] else m :: range (m+1) n

let rec trans source = match source with
  | S.Var v -> T.Var (transId v)
  | S.Num n ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Num n) in
    T.Abs(argvar, T.MatchList (arg, [patNil]))
  | S.Op (op, s1, s2) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Op(op, App(trans s1, arg), App(trans s2, arg))) in
    T.Abs(argvar, T.MatchList (arg, [patNil]))
  | S.Abs (v, s) -> T.Abs (transId v, trans s)
  | S.App (s1, s2) -> T.App(trans s1, trans s2)
  | S.Tuple (ls) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Num 0) in
    let size = List.length ls in
    let nums = range 0 (size - 1) in
    let args = List.combine ls nums in
    let f = fun (s, n) ->
      let var = gen T.TyList in
      (T.ConsPat(n, var), T.App (trans s, Var var))
      in
    T.Abs(argvar, T.MatchList (arg, patNil :: (List.map f args)))
  | S.Proj (i, s) ->
    let argvar = gen T.TyList in
    let path = T.Cons (Num i, T.Var argvar) in
    let body = T.App (trans s, path) in
    T.Abs(argvar, body)
  | S.InExpr (i, s) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let patNil = (T.NilPat, T.Num i) in
    let pat0 = (T.ConsPat(0, gen T.TyList), T.App (trans s, arg)) in
    T.Abs(argvar, T.MatchList (arg, [patNil; pat0]))
  | S.FixExpr (v, t) ->
    T.FixExpr (transId v, trans t)
  | S.MatchExpr (v, s, s0, s1) ->
    let argvar = gen T.TyList in
    let arg = T.Var argvar in
    let path = T.Cons (Num 0, arg) in
    let func = trans s in
    let lambda = T.Abs (argvar, T.App (func, path)) in
    let var = transId v in    
    let t0 = T.sbst var lambda (trans s0) in
    let t1 = T.sbst var lambda (trans s1) in
    If0Expr(T.App (func, T.Nil), t0, t1)

let rec toRules fixs t = match t with
  | T.Var v -> (Hfl.Var v.name, [])
  | T.Unit -> (Hfl.Bool true, [])
  | T.Num n -> (Hfl.Int n, [])
  | T.Op(op, t1, t2) ->
    let (f1, h1) = toRules fixs t1 in
    let (f2, h2) = toRules fixs t2 in
    (Hfl.Op(op, [f1; f2]), h1 @ h2)
  | T.Abs(v, t) ->
    let (f, h) = toRules fixs t in
    (Hfl.Abs(v.name, f), h)
  | T.App(t1, t2) ->
    let (f1, h1) = toRules fixs t1 in
    let (f2, h2) = toRules fixs t2 in
    (Hfl.App(f1, f2), h1 @ h2)
  | T.Nil -> (Hfl.Opl(Nil, [], []), [])
  | T.Cons (t1, t2) -> 
    let (f1, h1) = toRules fixs t1 in
    let (f2, h2) = toRules fixs t2 in
    (Hfl.Opl(Cons, [f1], [f2]), h1 @ h2)
  | T.If0Expr (t0, t1, t2) ->
    let (f0, h0) = toRules fixs t0 in
    let (f1, h1) = toRules fixs t1 in
    let (f2, h2) = toRules fixs t2 in
    let f1 = Hfl.Or(Hfl.Pred(Neq, [Hfl.Int 0; f0], []), f1) in
    let f2 = Hfl.Or(Hfl.Pred(Eq, [Hfl.Int 0; f0], []), f2) in
    (Hfl.And(f1, f2), h0 @ h1 @ h2)
  | T.MatchList (t, pats) ->
    let (f, h) = toRules fixs t in
    let sub (pat, ti) = 
      let (fi, hi) = toRules fixs ti in match pat with
    | T.NilPat -> (Hfl.Or (Hfl.Pred(Neql, [], [Hfl.Opl(Nil, [], []); f]), fi), hi)
    | T.ConsPat (n, v) ->
      let condNil = Hfl.Pred(Eql, [], [Hfl.Opl(Nil, [], []); f]) in
      let condHead = Hfl.Pred(Neq, [Hfl.Size(Head, f); Hfl.Int n], []) in
      let (fi, hi) = toRules fixs ti in
      (Hfl.or_fold [condNil; condHead; Hfl.sbst v.name (Hfl.Opl(Arith.Tail, [],[f])) fi], hi)
    in
    let (fml, hes) = List.split @@ List.map sub pats in
    (Hfl.and_fold fml, h @ List.concat hes)
  | T.FixExpr (v, t) -> 
    let (t, vars) = T.to_base t (T.ty_of_expr t) in
    let t = T.beta t in
    let names = List.map (fun v -> v.name) vars in
    let (f, h) = toRules fixs t in
    let fvars = SS.elements @@ SS.diff (SS.diff (Hfl.free_vars f) fixs) (SS.of_list names) in
    let newv = Hfl.app_fold_vars v.name fvars in
    let f = Hfl.sbst v.name newv f in
    let hes: Hfl.hes_rule = { var = v.name; args = names @ fvars; fix = Hfl.Mu; body = f} in
    (Hfl.Var(v.name), hes :: h)

let toHES lvar t =
  let (top, rules) = toRules (SS.of_list @@ T.fix_vars t) t in
  let top = Hfl.Forall(lvar.name, top) in
  let start: Hfl.hes_rule = { var = "Sentry"; args = []; fix = Hfl.Nu; body = top} in
  start :: rules