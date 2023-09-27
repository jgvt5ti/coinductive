type hfl_listz =
  | Bool of bool
  | Var  of string
  | Or   of hfl_listz * hfl_listz
  | And  of hfl_listz * hfl_listz
  | Abs  of string * hfl_listz
  | App  of hfl_listz * hfl_listz
  | Int  of int
  | Op   of Arith.op * hfl_listz list
  | Opl  of Arith.opl * hfl_listz list * hfl_listz list
  | Size of Arith.size * hfl_listz
  | Pred of Arith.pred * hfl_listz list * hfl_listz list
  | Forall of string * hfl_listz
  | Exists of string * hfl_listz
  | Not of hfl_listz
  [@@deriving eq,ord,show]

type fix = Nu | Mu
  [@@deriving eq,ord,show]

type hes_rule =
  { var  : string
  ; args : string list
  ; fix  : fix
  ; body : hfl_listz
  }
  [@@deriving eq,ord,show]

type hes = hes_rule list
  [@@deriving eq,ord,show]

let rec and_fold ls = match ls with
  | [f] -> f
  | f::ls' -> And (f, and_fold ls')
  | _ -> assert false

let rec or_fold ls = match ls with
  | [f] -> f
  | f::ls' -> Or (f, or_fold ls')
  | _ -> assert false