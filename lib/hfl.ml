type hfl_listz =
  | Bool of bool
  | Var  of string
  | Or   of hfl_listz * hfl_listz
  | And  of hfl_listz * hfl_listz
  | Abs  of string * hfl_listz
  | App  of hfl_listz * hfl_listz
  | Int  of int
  | Op   of Arith.binOp * hfl_listz list
  | Forall of string * hfl_listz
  | Exists of string * hfl_listz
  | Not of hfl_listz
  [@@deriving eq,ord,show]