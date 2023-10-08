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

let print_fix = function
  | Mu -> "m"
  | Nu -> "v"

let rec print_hfl f = match f with
  | Var v -> v
  | Bool true -> "true"
  | Bool false -> "false"
  | Int n -> string_of_int n
  | Op(op, [f1; f2]) ->
    "(" ^ print_hfl f1 ^ Arith.print_op op ^ print_hfl f2 ^ ")"
  | Opl(Nil, [], []) -> "[]"
  | Opl(Cons, [f1], [f2]) -> "(" ^ print_hfl f1 ^ " :: " ^ print_hfl f2 ^ ")"
  | Opl(Tail, [], [f]) -> "(tail " ^ print_hfl f ^ ")"
  | Pred (pred, [f1; f2], []) | Pred (pred, [], [f1; f2]) -> 
    "(" ^ print_hfl f1  ^ " " ^ Arith.print_pred pred ^ " " ^ print_hfl f2 ^ ")"
  | Size (size, f) -> "(" ^ Arith.print_size size ^ " " ^ print_hfl f ^ ")"
  | And(f1, f2) -> "(" ^ print_hfl f1 ^ " /\\ " ^ print_hfl f2 ^ ")"
  | Or(f1, f2) -> "(" ^ print_hfl f1 ^ " \\/ " ^ print_hfl f2 ^ ")"
  | Abs(v, f) -> "(\\" ^ v ^ "." ^ print_hfl f ^ ")"
  | App(f1, f2) -> "(" ^ print_hfl f1 ^ " " ^ print_hfl f2 ^ ")"
  | Forall(v, f) -> "∀" ^ v ^ "." ^ print_hfl f
  | Exists(v, f) -> "∃" ^ v ^ "." ^ print_hfl f
  | _ -> assert false

let print_rule rule =
  let args = List.fold_left (fun s1 -> fun s2 -> s1 ^ " " ^ s2) "" rule.args in
  rule.var ^ args ^ "=" ^ print_fix rule.fix ^ " " ^ print_hfl rule.body

let print_hes hes =
  "%HES\n" ^
  List.fold_right (fun s1 -> fun s2 -> print_rule s1 ^ ".\n" ^ s2) hes ""