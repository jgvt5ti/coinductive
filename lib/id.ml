type 'ty t =
  { name : string
  ; id   : int
  ; ty   : 'ty
  }
  [@@deriving eq,ord,show]

class counter = object
  val mutable cnt = 0
  method tick =
    let x = cnt in
    cnt <- x + 1;
    x
end
let counter = new counter
let gen_id () = counter#tick

let gen : ?name:string -> 'annot -> 'anno t =
  fun ?(name="_") ann ->
    let n = gen_id () in
    let name = if name = "_" then name ^ (string_of_int n) else name in
     { name = name
     ; id = n
     ; ty = ann
     }

let string_of_id id = id.name ^ "_" ^ (string_of_int id.id)