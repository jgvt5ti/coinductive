type 'ty t =
  { name : string
  ; id   : int
  ; ty   : 'ty
  }

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
  fun ?(name="x") ann ->
     { name = name
     ; id = gen_id()
     ; ty = ann
     }