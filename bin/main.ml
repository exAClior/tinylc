type expr =
    | Var of string
    | App of expr * expr
    | Lam of char * expr

let rec pretty_print t = match t with
  | Var s -> s
  | App (t1, t2) -> "(" ^ (pretty_print t1) ^ " " ^ (pretty_print t2) ^ ")"
  | Lam (c, t0) -> "λ" ^ (String.make 1 c) ^ "." ^ (pretty_print t0)

let replace_char s old_char new_char =
  String.map (fun c -> if c = old_char then new_char else c) s

let rec alpha_conversion_helper t x y= match t with
  | Var s -> Var (replace_char s x y)
  | App (_, _) -> t 
  | Lam (z, t0) -> match z with
    | x -> Lam (y, alpha_conversion_helper t0 x y)
    | _ ->  t 

    (* needs to check collision, cannot convert the current var to x if x already exists *)
let alpha_conversion t x = match t with 
  | Lam (z, t0) -> Lam(x, alpha_conversion_helper t0 z x)
  | _ -> t


let () = let lc = Lam ('x', Var "xy") in
      let lc_alpha = alpha_conversion lc 'z' in
      print_endline (pretty_print lc_alpha)

let () = let lc = Lam ('x', Var "y") in
      let lc_alpha = alpha_conversion lc 'z' in
      print_endline (pretty_print lc_alpha)

let rec beta_reduction t = match t with
  | Var y -> Var y
  | App (t1, t2) -> App (t1, t2)
  | Lam (y, t0) -> Lam (y, t0) 



let () = let t = App (Lam ("x", Var "x"), Var "y") in 
          print_endline (pretty_print t)
