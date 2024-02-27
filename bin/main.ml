(* following https://dev.to/chshersh/learn-lambda-calculus-in-10-minutes-with-ocaml-56ba *)

#require "angstrom";;
open Angstrom

type expr =
    | Var of string
    | App of expr * expr
    | Lam of string * expr

let rec pretty_print t = match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (pretty_print t1) ^ " " ^ (pretty_print t2) ^ ")"
  | Lam (x, t) -> "λ" ^ x ^ "." ^ (pretty_print t)

let is_left_param = function
  | '(' -> true
  | _   -> false

let is_right_param = function
  | ')' -> true
  | _   -> false

let left_param = take_while1 is_left_param

let right_param = take_while1 is_right_param

let parens_p p = char '(' *> p <* char ')' 

let name_p =
  take_while1 (function 'a' .. 'z' -> true | _ -> false)

let var_p = name_p >>| (fun name -> Var name)

let app_p expr_p =
  let ( let* ) = (>>=) in
  let* l = parens_p expr_p in
  let* _ = char ' ' in
  let* r = parens_p expr_p in
  return (App (l, r))

let lam_p expr_p =
  let ( let* ) = (>>=) in
  let* _ = string "λ" in
  let* var = name_p in
  let* _ = char '.' in
  let* body = parens_p expr_p in
  return (Lam (var, body))

let expr_p: expr t =
  fix (fun expr_p ->
    var_p <|> app_p expr_p <|> lam_p expr_p <|> parens_p expr_p
  )

let parse str =
  match parse_string ~consume:All expr_p str with
  | Ok expr   -> Printf.printf "Success: %s\n%!" (pretty_print expr)
  | Error msg -> failwith msg


(* let () = let t = App (Lam ("x", Var "x"), Var "y") in *)
(*          print_endline (pretty_print t) *)
