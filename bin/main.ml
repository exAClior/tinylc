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
  | Lam (x, t0) -> "λ" ^ x ^ "." ^ (pretty_print t0)

let rec alpha_conversion t x y = match t with
  | Var z -> Var y
  | App (t1, t2) -> App (alpha_conversion t1 x y, alpha_conversion t2 x y)
  | Lam (z, t0) -> if z = x then Lam (z, alpha_conversion t0 x y) else Lam (z, t0)

(* let rec beta_reduction t x = match t with
  | Var y -> if x = y then s else Var y
  | App (t1, t2) -> App (t1, t2)
  | Lam (y, t) -> if x = y then Lam (y, t) else Lam (y, beta_reduction t x s) *)

let () = let lc = App (Lam ("x", Var "x"), Var "y") in
      let lc_alpha = alpha_conversion lc "x" "y" in
      print_endline (pretty_print lc_alpha)

let () = let t = App (Lam ("x", Var "x"), Var "y") in 
          print_endline (pretty_print t)
