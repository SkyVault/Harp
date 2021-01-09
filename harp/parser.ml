(* New parser for the harp programming language *)
open Printf
open Value
open Lexer
open Common

let parse_expr (ts: token list): Value.t * token list =
  match ts with
  | (TNum v, i)::rest -> (Num (v, i), rest)
  | (TBol v, i)::rest -> (Bol (v, i), rest)
  | (TAtom v, i)::rest -> (Atom (v, i), rest)
  | (TStr v, i)::rest -> (Str (v, i), rest)
  | (v, _)::_ ->
    printf "Expr failure "; print_token v; printf "\n";
    (Nothing, [])
  | [] -> (Nothing, [])

let parse (ts: token list): Value.t =
  let rec loop (ts: token list) (vs: Value.t list) =
    match ts with
    | [] -> Progn (reverse vs)
    | rest ->
      let (v, rest') = (parse_expr rest) in
      loop rest' (v::vs)
  in
    loop ts []
