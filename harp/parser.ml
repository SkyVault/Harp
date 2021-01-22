(* New parser for the harp programming language *)
(* open Printf *)
open Ast
open Lexer
(* open Common *)

(*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

let rec parse_term (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_factor ts in
    match ts' with
    | (TAtom "+", _)::rest -> let (b, rs) = loop rest in (Term (Plus, a, b), rs)
    | (TAtom "-", _)::rest -> let (b, rs) = loop rest in (Term (Minus, a, b), rs)
    | _ -> (a, ts')
  in
    loop ts

and parse_factor (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_unary ts in
    match ts' with
    | (TAtom "*", _)::rest -> let (b, rs) = loop rest in (Factor (Mul, a, b), rs)
    | (TAtom "/", _)::rest -> let (b, rs) = loop rest in (Factor (Div, a, b), rs)
    | _ -> (a, ts')
  in
    loop ts

and parse_unary (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom "!", _)::rest ->
     let p, ts = parse_primary rest in (Unary (Bang, p), ts)
  | (TAtom "-", _)::rest ->
     let p, ts = parse_primary rest in (Unary (Neg, p), ts)
  | _ -> parse_primary ts

and parse_primary (ts: token list): Ast.node * token list =
  match ts with
  | (TNum n, _)::rest -> (NumValue n, rest)
  | (TStr s, _)::rest -> (StrValue s, rest)
  | (TBol b, _)::rest -> (BolValue b, rest)
  | _ ->
     failwith "Parimary:: unhandled token"
and parse_expr (ts: token list): Ast.node * token list =
  parse_term ts

let parse (ts: token list): Ast.node =
  let (v, _) = parse_expr ts in
  v
