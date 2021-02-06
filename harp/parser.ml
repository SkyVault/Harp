(* New parser for the harp programming language *)
(* open Printf *)
open Ast
open Lexer
open Common
open Printf
(* open Common *)

(*
expression     → equality ;
               | "let" atom := expression
               | "if" expr progn
progn          → "(" expression+ ")"
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

let rec parse_let_expr (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom a, _)::(TAtom ":=", _)::rest ->
     let (expr, rest') = parse_expr rest in
     (LetExpr (AtomValue a, expr), rest')
  | _ -> failwith "Malformed let expression"

and parse_if_expr (ts: token list): Ast.node * token list =
  match ts with
  | [] -> failwith "Empty if expression"
  | _ ->
     let (expr, rest') = parse_expr ts in
     let (progn, rest') = parse_progn rest' in
     match progn with
     | Progn _ -> begin
        match rest' with
        | (TAtom "else", _)::(TOpenBrace, i)::rest2 ->
          let (elseProgn, rest3) = parse_progn ((TOpenBrace, i)::rest2) in
          (IfExpr (expr, progn, Some elseProgn), rest3)
        | _ ->
          (IfExpr (expr, progn, None), rest')
      end
     | _ -> failwith "if expected a progn after the expression"

and parse_fun_def (ts: token list): Ast.node * token list =
  match ts with
  | [] -> failwith "Empty function definition"
  | (TAtom a, _)::rest' -> begin
     let atom = AtomValue a in
     let (args, rest') = parse_expr rest' in
     let (body, rest') = parse_progn rest' in
     match (atom, args, body) with
     | (AtomValue _, List _, Progn _) -> (Fun (atom, args, body), rest')
     | _ -> failwith "Function definition requires an atom, list of args and a progn"
    end
  | _ -> failwith "Function is missing a identifier"

and parse_fun_call (name : Ast.node) (ts : token list) =
  let (params, rest) = parse_list ts in
  (FunCall (name, params), rest)

and parse_list (ts: token list): Ast.node * token list =
  let rec collect ts ns =
    match ts with
    | (TCloseParen, _)::rest -> (reverse ns, rest)
    | [] -> failwith "Unbalanced parens"
    | _ ->
       let (n, rest) = parse_expr ts in
       let new_list = n::ns in
       (collect rest new_list)
  in
  match ts with
  | (TOpenParen, _)::rest ->
     let (ns, rest') = collect rest [] in
     (List ns, rest')
  | _ -> parse_expr ts

and parse_progn (ts: token list): Ast.node * token list =
  let rec collect ts ns =
    match ts with
    | (TCloseBrace, _)::rest -> (reverse ns, rest)
    | [] -> failwith "Unbalanced parens"
    | _ ->
       let (n, rest) = parse_expr ts in
       let new_list = n::ns in
       (collect rest new_list)
  in
  match ts with
  | (TOpenBrace, _)::rest ->
     let (ns, rest') = collect rest [] in
     (Progn ns, rest')
  | _ -> parse_expr ts

and parse_expr (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom "let", _)::rest -> parse_let_expr rest
  | (TAtom "if", _)::rest -> parse_if_expr rest
  | (TAtom "fun", _)::rest -> parse_fun_def rest
  | (TAtom n,_)::(TOpenParen,i)::rest ->
    parse_fun_call (AtomValue n) ((TOpenParen, i)::rest)
  | (TOpenParen, _)::_ -> parse_list ts
  (* | (TAtom "fun", _)::rest -> parse_fun_def rest *)
  | _ -> parse_equality ts

and parse_equality (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_comparison ts in
    match ts' with
    | (TAtom "=", _)::rest -> let (b, rs) = loop rest in (Equality (Eq, a, b), rs)
    | (TAtom "~=", _)::rest -> let (b, rs) = loop rest in (Equality (Neq, a, b), rs)
    | _ -> (a, ts')
  in loop ts

and parse_comparison (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_range ts in
    match ts' with
    | (TAtom ">", _)::rest -> let (b, rs) = loop rest in (Comparison (Gr, a, b), rs)
    | (TAtom ">=", _)::rest -> let (b, rs) = loop rest in (Comparison (Gre, a, b), rs)
    | (TAtom "<", _)::rest -> let (b, rs) = loop rest in (Comparison (Le, a, b), rs)
    | (TAtom "<=", _)::rest -> let (b, rs) = loop rest in (Comparison (Lee, a, b), rs)
    | _ -> (a, ts')
  in loop ts

and parse_range (ts: token list): Ast.node * token list =
  let (min, ts') = parse_term ts in
  match ts' with
  | (TRange, _)::rest ->
     let (max, rs) = parse_term rest in
     (Range (min, max), rs)
  | _ -> (min, ts')

and parse_term (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_factor ts in
    match ts' with
    | (TAtom "+", _)::rest -> let (b, rs) = loop rest in (Term (Plus, a, b), rs)
    | (TAtom "-", _)::rest -> let (b, rs) = loop rest in (Term (Minus, a, b), rs)
    | _ -> (a, ts')
  in loop ts

and parse_factor (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_unary ts in
    match ts' with
    | (TAtom "*", _)::rest -> let (b, rs) = loop rest in (Factor (Mul, a, b), rs)
    | (TAtom "/", _)::rest -> let (b, rs) = loop rest in (Factor (Div, a, b), rs)
    | _ -> (a, ts')
  in loop ts

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
  | (TAtom a, _)::rest -> (AtomValue a, rest)
  | (TOpenBrace, _)::_ -> parse_progn ts
  | (t,_)::_ ->
     failwith (sprintf "Illegal terminal token '%s'" (tok_to_str t))
  | _ -> failwith "Empty list"

let parse (ts: token list): Ast.node =
  (* Wrap the token list in parens to make it a progn *)
  let wrapped = (TOpenBrace, (0, 0))::ts@[(TCloseBrace, (0, 0))] in
  let (v, _) = parse_progn wrapped in
  v
