(* New parser for the harp programming language *)
(* open Printf *)
open Ast
open Lexer
open Tok_info
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

(*
 * NOTE(Dustin): We will need to fix our syntax a bit, the issue is with function calling and expressions,
 * an example of the problem: let xs := [1 (1/2) 3], the issue is that the parser thinks that its a function call 1(1/2) where 1 is the function
 * some solutions:
 * 1. Change the function call syntax
 *  we could make function calls look like: myFunc.(100 200 300), or even change the parens to brackets or something
 * 2. Introduce the comma operator to seperate elements
 * 3. only allow function calls on atoms, so no (fun (a) { a + 1 }) (32)
 * 4. Use spaces, so 1 (1/2)  isn't an issue, but 1(1/2) is.
 *  *)

let rec parse_let_expr (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom a, info)::(TAtom ":=", _)::rest ->
     let (expr, rest') = parse_expr rest in
     (LetExpr (AtomValue (info, a), expr), rest')
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

and parse_while_expr (ts: token list): Ast.node * token list =
  match ts with
  | [] -> failwith "Empty while expression"
  | _ ->
    let (expr, rest') = parse_expr ts in
    let (progn, rest') = parse_progn rest' in
    (While (expr, progn), rest')

and parse_each_expr (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom ident,_)::(TAtom "in",_)::rest -> begin
    let (range, rest') = parse_expr rest in
    match rest' with
    | (TOpenBrace, i)::rest' ->
      let (progn, rest') = (parse_progn ((TOpenBrace, i)::rest')) in
      (Each (AtomValue (i, ident), range, progn), rest')
    | _ -> failwith "Each is missing body"
  end
  | _ -> failwith "Each requires an ident and the in keyword"

and parse_fun_def (ts: token list): Ast.node * token list =
  match ts with
  | [] -> failwith "Empty function definition"
  | (TAtom a, info)::rest' -> begin
     let atom = AtomValue (info, a) in
     let (args, rest') = parse_args rest' in
     let (body, rest') = parse_progn rest' in
     match (atom, args, body) with
     | (AtomValue _, List _, Progn _) -> (Fun (atom, args, body), rest')
     | _ -> failwith "Function definition requires an atom, list of args and a progn"
    end
  | _ -> failwith "Function is missing a identifier"

and parse_fun_call (name : Ast.node) (ts : token list) =
  let (params, rest) = parse_args ts in
  (FunCall (name, params), rest)

and parse_declaration (ts : token list): Ast.node * token list =
  match ts with
  | (TAtom name,_)::(TOpenParen,i)::rest -> begin
    let (args, rest') = parse_args ((TOpenParen, i)::rest) in
    match args with
    | List as' -> (Declaration (name, (List.length as')), rest')
    | _ -> failwith "declaration expected args"
  end
  | _ -> failwith "Malformed declaration"

and parse_args (ts: token list): Ast.node * token list =
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

and parse_list (ts: token list): Ast.node * token list =
  let rec collect ts ns =
    match ts with
    | (TCloseBracket, _)::rest -> (reverse ns, rest)
    | [] -> failwith "Unbalanced parens"
    | _ ->
       let (n, rest) = parse_expr ts in
       let new_list = n::ns in
       (collect rest new_list)
  in
    match ts with
    | (TOpenBracket, _)::rest ->
      let (ns, rest') = collect rest [] in
      (List ns, rest')
    | _ -> parse_expr ts

and parse_dict (ts: token list): Ast.node * token list =
  match parse_list ts with
  | (List ns, rest) ->
    if (List.length ns) mod 2 <> 0 then
      failwith "Odd number of key value elements in dict"
    else
      (Dict ns, rest)
  | _ -> failwith "Parsing the list for dict failed"

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

and parse_expr (ts : token list): Ast.node * token list =
  let (callable, rest') = parse_expr_2 ts in
  match rest' with
  | (TOpenParen,i)::rest' ->
    if i.ws_before then
      (callable, (TOpenParen, i)::rest')
    else
      parse_fun_call (callable) ((TOpenParen, i)::rest')
  | _ -> (callable, rest')

and parse_expr_2 (ts: token list): Ast.node * token list =
  match ts with
  | (TAtom "let", _)::rest -> parse_let_expr rest
  | (TAtom "if", _)::rest -> parse_if_expr rest
  | (TAtom "while", _)::rest -> parse_while_expr rest
  | (TAtom "each", _)::rest -> parse_each_expr rest
  | (TAtom "fun", _)::rest -> parse_fun_def rest
  | (TAtom "declare", _)::rest -> parse_declaration rest
  | (TAtom "#",_)::(TOpenBracket,i)::rest ->
    parse_dict ((TOpenBracket, i)::rest)
  (* | (TAtom n, info)::(TOpenParen,i)::rest ->
   *   parse_fun_call (AtomValue (info, n)) ((TOpenParen, i)::rest) *)
  | (TOpenBracket, _)::_ -> parse_list ts
  | _ -> parse_assignment ts

and parse_assignment (ts: token list): Ast.node * token list =
  let (a, ts') = parse_equality ts in
  match ts' with
  | (TAtom "<-", _)::ts' ->
    let (b, ts') = parse_equality ts' in
    (Assignment (a, b), ts')
  | _ -> (a, ts')

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
     let p, ts = parse_dot_operator rest in (Unary (Bang, p), ts)
  | (TAtom "-", _)::rest ->
     let p, ts = parse_dot_operator rest in (Unary (Neg, p), ts)
  | _ -> parse_dot_operator ts

and parse_dot_operator (ts: token list): Ast.node * token list =
  let rec loop (ts : token list) : Ast.node * token list =
    let (a, ts') = parse_primary ts in
    match ts' with
    | (TDot,_)::rest ->
      let (b, rs) = loop rest in (Dot (a, b), rs)
    | _ -> (a, ts')
  in loop ts

and parse_primary (ts: token list): Ast.node * token list =
  match ts with
  | (TNum n, _)::rest -> (NumValue n, rest)
  | (TStr s, _)::rest -> (StrValue s, rest)
  | (TBol b, _)::rest -> (BolValue b, rest)
  | (TAtom a, info)::rest -> (AtomValue (info, a), rest)
  | (TOpenBrace, _)::_ -> parse_progn ts
  | (TOpenParen, _)::rest -> begin
    let (n, rest') = parse_expr rest in
    match rest' with
    | (TCloseParen,_)::rest' -> (n, rest')
    | _ -> failwith "Unbalanced parenthises"
  end
  | (t,_)::_ ->
     failwith (sprintf "Illegal primary token '%s'" (tok_to_str t))
  | _ -> failwith "Empty list"

let parse (ts: token list): Ast.node =
  (* Wrap the token list in parens to make it a progn *)
  let default = {line=0;column=0;ws_before=false} in
  let wrapped = (TOpenBrace, default)::ts@[(TCloseBrace, default)] in
  let (v, _) = parse_progn wrapped in
  v
