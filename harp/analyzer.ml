open Ast
open Printf

type vardef =
  { ident: string; }

type state =
  { env : vardef list }

let atom_to_vardef = function
  | AtomValue v -> { ident = v }
  | _ -> failwith "Not an atom"

let dump_env st =
  printf "ENV: \n";
  st.env |> List.iter (fun v -> printf "%s\n" v.ident)

let rec var_defined env name =
  match env with
  | { ident }::rest ->
    if ident = name
    then true
    else var_defined rest name
  | _ -> false

let rec analyze_expr (state : state) (expr : node) : (node * state) =
  match expr with
  | LetExpr (AtomValue name, value) ->
    (LetExpr (AtomValue name, analyze_equality state value),
      { env = {ident = name}::state.env })
  | IfExpr (expr, Progn ifTrue, Some (Progn ifFalse)) ->
    let a = analyze_node_list state ifTrue in
    let b = analyze_node_list state ifFalse in
    (IfExpr (analyze_equality state expr, Progn a, Some (Progn b)), state)
  | IfExpr (expr, (Progn ifTrue), None) ->
    let a = analyze_node_list state ifTrue in
    (IfExpr (analyze_equality state expr, Progn a, None), state)
  | Fun (atom, List args, Progn ns) ->
    let vals = args |> List.map atom_to_vardef in
    let func_state = { env = (List.append state.env vals) } in
    let new_state = { env = (atom_to_vardef atom)::state.env } in
    (Fun (atom, List args, Progn (analyze_node_list func_state ns)), new_state)
  | FunCall (AtomValue atom, List ps) ->
    if not (var_defined state.env atom)
    then failwith (Printf.sprintf "Error:: function '%s' is undefined" atom)
    else (FunCall (AtomValue atom, List (analyze_node_list state ps)), state)
  | eq -> (analyze_equality state eq, state)

and analyze_equality state eq =
  match eq with
  | Equality (e, a, b) ->
    Equality (e, analyze_comparison state a, analyze_comparison state b)
  | e -> analyze_comparison state e

and analyze_comparison state comp =
  match comp with
  | Comparison (g, a, b) ->
    Comparison (g, analyze_range state a, analyze_range state b)
  | r -> analyze_range state r

and analyze_range state range =
  match range with
  | Range (min, max) ->
    Range (analyze_term state min, analyze_term state max)
  | term -> analyze_term state term

and analyze_term state te =
  match te with
  | Term (op, a, b) -> Term (op, analyze_factor state a, analyze_factor state b)
  | f -> analyze_factor state f

and analyze_factor state fa =
  match fa with
  | Factor (op, a, b) ->
    Factor (op, analyze_unary state a, analyze_unary state b)
  | un -> analyze_unary state un

and analyze_unary state un =
  match un with
  | Unary (Neg, prim) -> Unary (Neg, analyze_primary state prim)
  | Unary (Bang, prim) -> Unary (Bang, analyze_primary state prim)
  | p -> analyze_primary state p

and analyze_primary state prim =
  match prim with
  | AtomValue name ->
    if not (var_defined state.env name)
    then failwith (Printf.sprintf "Error:: variable '%s' is undefined" name)
    else prim
  | v -> v

and analyze_node_list (state : state) (ns : node list) : node list =
  match ns with
  | expr::rest ->
    let (head, state') = analyze_expr state expr in
    head::(analyze_node_list state' rest)
  | _ -> ns

let analyze_ast = function
  | Progn ns -> Progn (analyze_node_list { env = [
      { ident = "print" };
    ] } ns)
  | _ -> failwith "analyze ast expects a progn"
