open Printf
open Ast
open Common

let rec fun_to_lua atom args progn =
  let rec args_loop args build =
    match args with
    | (AtomValue a)::[] -> a::build
    | (AtomValue a)::rest ->
      args_loop rest (List.append [(sprintf "%s," a)] build)
    | [] -> build
    | _ -> build
  in
    match (atom, args, progn) with
    | (AtomValue a, List ns, Progn ps) ->
      (* NOTE: We need to prepend return to the last value in the progn *)
      let args_str = (args_loop ns []) |> reverse |> cat_strings in
      sprintf "local %s = function(%s)\nreturn %s\nend" (a) args_str (progn_to_lua ps)
    | _ -> failwith "fun_to_lua malformed function"

and if_to_lua expr progn else' =
  match progn with
  | Progn ns -> begin
    match else' with
    | Some (Progn es) ->
      let inner = sprintf "if %s then\nreturn%selse\nreturn%send" (expr_to_lua expr) (progn_to_lua ns) (progn_to_lua es) in
      sprintf "(function()\n%s\nend)()" inner
    | _ ->
      let inner = sprintf "if %s then\nreturn%s\nend" (expr_to_lua expr) (progn_to_lua ns) in
      sprintf "(function()\n%s\nend)()" inner
  end
  | _ -> failwith "if_to_lua expects a progn"

and list_to_lua (es : node list) : string =
  let inn = es |> List.map (fun e -> (e |> expr_to_lua) ^ ",") |> cat_strings in
  sprintf "{%s}" inn

and expr_to_lua (expr : node) : string =
  let expr_ab a mid b =
    sprintf "(%s %s %s)" (expr_to_lua a) mid (expr_to_lua b)
  in
  match expr with
  | NumValue n ->
    let s = string_of_float n in
    if (String.get s ((String.length s) - 1)) == '.' then
      s ^ "0"
    else
      s
  | Equality (eq, a, b) -> expr_ab a (eq_to_str eq) b
  | Comparison (cmp, a, b) -> expr_ab a (comp_to_str cmp) b
  | Term (t, a, b) -> expr_ab a (term_to_str t) b
  | Factor (t, a, b) -> expr_ab a (factor_to_str t) b
  | Unary (u, a) -> sprintf "(%s%s)" (unary_to_str u) (expr_to_lua a)
  | AtomValue a -> a
  | LetExpr (ident, expr') ->
    sprintf "local %s = %s" (expr_to_lua ident) (expr_to_lua expr')
  | IfExpr (expr', progn', else') -> if_to_lua expr' progn' else'
  | Fun (atom', args', progn') ->  fun_to_lua atom' args' progn'
  | List es -> list_to_lua es
  | _ -> failwith (sprintf "Unhandled node type in expr_to_lua: (%s)" (Ast.to_str expr))

and progn_to_lua (ns : node list) : string =
  let ss = ns |> List.map (fun e -> (e |> expr_to_lua) ^ "\n") in
  let rev = reverse ss in
  let last = List.hd rev in
  let body = List.tl rev |> reverse in
  sprintf "%s\nreturn%s" (body |> cat_strings) (last)

let ast_to_lua (ast : Ast.node) : string =
  match ast with
  | Progn ns -> progn_to_lua ns
  | _ -> failwith "Ast to lua expects a progn at the top"
