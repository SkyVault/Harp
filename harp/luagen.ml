open Printf
open Ast
open Common

type state =
  { indent : int
  ; scope_top : string list
  ; is_expr : bool }

let fn_wrap = sprintf "(function()\n%s\nend)()"

let rec handle_last_val_in_progn it last =
  match last with
  | LetExpr (AtomValue name,_) ->
    sprintf "%s\nreturn %s" (expr_to_lua it last ~value: true) name
  | Fun (AtomValue name,_,_) ->
    sprintf "%s\nreturn %s" (expr_to_lua it last ~value: true) name
  | _ -> sprintf "return %s" (expr_to_lua it last ~value: true)

and fun_to_lua ?value:(is_value=false) it atom args progn =
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
      let fn_part = sprintf "function(%s)\n%s\nend" args_str (progn_to_lua it ps ~ret:true) in
      let inner = sprintf "local %s = nil\n%s = %s" (a) (a) fn_part in
      if is_value then fn_part
      else inner
    | _ -> failwith "fun_to_lua malformed function"

and fun_call_to_lua it atom args =
  let rec args_loop args build =
    match args with
    | expr::[] -> (expr_to_lua it expr ~value:true)::build
    | expr::rest ->
      args_loop rest (List.append [(sprintf "%s," (expr_to_lua it expr ~value:true))] build)
    | [] -> build
  in
    match (atom, args) with
    | (AtomValue a, List ns) ->
      (* NOTE: We need to prepend return to the last value in the progn *)
      let args_str = (args_loop ns []) |> reverse |> cat_strings in
      sprintf "%s(%s)" a args_str
    | _ -> failwith "fun_to_lua malformed function"


and if_to_lua it expr progn else' ~value =
  match progn with
  | Progn ns -> begin
    match else' with
    | Some (Progn es) ->
      let inner = sprintf "if %s then\n%s\nelse\n%s\nend" (expr_to_lua it expr) (progn_to_lua it ns ~ret:true) (progn_to_lua it es ~ret:true) in
      if value then fn_wrap inner
      else inner
    | _ ->
      let inner = sprintf "if %s then\n%s\nend" (expr_to_lua it expr) (progn_to_lua it ns ~ret:true) in
      if value then fn_wrap inner
      else inner
  end
  | _ -> failwith "if_to_lua expects a progn"

and each_to_lua it ident range progn ~value =
  match (ident, progn) with
  | (AtomValue i, Progn ns) -> begin
    match range with
    | AtomValue _ | Range _ ->
      let inner = sprintf "for %s in iter(%s) do\n%s\nend" i (expr_to_lua it range) (progn_to_lua it ns ~ret:false) in
      if value then fn_wrap inner
      else inner
    | List _ ->
      let inner = sprintf "for _,%s in ipairs(%s) do\n%s\nend" i (expr_to_lua it range) (progn_to_lua it ns ~ret:false) in
      if value then fn_wrap inner
      else inner
    | _ -> failwith "Can't iterate the value in each"
  end
  | _ -> failwith "Each expected an identifier"

and list_to_lua it (es : node list) : string =
  let inn = es |> List.map (fun e -> (expr_to_lua it e) ^ ",") |> cat_strings in
  sprintf "{%s}" inn

and expr_to_lua ?value:(is_value=false) it (expr : node) : string =
  let expr_ab a mid b =
    sprintf "(%s %s %s)" (expr_to_lua it a) mid (expr_to_lua it b)
  in
  match expr with
  | NumValue n ->
    let s = string_of_float n in
    if (String.get s ((String.length s) - 1)) == '.' then
      s ^ "0"
    else
      s
  | StrValue s ->
    "\"" ^ s ^ "\""
  | BolValue b -> if b then "true" else "false"
  | Equality (eq, a, b) -> expr_ab a (eq_to_str eq) b
  | Comparison (cmp, a, b) -> expr_ab a (comp_to_str cmp) b
  | Term (t, a, b) -> expr_ab a (term_to_str t) b
  | Factor (t, a, b) -> expr_ab a (factor_to_str t) b
  | Unary (u, a) -> sprintf "(%s%s)" (unary_to_str u) (expr_to_lua it a)
  | Range (min, max) -> sprintf "range(%s, %s)" (expr_to_lua it min) (expr_to_lua it max)
  | AtomValue a -> a
  | LetExpr (ident, expr') ->
    sprintf "local %s = %s;" (expr_to_lua it ident) (expr_to_lua ~value:true it expr')
  | IfExpr (expr', progn', else') -> if_to_lua it expr' progn' else' ~value:is_value
  | Each (ident', range', progn') -> each_to_lua it ident' range' progn' ~value:is_value
  | Fun (atom', args', progn') -> fun_to_lua it atom' args' progn' ~value:is_value
  | FunCall (atom', params') -> fun_call_to_lua it atom' params'
  | List es -> list_to_lua it es
  | Progn ns -> progn_to_lua it ns ~ret:true |> fn_wrap
  | _ -> failwith (sprintf "Unhandled node type in expr_to_lua: (%s)" (Ast.to_str expr))

and progn_to_lua it (ns : node list) ~ret : string =
  if (List.length ns) = 0 then ""
  else
    let (nss, last) = pop_last ns in
    let ss = nss |> List.map (fun e -> (expr_to_lua it e) ^ "\n") in
    sprintf
      "%s\n%s"
      (ss |> cat_strings |> String.trim)
      (if ret then
        (handle_last_val_in_progn it last)
      else (expr_to_lua it last)) |> String.trim

let ast_to_lua (ast : Ast.node) : string =
  let it = { indent = 0; scope_top = []; is_expr = false } in

  let pre = "require \"../luastd/range\"\nrequire \"../luastd/std\"\n" in
  match ast with
  | Progn ns -> sprintf "%s\n%s\n" pre (progn_to_lua it ns ~ret:true)
  | _ -> failwith "Ast to lua expects a progn at the top"
