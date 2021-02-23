open Ast
open Printf
(* open Tok_info *)

type types =
  | Any
  | List

type func =
  { arity : int }

type vartype =
  | VarAny
  | VarFun of func
  | VarVar

type vardef =
  { ident: string;
    t : types;
    v : vartype; }

type state =
  { env : vardef list
  ; imports : string list }

let mk_vardef n = { ident = n; t = Any; v = VarAny }
let mk_vardef_t n t' = { ident = n; t = t'; v = VarAny }
let mk_fundef n a = { ident = n; t = Any; v = VarFun { arity = a } }

let atom_to_vardef = function
  | AtomValue (_, v) -> mk_vardef v
  | _ -> failwith "Not an atom"

let atom_to_str = function | AtomValue (_, v) -> v | _ -> failwith "Not an atom"

let dump_env st =
  printf "ENV: \n";
  st.env |> List.iter (fun v -> printf "%s\n" v.ident)

let rec var_find env name =
  match env with
  | v::rest ->
    if v.ident = name then Some v
    else var_find rest name
  | _ -> None

let rec var_defined env name =
  match env with
  | { ident; _ }::rest ->
    if ident = name
    then true
    else var_defined rest name
  | _ -> false

let rec analyze_expr (state : state) (expr : node) : (node * state) =
  match expr with
  | Declaration (name, arity) ->
    (Declaration (name, arity), { state with env = (mk_fundef name arity)::state.env })
  | LetExpr (AtomValue (info, name), value) ->
    let (node, state') = analyze_expr state value in
    (LetExpr (AtomValue (info, name), node),
      { state with env = (mk_vardef name)::state'.env })
  | IfExpr (expr, Progn ifTrue, Some (Progn ifFalse)) ->
    let (a, _) = analyze_node_list state ifTrue in
    let (b, _) = analyze_node_list state ifFalse in
    (IfExpr (analyze_equality state expr, Progn a, Some (Progn b)), state)
  | IfExpr (expr, (Progn ifTrue), None) ->
    let (a, _) = analyze_node_list state ifTrue in
    (IfExpr (analyze_equality state expr, Progn a, None), state)
  | Fun (atom, List args, Progn ns) ->
    let vals = args |> List.map atom_to_vardef in
    let vardef = mk_fundef (atom_to_str atom) (List.length args) in
    let func_state = { state with env = (List.append state.env (vardef::vals)) } in
    let new_state = { state with env = vardef::state.env } in
    let (node, _) = analyze_node_list func_state ns in
    (Fun (atom, List args, Progn node), new_state)
  | List ns ->
    let (node, _) = analyze_node_list state ns in
    (List node, state)
  | Dot (a, b) ->
    let (a', state') = (analyze_expr state a) in
    let (b', state') = (analyze_expr state' b) in
    (Dot (a', b'), state')

  (* handle the import function *)
  | FunCall (AtomValue (info, "import"), List ps) -> begin
    match ps with
    | AtomValue (_, file)::[] ->
      (Terminal, { state with imports = file::state.imports })
    | StrValue file::[] ->
      (Terminal, { state with imports = file::state.imports })
    | _ -> Common.log_error info "Import expects the name of the module"
  end
  | FunCall (AtomValue (info, atom), List ps) ->
    if not (var_defined state.env atom)
    then Common.log_error info (sprintf "function '%s' is undefined" atom)
    else begin
      let fn = var_find state.env atom in
      match fn with
      | Some { ident = _; t = _;  v = VarFun a } ->
        let passed = (List.length ps) in
        if a.arity <> passed
        then Common.log_error info (sprintf "Wrong number of arguments for function '%s', got %d but expected %d" atom passed a.arity)
        else begin
          let (node, _) = analyze_node_list state ps in
          (FunCall (AtomValue (info, atom), List node), state)
        end
      | Some {ident = _; t = _; v = VarAny } ->
        (* TODO(Dustin): Arity check on passed functions *)
        let (ns, _) = analyze_node_list state ps in
        (FunCall (AtomValue (info, atom), List ns), state)
      | None | _ -> failwith (sprintf "Failed to get function '%s' from env" atom)
    end
  | Each (AtomValue (info, name), range, Progn ns) ->
    let new_state = { state with env = (mk_vardef name)::state.env } in
    let (node, _) = analyze_node_list new_state ns in
    (Each (AtomValue (info, name), analyze_equality state range, Progn node), state)
  | eq -> (analyze_equality state eq, state)

and analyze_equality state eq =
  match eq with
  | Equality (e, a, b) ->
    Equality (e, analyze_equality state a, analyze_equality state b)
  | e -> analyze_comparison state e

and analyze_comparison state comp =
  match comp with
  | Comparison (g, a, b) ->
    Comparison (g, analyze_comparison state a, analyze_comparison state b)
  | r -> analyze_range state r

and analyze_range state range =
  match range with
  | Range (min, max) ->
    Range (analyze_range state min, analyze_range state max)
  | term -> analyze_term state term

and analyze_term state te =
  match te with
  | Term (op, a, b) -> Term (op, analyze_term state a, analyze_term state b)
  | f -> analyze_factor state f

and analyze_factor state fa =
  match fa with
  | Factor (op, a, b) ->
    Factor (op, analyze_factor state a, analyze_factor state b)
  | un -> analyze_unary state un

and analyze_unary state un =
  match un with
  | Unary (Neg, prim) -> Unary (Neg, analyze_unary state prim)
  | Unary (Bang, prim) -> Unary (Bang, analyze_unary state prim)
  | p -> analyze_primary state p

and analyze_primary state prim =
  match prim with
  | AtomValue (info, name) ->
    if not (var_defined state.env name)
    then Common.log_error info (sprintf "variable '%s' is undefined" name)
    else prim
  | v -> v

and analyze_node_list (state : state) (ns : node list) : node list * state =
  match ns with
  | expr::rest ->
    let (head, state') = analyze_expr state expr in
    let (nodes, state') = (analyze_node_list state' rest) in
    (head::nodes, state')
  | _ -> (ns, state)

let analyze_ast (node : Ast.node) (state : state) : (Ast.node * state) =
    match node with
    | Progn ns ->
      let (node, state') = analyze_node_list state ns in
      (Progn node, state')
    | _ -> failwith "analyze ast expects a progn"
