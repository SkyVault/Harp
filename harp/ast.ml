open Printf
open Common

type equality = Neq | Eq
type comparison = Gr | Gre | Le | Lee
type term = Minus | Plus
type factor = Div | Mul
type unary = Neg | Bang

type node =
  | Terminal
  | NumValue of float
  | AtomValue of string
  | StrValue of string
  | BolValue of bool
  | Unary of unary * node
  | Factor of factor * node * node
  | Term of term * node * node
  | Range of node * node
  | Comparison of comparison * node * node
  | Equality of equality * node * node
  | LetExpr of node * node (* Name -> Value *)
  | IfExpr of node * node * node option (* Expr -> Progn *)
  | Each of node * node * node (* Atom, Range, Progn *)
  | List of node list
  | Fun of node * node * node (* Atom Args Progn *)
  | FunCall of node * node (* Atom Params *)
  | Progn of node list
  | Expression of node

let unary_to_str = function | Neg -> "-" | Bang -> "!"
let factor_to_str = function | Div -> "/" | Mul -> "*"
let term_to_str = function | Minus -> "-" | Plus -> "+"
let comp_to_str = function | Gr -> ">" | Gre -> ">=" | Le -> "<" | Lee -> "<="
let eq_to_str = function | Eq -> "==" | Neq -> "~="

let rec list_to_str (asts : node list) : string =
  asts |> List.map (to_str) |> cat_strings

and list_to_str_app (asts : node list) (append : string) : string =
  asts |> List.map (fun s -> (s |> to_str) ^ append) |> cat_strings

and to_str (ast : node) : string =
  match ast with
  | NumValue v -> sprintf "%f" v
  | AtomValue s -> sprintf ":%s" s
  | StrValue s -> sprintf "\"%s\"" s
  | BolValue b -> sprintf "%s" (if b then "#t" else "#f")
  | Unary (u, ast') -> sprintf "%s%s" (unary_to_str u) (to_str ast')
  | Factor (f, a, b) -> sprintf "(%s %s %s)" (factor_to_str f) (to_str a) (to_str b)
  | Term (t, a, b) -> sprintf "(%s %s %s)" (term_to_str t) (to_str a) (to_str b)
  | Range (min, max) -> sprintf "(%s..%s)" (to_str min) (to_str max)
  | Comparison (t, a, b) -> sprintf "(%s %s %s)" (comp_to_str t) (to_str a) (to_str b)
  | Equality (t, a, b) -> sprintf "(%s %s %s)" (eq_to_str t) (to_str a) (to_str b)
  | LetExpr (name, value) -> sprintf "(let %s %s)" (to_str name) (to_str value)
  | IfExpr (expr, progn, el) -> begin
    match el with
    | Some elseProgn ->
      sprintf "(if %s %s %s)" (to_str expr) (to_str progn) (to_str elseProgn)
    | _ -> sprintf "(if %s %s)" (to_str expr) (to_str progn)
  end
  | Each (name, range, progn) -> sprintf "(each %s in %s %s)" (to_str name) (to_str range) (to_str progn)
  | Fun (atom, args, progn) -> sprintf "<fun %s %s %s>" (to_str atom) (to_str args) (to_str progn)
  | FunCall (atom, params) -> sprintf "<%s>(%s)" (to_str atom) (to_str params)
  | Progn ns -> sprintf "{%s }" (list_to_str ns)
  | List ns -> sprintf "[%s]" (list_to_str ns)
  | Terminal -> "EOF"
  | _ -> failwith "Unhandled ast type in to_str"
