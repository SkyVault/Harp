open Printf

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
  | Comparison of node * (comparison * node) list
  | Equality of node * (equality * node) list
  | Expression of node

let cat_strings = List.fold_left (fun a b -> a ^ b) ""
let unary_to_str = function | Neg -> "-" | Bang -> "!"
let factor_to_str = function | Div -> "/" | Mul -> "*"
let term_to_str = function | Minus -> "-" | Plus -> "+"

let rec list_to_str (asts : node list) : string =
  asts |> List.map (to_str) |> cat_strings

and to_str (ast : node) : string =
  match ast with
  | NumValue v -> sprintf "%f" v
  | AtomValue s -> sprintf ":%s" s
  | StrValue s -> sprintf "\"%s\"" s
  | BolValue b -> sprintf "%s" (if b then "#t" else "#f")
  | Unary (u, ast') -> sprintf "%s%s" (unary_to_str u) (to_str ast')
  | Factor (f, a, b) -> sprintf "(%s %s %s)" (factor_to_str f) (to_str a) (to_str b)
  | Term (t, a, b) -> sprintf "[%s %s %s]" (term_to_str t) (to_str a) (to_str b)
  | Terminal -> "EOF"
  | _ -> ""
