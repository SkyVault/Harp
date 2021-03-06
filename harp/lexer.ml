open Common
open Printf
open Tok_info

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_ws = function ' ' | '\n' | '\t' | '\b' -> true | _ -> false
let is_sym = function '.' | '(' | ')' | '[' | ']' | '{' | '}' -> true | _ -> false
let char_to_digit chr = float_of_int (Char.code chr - Char.code '0')
let string_of_char_list = List.to_seq >> String.of_seq
let float_of_char_list = string_of_char_list >> float_of_string

(* type token_value *)
type token_value
  = TNum of float
  | TAtom of string
  | TBol of bool
  | TStr of string
  | TDot
  | TComma
  | TRange
  | TOpenParen
  | TCloseParen
  | TOpenBracket
  | TCloseBracket
  | TOpenBrace
  | TCloseBrace

type token = token_value * token_info

(* type position = int * int (\* line * column *\)
 *
 * type token = { v : token_value; pos : position } *)

let tok_to_str = function
  | TNum n -> sprintf "TNum %f" n
  | TAtom a -> sprintf "TAtom '%s'" a
  | TStr s -> sprintf "TStr %s" s
  | TBol b -> sprintf "TBol %s" (if b then "#t" else "#f")
  | TDot -> "."
  | TComma -> ","
  | TRange -> ".."
  | TOpenParen -> "("
  | TCloseParen -> ")"
  | TOpenBracket -> "["
  | TCloseBracket -> "]"
  | TOpenBrace -> "{"
  | TCloseBrace -> "}"

let get_number (cs: char list): token_value * char list =
  let rec loop (cs: char list) (decimal: bool) (res: char list): char list * char list =
    match cs with
    | [] -> (res, [])
    | '.'::'.'::rest -> (res, '.'::'.'::rest)
    | n::rest when is_digit n -> loop rest decimal (n::res)
    | '.'::rest ->
      if decimal then (res, rest)
      else loop rest true ('.'::res)
    | rest -> (res, rest)
  in
    let (ns, rest) = loop cs false [] in
    let num = ns |> reverse |> float_of_char_list in
    (TNum num, rest)

let get_atom (cs: char list): token_value * char list =
  let rec loop (cs: char list) (res: char list): char list * char list =
    match cs with
    | [] -> (res, [])
    | w::rest when (is_ws w) || (is_sym w) -> (res, w::rest)
    | c::rest ->
      loop rest (c::res)
  in
    let (ss, rest) = loop cs [] in
    let s = ss |> reverse |> string_of_char_list in
    (TAtom s, rest)

let get_str (cs: char list): token_value * char list =
  let rec loop (cs: char list) (res: char list): char list * char list =
    match cs with
    | '"'::rest -> (res, rest)
    | c::rest -> loop rest (c::res)
    | [] -> failwith "Lexer error, missing closing '\"'"
  in
    let (str, rest) = loop cs [] in
    (TStr (str |> reverse |> string_of_char_list), rest)

let get_atom_literal (cs: char list): token_value * char list =
  let rec loop (cs: char list) (res: char list): char list * char list =
    match cs with
    | c::rest when (is_ws c) || (is_sym c) -> (res, (c::rest))
    | c::rest -> loop rest (c::res)
    | [] -> (res, [])
  in
    let (str, rest) = loop cs [] in
    (TStr (str |> reverse |> string_of_char_list), rest)

let rec skip_till_newline = function
  | '\n'::rest -> rest
  | _::rest -> skip_till_newline rest
  | [] -> []

let rec skip_till_ws = function
  | c::rest when (is_ws c) -> rest
  | _::rest -> skip_till_ws rest
  | [] -> []

let tokenize (s: string): token list =
  let rec loop (s: char list) (res: token list) (info: token_info): token list =
    match s with
    | [] -> res
    | c::rest when c = '\n' -> loop rest res { info with line = info.line + 1 }
    | c::'('::rest when (is_ws c) ->
      loop rest ((TOpenParen, { info with ws_before = true })::res) info
    | c::rest when (is_ws c) -> loop rest res info
    | ';'::rest -> loop (skip_till_newline rest) res { info with line = info.line + 1 }
    | ':'::'='::rest -> loop rest ((TAtom ":=", info)::res) info
    | '('::rest -> loop rest ((TOpenParen, info)::res) info
    | ')'::rest -> loop rest ((TCloseParen, info)::res) info
    | '['::rest -> loop rest ((TOpenBracket, info)::res) info
    | ']'::rest -> loop rest ((TCloseBracket, info)::res) info
    | '{'::rest -> loop rest ((TOpenBrace, info)::res) info
    | '}'::rest -> loop rest ((TCloseBrace, info)::res) info
    | '.'::'.'::rest -> loop rest ((TRange, info)::res) info
    | '#'::'t'::rest -> loop rest (((TBol true), info)::res) info
    | '#'::'f'::rest -> loop rest (((TBol false), info)::res) info
    | '!'::rest -> loop rest (((TAtom "!"), info)::res) info
    | '-'::rest -> loop rest (((TAtom "-"), info)::res) info
    | '*'::rest -> loop rest (((TAtom "*"), info)::res) info
    | '/'::rest -> loop rest (((TAtom "/"), info)::res) info
    | '+'::rest -> loop rest (((TAtom "+"), info)::res) info
    | '"'::rest ->
      let (tok, rest') = get_str rest in
      loop rest' ((tok, info)::res) info
    | ':'::rest ->
      let (tok, rest') = get_atom_literal rest in
      loop rest' ((tok, info)::res) info
    | n::rest when is_digit n ->
       let (tok, rest') = (n::rest) |> get_number in
       loop rest' ((tok, info)::res) info
    | '.'::rest -> loop rest ((TDot, info)::res) info
    | c::rest when not (is_ws c) ->
       let (tok, rest') = (c::rest) |> get_atom in
       loop rest' ((tok, info)::res) info
    | c::_ ->
        failwith (sprintf "Unhandled token '%c'\n" c);
       (* loop rest res info *)
  in
    loop (explode s) [] { line=1; column=0; ws_before=false } |> reverse
