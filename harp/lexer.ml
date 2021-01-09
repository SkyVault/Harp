open Common
open Printf

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_ws = function ' ' | '\n' | '\t' | '\b' -> true | _ -> false
let is_sym = function '(' | ')' | '[' | ']' | '{' | '}' -> true | _ -> false
let char_to_digit chr = float_of_int (Char.code chr - Char.code '0')
let string_of_char_list = List.to_seq >> String.of_seq
let float_of_char_list = string_of_char_list >> float_of_string

(* type token_value *)
type token_value
  = TNum of float
  | TAtom of string
  | TBol of bool
  | TStr of string
  | TKeyWord of string
  | TOpenParen
  | TCloseParen
  | TOpenBracket
  | TCloseBracket
  | TOpenBrace
  | TCloseBrace

type token_info = int * int

type token = token_value * token_info

let make_tok (v) (i) : token = (v, i)

(* type position = int * int (\* line * column *\)
 *
 * type token = { v : token_value; pos : position } *)

let print_token = function
  | TNum n -> printf "TNum %f" n
  | TAtom a -> printf "TAtom '%s'" a
  | TStr s -> printf "TStr %s" s
  | TBol b -> printf "TBol %s" (if b then "#t" else "#f")
  | TKeyWord s -> printf "TKeyWord %s" s
  | TOpenParen -> printf "("
  | TCloseParen -> printf ")"
  | TOpenBracket -> printf "["
  | TCloseBracket -> printf "]"
  | TOpenBrace -> printf "{"
  | TCloseBrace -> printf "}"

let print_tok_ws t = printf "<'"; print_token t; printf "'>\n"

let get_number (cs: char list): token_value * char list =
  let rec loop (cs: char list) (decimal: bool) (res: char list): char list * char list =
    match cs with
    | [] -> (res, [])
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
    | c::rest -> loop rest (c::res)
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

let tokenize (s: string): token list =
  let rec loop (s: char list) (res: token list) (info: token_info): token list =
    match s with
    | [] -> res
    | c::rest when (is_ws c) ->
       loop rest res info
    | '('::rest -> loop rest ((make_tok TOpenParen info)::res) info
    | ')'::rest -> loop rest ((make_tok TCloseParen info)::res) info
    | '['::rest -> loop rest ((make_tok TOpenBracket info)::res) info
    | ']'::rest -> loop rest ((make_tok TCloseBracket info)::res) info
    | '{'::rest -> loop rest ((make_tok TOpenBrace info)::res) info
    | '}'::rest -> loop rest ((make_tok TCloseBrace info)::res) info
    | '#'::'t'::rest -> loop rest ((make_tok (TBol true) info)::res) info
    | '#'::'f'::rest -> loop rest ((make_tok (TBol false) info)::res) info
    | '"'::rest ->
      let (tok, rest') = get_str rest in
      loop rest' ((make_tok tok info)::res) info
    | '-'::n::rest when is_digit n -> begin
      match (n::rest) |> get_number with
      | (TNum v, rest') -> loop rest' ((make_tok (TNum (v *. -1.)) info::res)) info
      | _ -> failwith "number lexer failure"
    end
    | n::rest when is_digit n ->
       let (tok, rest') = (n::rest) |> get_number in
       loop rest' ((make_tok tok info)::res) info
    | c::rest when not (is_ws c) ->
       let (tok, rest') = (c::rest) |> get_atom in
       loop rest' ((make_tok tok info)::res) info
    | c::rest ->
       printf "Unhandled token '%c'\n" c;
       loop rest res info
  in
    loop (explode s) [] (0, 0) |> reverse
