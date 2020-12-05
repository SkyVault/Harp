open Common
open Printf

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_ws = function ' ' | '\n' | '\t' | '\b' -> true | _ -> false
let is_sym = function '(' | ')' | '[' | ']' | '{' | '}' -> true | _ -> false
let char_to_digit chr = float_of_int (Char.code chr - Char.code '0')
let string_of_char_list = List.to_seq >> String.of_seq
let float_of_char_list = string_of_char_list >> float_of_string

type token = TNum of float
           | TAtom of string
           | TBol of bool
           | TStr of string
           | TOpenParen
           | TCloseParen

let print_token = function
  | TNum n -> printf "TNum %f" n
  | TAtom a -> printf "TAtom %s" a
  | TStr s -> printf "TStr %s" s
  | TBol b -> printf "TBol %s" (if b then "#t" else "#f")
  | TOpenParen -> printf "("
  | TCloseParen -> printf ")"

let print_tok_ws t = printf "<'"; print_token t; printf "'> "

let get_number (cs: char list): token * char list =
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

let get_atom (cs: char list): token * char list =
  let rec loop (cs: char list) (res: char list): char list * char list =
    match cs with
    | [] -> (res, [])
    | w::rest when (is_ws w) || (is_sym w) -> (res, w::rest)
    | c::rest -> loop rest (c::res)
  in
    let (ss, rest) = loop cs [] in
    let s = ss |> reverse |> string_of_char_list in
    (TAtom s, rest)

let tokenize (s: string): token list =
  let rec loop (s: char list) (res: token list): token list =
    match s with
    | [] -> res
    | '('::rest -> loop rest (TOpenParen::res)
    | ')'::rest -> loop rest (TCloseParen::res)
    | '#'::'t'::rest -> loop rest (TBol true::res)
    | '#'::'f'::rest -> loop rest (TBol false::res)
    | '-'::n::rest when is_digit n -> begin
      match (n::rest) |> get_number with
      | (TNum v, rest') -> loop rest' (TNum (v *. -1.)::res)
      | _ -> failwith "number lexer failure"
    end
    | n::rest when is_digit n ->
       let (tok, rest') = (n::rest) |> get_number in
       loop rest' (tok::res)
    | c::rest when not (is_ws c) ->
       let (tok, rest') = (c::rest) |> get_atom in
       loop rest' (tok::res)
    | _::rest -> loop rest res
  in
    loop (explode s) [] |> reverse
