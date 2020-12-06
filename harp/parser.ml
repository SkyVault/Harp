open Printf
open Common
open Value
open Lexer

let rec parse_list (ts: token list): Value.t * token list =
  let rec loop (ts: token list) (vs: Value.t list): Value.t list * token list =
    match ts with
    | [] -> printf "Error, missing closing paren"; exit 1
    | TCloseParen::rest -> (vs,  rest)
    | rest -> let (v, rest') = parse_expr rest in loop rest' (v::vs)
  in
    let (vs, rest) = loop ts [] in (List (reverse vs), rest)

and parse_seq (ts: token list): Value.t * token list =
  let rec loop (ts: token list) (vs: Value.t list): Value.t list * token list =
    match ts with
    | [] -> printf "Error, missing closing bracket"; exit 1
    | TCloseBracket::rest -> (vs,  rest)
    | rest -> let (v, rest') = parse_expr rest in loop rest' (v::vs)
  in
    let (vs, rest) = loop ts [] in (Seq (reverse vs), rest)

and parse_expr (ts: token list): Value.t * token list =
  match ts with
  | [] -> (Nothing, []) (* Shouldn't be possible... *)
  | (TNum v)::rest -> (Num v, rest)
  | (TBol v)::rest -> (Bol v, rest)
  | (TAtom v)::rest -> (Atom v, rest)
  | (TStr v)::rest -> (Str v, rest)
  | TOpenParen::rest -> parse_list rest
  | TCloseParen::rest -> (Nothing, rest)
  | TOpenBracket::rest -> parse_seq rest
  | TCloseBracket::rest -> (Nothing, rest)

let parse_progn (ts: token list): Value.t =
  let rec loop (ts: token list) (vs: Value.t list) =
    match ts with
    | [] -> Progn (reverse vs)
    | rest ->
      let (v, rest') = (parse_expr rest) in
      loop rest' (v::vs)
  in
    loop ts []
