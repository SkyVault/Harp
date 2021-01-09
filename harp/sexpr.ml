open Printf
open Common
open Value
open Lexer

let rec parse_list (ts: token list) (info: token_info): Value.t * token list =
  let rec loop (ts: token list) (vs: Value.t list): Value.t list * token list =
    match ts with
    | [] -> printf "Error, missing closing paren"; exit 1
    | (TCloseParen, _)::rest -> (vs,  rest)
    | rest -> let (v, rest') = parse_expr rest in loop rest' (v::vs)
  in
    let (vs, rest) = loop ts [] in (List ((reverse vs), info), rest)

and parse_brace_list (ts: token list) (info: token_info): Value.t * token list =
  let rec loop (ts: token list) (vs: Value.t list): Value.t list * token list =
    match ts with
    | [] -> printf "Error, missing closing paren"; exit 1
    | (TCloseBrace, _)::rest -> (vs,  rest)
    | rest -> let (v, rest') = parse_expr rest in loop rest' (v::vs)
  in
    let (vs, rest) = loop ts [] in (List ((reverse vs), info), rest)

and parse_seq (ts: token list) (info: token_info): Value.t * token list =
  let rec loop (ts: token list) (vs: Value.t list): Value.t list * token list =
    match ts with
    | [] -> printf "Error, missing closing bracket"; exit 1
    | (TCloseBracket, _)::rest -> (vs,  rest)
    | rest -> let (v, rest') = parse_expr rest in loop rest' (v::vs)
  in
    let (vs, rest) = loop ts [] in (Seq ((reverse vs), info), rest)

and parse_expr (ts: token list): Value.t * token list =
  match ts with
  | [] -> (Nothing, []) (* Shouldn't be possible... *)
  | (TNum v, i)::rest -> (Num (v, i), rest)
  | (TBol v, i)::rest -> (Bol (v, i), rest)
  | (TAtom v, i)::rest -> (Atom (v, i), rest)
  | (TStr v, i)::rest -> (Str (v, i), rest)
  | (TOpenParen, i)::rest -> parse_list rest i
  | (TCloseParen, _)::rest -> (Nothing, rest)
  | (TOpenBracket, i)::rest -> parse_seq rest i
  | (TCloseBracket, _)::rest -> (Nothing, rest)
  | (TOpenBrace, i)::rest -> parse_brace_list rest i
  | (TCloseBrace, _)::rest -> (Nothing, rest)
  | _ -> failwith "Token is not supported by the lisp parser"

let parse_progn (ts: token list): Value.t =
  let rec loop (ts: token list) (vs: Value.t list) =
    match ts with
    | [] -> Progn (reverse vs)
    | rest ->
      let (v, rest') = (parse_expr rest) in
      loop rest' (v::vs)
  in
    loop ts []
