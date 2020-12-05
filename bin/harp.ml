open Printf
open Harp
open Harp.Lexer
open Harp.Parser
open Harp.Value
open Harp.Eval

let rec repl env =
  printf "> ";
  let line = read_line () in
  match line with
  | "" -> ()
  | _ ->
    let (env', result) = line |> tokenize |> parse_progn |> eval_progn env in
    print_value result; printf "\n";
    repl env'

let () =
  let tokens = tokenize "-3.1415926" in
  List.iter print_tok_ws tokens;

  repl (Std.make_std_env)
