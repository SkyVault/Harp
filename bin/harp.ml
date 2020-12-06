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
  match (Array.to_list Sys.argv) with
  | _::script::[] ->
    let text = Common.read_whole_file script in
    let env = (Std.make_std_env) in
    text |> tokenize |> parse_progn |> eval_progn env |> ignore
    (* let (env', _) = text |> tokenize |> parse_progn |> eval_progn env in *)
    (* repl env' *)
  | _ -> repl (Std.make_std_env)
