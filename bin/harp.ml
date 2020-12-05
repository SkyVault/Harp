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
  repl (Std.make_std_env);
  let program = "
    (fun loop (n)
       (if (> n 0)
          (do (println n)
              (loop (- n 1)))))
    (loop 2000)
  " in
  (* let tokens = program |> tokenize in
   * List.iter print_tok_ws tokens; *)
  let (_, result) = (program |> tokenize |> parse_progn |> eval_progn (Std.make_std_env)) in
  print_value result
