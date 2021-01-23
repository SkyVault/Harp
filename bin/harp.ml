open Harp
open Harp.Lexer

(* let rec repl env =
 *   printf "> ";
 *   let line = read_line () in
 *   match line with
 *   | "" -> ()
 *   | _ ->
 *     let (env', result) = line |> tokenize |> parse_progn |> eval_progn env in
 *     print_value result; printf "\n";
 *     repl env'
 *)
 let () =
    Printf.printf "\n";
    match (Array.to_list Sys.argv) with
    | _::script::[] ->
      let text = Common.read_whole_file script in
      text |> tokenize |> Parser.parse |> Ast.to_str |> Printf.printf "\n%s\n"
    | _ -> ()
