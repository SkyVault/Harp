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
    let ast =
      Common.read_whole_file script
      |> tokenize
      |> Parser.parse
    in
      ast |> Ast.to_str |> Printf.printf "\n<<AST>>\n%s\n";
      ast
      |> Luagen.ast_to_lua
      |> (fun s -> Common.write_string_to_file "out.lua" s)
  | _ -> ()
