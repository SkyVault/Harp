open Harp
open Harp.Lexer

let () =
  Printf.printf "\n";
  match (Array.to_list Sys.argv) with
  | _::script::[] ->
    let ast =
      Common.read_whole_file script
      |> tokenize
      |> Parser.parse
    in
      ast |> Ast.to_str |> Printf.printf "\n-=<{ AST OUT }>=-\n%s\n";

      let lua = ast |> Luagen.ast_to_lua in
      Printf.printf "\n-=<{ LUA OUT }>=-\n%s\n\n" lua;
      Common.write_string_to_file "out.lua" lua
  | _ -> ()
