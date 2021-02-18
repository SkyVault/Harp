open Printf
open Harp
open Tok_info
open Harp.Lexer

let generate_project pname =
  printf "Generating new project '%s'" pname;
  Sys.command (sprintf "mkdir -p %s" pname) |> ignore;
  Common.write_string_to_file (sprintf "%s/main.harp" pname) "print (\"Hello World\")"

let write_lua_to_cache pname lua =
  Sys.command (sprintf "mkdir -p %s/.cache" pname) |> ignore;
  Common.write_string_to_file (sprintf "%s/.cache/main.lua" pname) lua

let () =
  Printf.printf "\n";
  match (Array.to_list Sys.argv) with
  | _::"new"::pname::[] -> generate_project pname
  | _::"run"::pname::[] ->
    Common.read_whole_file (sprintf "%s/main.harp" pname)
    |> tokenize
    |> Parser.parse
    |> Analyzer.analyze_ast
    |> Luagen.ast_to_lua
    |> write_lua_to_cache pname;
    Sys.command (sprintf "lua %s/.cache/main.lua" pname) |> ignore
  | _::"build"::pname::[] ->
    Common.read_whole_file (sprintf "%s/main.harp" pname)
    |> tokenize
    |> Parser.parse
    |> Analyzer.analyze_ast
    |> Luagen.ast_to_lua
    |> write_lua_to_cache pname;
  | _::script::[] ->
    let ast =
      Common.read_whole_file script
      |> tokenize
      |> List.map (fun (t, i) -> (printf "T: %s %s\n" (tok_to_str t) (tok_info_to_str i)); (t, i))
      |> Parser.parse
    in
      ast |> Ast.to_str |> Printf.printf "\n-=<{ AST OUT }>=-\n%s\n";
      let lua = ast |> (* Analyzer.analyze_ast |> *) Luagen.ast_to_lua in
      Printf.printf "\n-=<{ LUA OUT }>=-\n%s\n\n" lua;
      Common.write_string_to_file "main.lua" lua
  | _ -> ()
