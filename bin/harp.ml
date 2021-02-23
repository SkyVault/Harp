open Printf
open Harp
(* open Harp.Lexer *)

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
  | _::script::[] ->
    let bundle = Builder.build_project script in
    bundle |> Common.write_string_to_file "bundle.lua"
  | _ -> ()
