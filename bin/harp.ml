open Printf
open Harp

let generate_project pname =
  printf "Generating new project '%s'" pname;
  Sys.command (sprintf "mkdir -p %s" pname) |> ignore;
  Common.write_string_to_file (sprintf "%s/main.harp" pname) "print (\"Hello World\")"

let write_lua_to_cache pname lua =
  Sys.command (sprintf "mkdir -p %s/.cache" pname) |> ignore;
  Common.write_string_to_file (sprintf "%s/.cache/bundle.lua" pname) lua

let () =
  Printf.printf "\n";
  match (Array.to_list Sys.argv) with
  | _::"new"::pname::[] -> generate_project pname
  | _::"build"::pname::[] ->
    let bundle = Builder.build_project (sprintf "%s/main.harp" pname) in
    write_lua_to_cache pname bundle
  | _::script::[] ->
    let bundle = Builder.build_project script in
    printf "%s\n\n" bundle;
    bundle |> Common.write_string_to_file "bundle.lua"
  | _ -> ()
