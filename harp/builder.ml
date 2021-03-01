(* Controlls the entire build process *)

open Lexer
open Printf
open Ast
open Analyzer

type import = { path : string }

let get_imports node =
  let rec get_imports_loop ns is =
    match ns with
    | FunCall (AtomValue (info, "import"), List ps)::rest' -> begin
      match ps with
      | AtomValue (_, file)::[] ->
        get_imports_loop rest' ({ path = file }::is)
      | StrValue file::[] ->
        get_imports_loop rest' ({ path = file }::is)
      | _ -> Common.log_error info "Import expects the name of the module"
    end
    | _::rest -> get_imports_loop rest is
    | [] -> is
  in
    match node with
    | Progn ns -> get_imports_loop ns []
    | _ -> failwith "Expected progn"

let parse_file path =
  (* printf "loading file: %s\n" path; *)
  Common.read_whole_file path
  |> tokenize
  |> Parser.parse

let build_file path =
  let lua_prelude = Common.read_whole_file "luastd/std.lua" in
  let lua_range = Common.read_whole_file "luastd/range.lua" in

  let ast = parse_file path in
  let imports = get_imports ast in

  let asts = imports
  |> List.map(fun { path } -> parse_file (sprintf "%s.harp" path)) in

  let std = {
    env =
    [ (mk_fundef "print" 1);
      (mk_fundef "println" 1);
      (mk_fundef "range" 2);
      (mk_fundef "push" 2);
      (mk_fundef "remove" 2);
      (mk_fundef "len" 1);
      (mk_fundef "nth" 2);
      (mk_fundef "read" 0);
      (mk_fundef "mod" 2);
      (mk_fundef "import" 1);
      (mk_fundef "stri" 2);
      (mk_fundef "strcat" 2) ]
    ; imports = [] }
  in
    let states = asts
      |> List.map(fun ast -> analyze_ast ast std)
      |> List.map(fun (_,st) -> st)
    in
      let envs : vardef list = states
        |> List.map (fun e -> e.env)
        |> List.flatten
      in

      let (ast, state) = analyze_ast ast { std with env = List.append std.env envs } in

      let main = ast |> Luagen.ast_to_lua in
      let final = List.append (List.map (fun a -> Luagen.ast_to_lua ~ret:false a) asts) [lua_range; lua_prelude; main] in
      (final |> Common.cat_strings, state)

let build_project entry_path =
  let (code,_) = build_file entry_path in
  code
