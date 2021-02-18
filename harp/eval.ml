open Printf
open Common
open Value
open Tok_info

let default = { line = 0; column = 0; ws_before = false; }

let rec eval_expr (env: Value.t) (expr: Value.t): Value.t * Value.t=
  let func_call env atom args =
    match (eval_expr env atom) with
    | (_, NatFunc fn) -> fn env (List (args, default))
    | (_, Func ((params, fenv, progn), i)) ->
      let func = (Func ((params, fenv, progn), i)) in
      let evalued = List.map (fun a -> evalV env a) args in
      let zipped = (zip (params, evalued)) in
      let env' = (env_merge fenv zipped) in
      let (_, return) = eval_progn (env_put env' atom func) progn in
      (env, return)
    | (_, Nothing) ->
       printf "Cannot find function in the environment: ";
       print_value atom;
       (env, Nothing)
    | (_, v) ->
      let i = get_token_info v in
      printf "(%d, %d) Cannot apply (" i.line i.column;
      print_value v;
      printf ")\n";
      (env, Nothing)
  in
    match expr with
    | Num (a, i) -> (env, Num (a, i))
    | Str (s, i) -> (env, Str (s, i))
    | Bol (b, i) -> (env, Bol (b, i))
    | Seq (e, i) -> (env, Seq (e, i))
    | Atom ("else", i) -> (env, Atom ("else", i))
    | Atom (a, i) -> begin
        match (env_find env (Atom (a, i))) with
        | Some v -> (env, v)
        | None -> (env, Nothing)
      end
    | List (callable::args, _) -> func_call env callable args
    | _ -> (env, Nothing)

and evalV (e: Value.t) (v: Value.t): Value.t =
  let (_, v) = (eval_expr e v) in v

and eval_progn (env: Value.t) (progn: Value.t): Value.t * Value.t =
  let rec loop (env: Value.t) (list: Value.t list) (result: Value.t): Value.t * Value.t =
    match list with
    | [] -> (env, result)
    | v::rest ->
       let (env', result') = eval_expr env v in
       loop env' rest result'
  in
    match progn with
    | Progn list -> loop env list Nothing
    | v -> printf "Error, eval_progn expects a progn but got: "; print_value v; exit 1
