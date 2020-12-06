open Printf
open Value
open Eval

let std_bin_op (op: float -> float -> float) (start: float) (env: Value.t) (list: Value.t): Value.t * Value.t =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, Num a'), (_, Num b')) -> Num (op a' b')
    | _ -> printf "Error cannot add "; print_value a; printf " with "; print_value b; Nothing
  in
    match list with
    | List vs -> (env, List.fold_left (apply) (Num start) vs)
    | _ -> printf "Error"; (env, Nothing)

let std_minus env list =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, Num a'), (_, Num b')) -> Num (a' -. b')
    | _ -> printf "Error cannot subtract"; print_value a; printf " with "; print_value b; Nothing
  in
    match list with
    | List (v::vs) ->
       let (_, n) = eval_expr env v in
       (env, List.fold_left (apply) n vs)
    | _ -> failwith "Minus failure"

let std_cmp op env list =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, a'), (_, b')) -> Bol (op a' b')
  in
    match list with
    | List (a::b::[]) -> (env, apply a b)
    | _ -> printf "Error"; (env, Nothing)

let std_def env list =
  match list with
  | List (Atom a::e::[]) ->
     let (_, result) = eval_expr env e in
     (env_update env (Atom a) result, result)
  | _ ->
     failwith "Def requires an atom and an expr"

let std_if env list =
  match list with
  | List (expr::ifTrue::ifFalse::[]) -> begin
     let (_, res) = eval_expr env expr in
     match res with
     | Bol true -> let (_, res) = (eval_expr env ifTrue) in (env, res)
     | Bol false | Nothing | _ -> let (_, res) = (eval_expr env ifFalse) in (env, res)
    end
  | List (expr::ifTrue::[]) -> begin
     let (_, res) = eval_expr env expr in
     match res with
     | Bol true -> let (_, res) = (eval_expr env ifTrue) in (env, res)
     | _ -> (env, Nothing)
    end
  | _ -> failwith "if error"

let std_cond env list =
  let rec cond conds =
    match conds with
    | condition::expr::rest -> begin
      let (_, res) = eval_expr env condition in
      match res with
      | Atom "else" -> let (_, res) = (eval_expr env expr) in res
      | Bol true -> let (_, res) = (eval_expr env expr) in res
      | Bol false -> cond rest
      | _ -> failwith "Unexpected condition"
    end
    | [] -> Nothing
    | _ -> Nothing
  in
    match list with
    | List xs -> (env, cond xs)
    | _ -> failwith "cond error"

let std_fun env list =
  match list with
  | List ((Atom name)::(List argList)::exprs) -> begin
    let atom = (Atom name) in
    let func = (Func (argList, env, (Progn exprs))) in
    let env' = (env_put env atom func) in
    (env', func)
  end
  | _ -> printf "Func syntax error"; exit 1

let std_do env list =
  match list with
  | List exprs ->
     let (_, result) = eval_progn env (Progn exprs) in
     (env, result)
  | _ -> failwith "std do failure"

let std_print env list =
  let print v =
    let (_, v') = eval_expr env v in print_val_ws v'
  in
  match list with
  | List vs ->
    (List.iter print vs) |> ignore;
    (env, Nothing)
  | _ -> failwith "failure"

let std_println env list =
  let (_, v) = std_print env list in
  printf "\n";
  (env, v)

let std_push env list =
    match list with
    | List (seq::v::[]) -> begin
        match (eval_expr env seq) with
        | (_, Seq seq) ->
            let (_, v') = eval_expr env v in
            (env, Seq (v'::seq))
        | _ -> failwith "push requires a seq as its first argument"
    end
    | _ -> failwith "std push error"

let std_pop env list =
    match list with
    | List (seq::[]) -> begin
        match (eval_expr env seq) with
        | (_, Seq (_::seq)) -> (env, Seq (seq))
        | (_, Seq []) -> (env, Seq [])
        | _ -> failwith "push requires a seq as its first argument"
    end
    | _ -> failwith "std push error"

let make_std_env =
  Env [
    (Atom "-", (NatFunc std_minus));
    (Atom "+", (NatFunc (std_bin_op ( +. ) 0.)));
    (Atom "*", (NatFunc (std_bin_op ( *. ) 1.)));
    (Atom "=", (NatFunc (std_cmp (=))));
    (Atom ">", (NatFunc (std_cmp (>))));
    (Atom "<", (NatFunc (std_cmp (<))));
    (Atom ">=", (NatFunc (std_cmp (>=))));
    (Atom "<=", (NatFunc (std_cmp (<=))));
    (Atom "if", (NatFunc std_if));
    (Atom "cond", (NatFunc std_cond));
    (Atom "def", (NatFunc std_def));
    (Atom "do", (NatFunc std_do));
    (Atom "print", (NatFunc std_print));
    (Atom "println", (NatFunc std_println));
    (Atom "fun", (NatFunc std_fun));
    (Atom "push", (NatFunc std_push));
    (Atom "pop", (NatFunc std_pop));
  ]
