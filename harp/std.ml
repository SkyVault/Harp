open Printf
open Value
open Eval

let std_bin_op (op: float -> float -> float) (start: float) (env: Value.t) (list: Value.t): Value.t * Value.t =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, Num (a', i)), (_, Num (b', _))) -> Num ((op a' b'), i)
    | _ ->
      let (line, chr) = get_token_info a in
      printf "(%d, %d) Error cannot add " line chr;
      print_value a;
      printf " with ";
      print_value b;
      printf "\n"; Nothing;
  in
    match list with
    | List (vs, _) -> (env, List.fold_left (apply) (Num (start, (0, 0))) vs)
    | _ -> printf "Error"; (env, Nothing)

let std_minus env list =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, Num (a', i)), (_, Num (b', _))) -> Num (a' -. b', i)
    | _ ->
      let (line, chr) = get_token_info a in
      printf "(%d, %d) Error cannot sub " line chr;
      print_value a;
      printf " with ";
      print_value b;
      printf "\n"; Nothing;
  in
    match list with
    | List (v::vs, _) ->
       let (_, n) = eval_expr env v in
       (env, List.fold_left (apply) n vs)
    | _ -> failwith "Minus failure"

let std_cmp op env list =
  let apply a b =
    match (eval_expr env a, eval_expr env b) with
    | ((_, a'), (_, b')) -> (op a' b')
  in
    match list with
    | List (a::b::[], i) -> (env, Bol (apply a b, i))
    | _ -> printf "Error"; (env, Nothing)

let std_def env list =
  match list with
  | List (Atom (a, _)::e::[], i) ->
     let (_, result) = eval_expr env e in
     (env_put env (Atom (a, i)) result, result)
  | _ ->
     failwith "Def requires an atom and an expr"

let std_set env list =
    match list with
    | List (Atom (a, _)::e::[], i) ->
      let (_, result) = eval_expr env e in
      (env_update env (Atom (a, i)) result, result)
    | _ ->
      failwith "Set requires an atom and an expr"

let std_if env list =
  match list with
  | List (expr::ifTrue::ifFalse::[], _) -> begin
     let (_, res) = eval_expr env expr in
     match res with
     | Bol (true, _) -> let (_, res) = (eval_expr env ifTrue) in (env, res)
     | Bol (false, _) | Nothing | _ -> let (_, res) = (eval_expr env ifFalse) in (env, res)
    end
  | List (expr::ifTrue::[], _) -> begin
     let (_, res) = eval_expr env expr in
     match res with
     | Bol (true, _) -> let (_, res) = (eval_expr env ifTrue) in (env, res)
     | _ -> (env, Nothing)
    end
  | _ -> failwith "if error"

let std_cond env list =
  let rec cond conds =
    match conds with
    | condition::expr::rest -> begin
      let (_, res) = eval_expr env condition in
      match res with
      | Atom ("else", _) -> let (_, res) = (eval_expr env expr) in res
      | Bol (true, _) -> let (_, res) = (eval_expr env expr) in res
      | Bol (false, _) -> cond rest
      | v ->
        let (line, chr) = get_token_info v in
        printf "(%d, %d) Value in condition, expected a bool, but got: " line chr;
        print_value v;
        printf "\n";
        Nothing
    end
    | [] -> Nothing
    | _ -> Nothing
  in
    match list with
    | List (xs, _) -> (env, cond xs)
    | _ -> failwith "cond error"

let std_fun env list =
  match list with
  | List (Atom (name, ai)::(List (argList, _))::exprs, i) -> begin
    let atom = Atom (name, ai) in
    let func = Func ((argList, env, (Progn exprs)), i) in
    let env' = (env_put env atom func) in
    (env', func)
  end
  | _ -> printf "Func syntax error"; exit 1

let std_do env list =
  match list with
  | List (exprs, _) ->
     let (_, result) = eval_progn env (Progn exprs) in
     (env, result)
  | _ -> failwith "std do failure"

let std_print env list =
  let print v =
    let (_, v') = eval_expr env v in print_val_ws v'
  in
  match list with
  | List (vs, _) ->
    (List.iter print vs) |> ignore;
    (env, Nothing)
  | _ -> failwith "failure"

let std_println env list =
  let (_, v) = std_print env list in
  printf "\n";
  (env, v)

let std_readln env list =
  match list with
  | List ([Str (s, _)], _) ->
    printf "%s" s;
    let inp = read_line () in
    (env, Str (inp, (0, 0)))
  | List ([], _) ->
    let inp = read_line () in
    (env, Str (inp, (0, 0)))
  | _ -> failwith "std readln error"

let std_push env list =
    match list with
    | List (seq::v::[], (line, chr)) -> begin
        match (eval_expr env seq) with
        | (_, Seq (seq, i)) ->
            let (_, v') = eval_expr env v in
            (env, Seq (v'::seq, i))
        | _ ->
          printf "(%d, %d) Error: push requires a list and a value\n" line chr;
          (env, Nothing)
    end
    | _ -> failwith "std push error"

let std_pop env list =
    match list with
    | List (seq::[], (line, chr)) -> begin
        match (eval_expr env seq) with
        | (_, Seq (_::seq, i)) -> (env, Seq (seq, i))
        | (_, Seq ([], i)) -> (env, Seq ([], i))
        | _ ->
          printf "(%d, %d) Error: pop requires only a seq as an argument\n" line chr;
          (env, Nothing)
    end
    | _ -> failwith "std push error"

let std_len env list =
  match list with
  | List (list::[], i) -> begin
    match (eval_expr env list) with
    | (_, Seq (xs, _)) -> (env, Num (float_of_int (List.length xs), i))
    | (_, Str (str, _)) -> (env, Num (float_of_int (String.length str), i))
    | _ ->
      let (line, chr) = i in
      printf "(%d, %d) Error: len requires either a seq or a st ring as its argument\n" line chr;
      (env, Nothing)
  end
  | _ -> failwith "std len requires only 1 argument"

let std_geti env list =
  match list with
  | List (list'::index::[], i) -> begin
    match (eval_expr env list', eval_expr env index) with
    | ((_, Seq (xs, _)), (_, Num (index, _))) ->
      (env, List.nth xs (int_of_float index))
    | _ ->
      let (line, chr) = i in
      printf "(%d, %d) Error: std geti requires a seq and an index\n" line chr;
      (env, Nothing)
  end
  | _ -> failwith "std geti requires a seq and an index"

let std_env env _ =
  match env with
  | Env list ->
    List.iter (fun (k, v) ->
      print_value k; printf " "; print_value v; printf "\n") list;
    (env, Nothing)
  | _ -> failwith ""

let make_std_env =
  Env [
    (Atom ("-", (0,0)), (NatFunc std_minus));
    (Atom ("+", (0,0)), (NatFunc (std_bin_op ( +. ) 0.)));
    (Atom ("*", (0,0)), (NatFunc (std_bin_op ( *. ) 1.)));
    (Atom ("=", (0,0)), (NatFunc (std_cmp (=))));
    (Atom (">", (0,0)), (NatFunc (std_cmp (>))));
    (Atom ("<", (0,0)), (NatFunc (std_cmp (<))));
    (Atom (">=", (0,0)), (NatFunc (std_cmp (>=))));
    (Atom ("<=", (0,0)), (NatFunc (std_cmp (<=))));
    (Atom ("if", (0,0)), (NatFunc std_if));
    (Atom ("cond", (0,0)), (NatFunc std_cond));
    (Atom ("def", (0,0)), (NatFunc std_def));
    (Atom ("set", (0,0)), (NatFunc std_set));
    (Atom ("do", (0,0)), (NatFunc std_do));
    (Atom ("print", (0,0)), (NatFunc std_print));
    (Atom ("println", (0,0)), (NatFunc std_println));
    (Atom ("readln", (0,0)), (NatFunc std_readln));
    (Atom ("fun", (0,0)), (NatFunc std_fun));

    (Atom ("env", (0,0)), (NatFunc std_env));

    (Atom ("push", (0,0)), (NatFunc std_push));
    (Atom ("pop", (0,0)), (NatFunc std_pop));
    (Atom ("len", (0,0)), (NatFunc std_len));
    (Atom ("geti", (0,0)), (NatFunc std_geti));

  ]
