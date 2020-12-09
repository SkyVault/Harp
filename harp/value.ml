open Printf
open Lexer

type t = Nothing
       | Num of float * token_info
       | Atom of string * token_info
       | Str of string * token_info
       | Bol of bool * token_info
       | List of t list * token_info
       | Seq of t list * token_info (* Userland expanding vector *)
       | Progn of t list
       | Func of (t list * t * t) * token_info (* (args * env * list) *)
       | NatFunc of (t -> t -> t * t) (* env -> list -> env * value *)
       | Env of ((t * t) list)

let get_token_info v =
  match v with
  | Nothing -> (0, 0)
  | Num (_, i) -> i
  | Atom (_, i) -> i
  | Str (_, i) -> i
  | Bol (_, i) -> i
  | Env _ -> (0, 0)
  | Func (_, i) -> i
  | NatFunc _ -> (0, 0)
  | List (_, i) -> i
  | Seq (_, i) -> i
  | Progn _ -> (0, 0)

let rec print_value (v: t): unit =
  match v with
  | Nothing -> printf "none"
  | Num (n, _) -> printf "%f" n
  | Atom (a, _) -> printf "%s" a
  | Str (s, _) -> printf "%s" s
  | Bol (b, _) -> printf "%s" (if b then "#t" else "#f")
  | Env list ->
    List.iter (fun (k, v) ->
      print_value k; printf " "; print_value v; printf "\n") list
  | Func _ -> printf "<func>"
  | NatFunc _ -> printf "<nat-func>"
  | List (e, _) ->
      printf "("; List.iter (fun v -> print_value v; printf " ") e; printf ")"
  | Seq (e, _) ->
      printf "["; List.iter (fun v -> print_value v; printf " ") e; printf "]"
  | Progn e ->
      printf "(";
      List.iter (fun v -> print_value v; printf " ") e;
      printf ")"

let print_val_ws v = print_value v; printf " "

(* Environment *)
let env_find env key =
  let rec loop key = function
    | [] -> None
    | (key', value)::_ when key' = key -> Some value
    | _::rest -> loop key rest
  in
    env |> function Env list -> loop key list | _ -> None

let env_put env key value =
  match env with
  | Env list -> Env ((key, value)::list)
  | _ -> env

let env_update env key value =
  match env with
  | Env list ->
    let newList =
        list |> List.map (fun (key', value') ->
                    if key' = key then (key, value)
                    else (key', value'))
    in
      (Env newList)
  | _ -> env

let rec env_merge env list =
  match list with
  | (k, v)::rest -> env_merge (env_put env k v) rest
  | [] -> env
