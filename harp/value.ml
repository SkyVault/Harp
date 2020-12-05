open Printf

type t = Nothing
       | Num of float
       | Atom of string
       | Str of string
       | Bol of bool
       | List of t list
       | Progn of t list
       | Func of (t list * t * t) (* (args * env * list) *)
       | NatFunc of (t -> t -> t * t) (* env -> list -> env * value *)
       | Env of (t * t) list

let rec print_value (v: t): unit =
  match v with
  | Nothing -> printf "none"
  | Num n -> printf "%f" n
  | Atom a -> printf "%s" a
  | Str s -> printf "%s" s
  | Bol b -> printf "%s" (if b then "#t" else "#f")
  | Env _ -> printf "<env>"
  | Func _ -> printf "<func>"
  | NatFunc _ -> printf "<nat-func>"
  | List e ->
      printf "(";
      List.iter (fun v -> print_value v; printf " ") e;
      printf ")"
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
    let newList = List.filter (fun (k, _) -> k <> key) list in
    env_put (Env newList) key value
  | _ -> env

let rec env_merge env list =
  match list with
  | (k, v)::rest -> env_merge (env_put env k v) rest
  | [] -> env
