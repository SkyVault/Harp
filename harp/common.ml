let (<<) f g x = f(g(x))
let (>>) f g x = g(f(x))
let (>>|) r f = Result.map f r
let (>>=) r f = Result.bind r f

let cat_strings = List.fold_left (fun a b -> a ^ " " ^ b) ""

let rec zip paired_lists =
  match paired_lists with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1, h2)::(zip (t1, t2))
  | _, _ -> failwith "Wrong number of args"

let explode s = List.init (String.length s) (String.get s)

let reverse list =
    let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in aux [] list

let pop_last is =
  let rev = reverse is in ((List.tl rev |> reverse), List.hd rev)

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_string_to_file filename content =
  let oc = open_out filename in
  Printf.fprintf oc "%s" content;
  close_out oc
