type token_info =
  { line: int
  ; column: int
  ; ws_before: bool (* true if the previous character was whitespace *)
  }

let to_tup t = (t.line, t.column)
let tok_info_to_str info =
  Printf.sprintf "(%d:%d)" info.line info.column
