type token =
  | LEFT_P
  | RIGHT_P
  | LEFT_B
  | RIGHT_B
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMI_C
  | SLASH
  | STAR
  | BANG
  | BANG_EQ
  | EQ
  | EQ_EQ
  | GREATER
  | GREATER_EQ
  | LESS
  | LESS_EQ
  | IDENTIFIER of string
  | STR of string
  | NUMBER of float
  | AND
  | ELSE
  | FOR
  | IF
  (* | NIL *)
  | OR
  | PRINT
  (* | RETURN *)
  (* | TRUE *)
  | LET
  (* | WHILE *)
  | EOF

exception NotFound of string

let keywords =
  [
    ("and", AND);
    ("or", OR);
    ("else", ELSE);
    ("for", FOR);
    ("let", LET);
    ("if", IF);
    ("print", PRINT);
  ]

let ke (k, _) : string = k
let va (_, v) : token = v

let rec find x = function
  | [] -> None
  | h :: t -> if ke h = x then Some (va h) else find x t

let rec scan (s : string) (c : int) : token list =
  if c >= String.length s then [ EOF ]
  else
    match String.get s c with
    | ' ' | '\t' -> scan s (c + 1)
    | '{' -> LEFT_P :: scan s (c + 1)
    | '}' -> RIGHT_P :: scan s (c + 1)
    | '(' -> LEFT_B :: scan s (c + 1)
    | ')' -> RIGHT_B :: scan s (c + 1)
    | ',' -> COMMA :: scan s (c + 1)
    | '.' -> DOT :: scan s (c + 1)
    | '-' -> MINUS :: scan s (c + 1)
    | '+' -> PLUS :: scan s (c + 1)
    | ';' -> SEMI_C :: scan s (c + 1)
    | '/' -> SLASH :: scan s (c + 1)
    | '*' -> STAR :: scan s (c + 1)
    | '!' ->
        if c < String.length s - 1 && String.get s (c + 1) = '=' then
          BANG_EQ :: scan s (c + 2)
        else BANG :: scan s (c + 1)
    | '=' ->
        if c < String.length s - 1 && String.get s (c + 1) = '=' then
          EQ_EQ :: scan s (c + 2)
        else EQ :: scan s (c + 1)
    | '>' ->
        if c < String.length s - 1 && String.get s (c + 1) = '=' then
          GREATER_EQ :: scan s (c + 2)
        else GREATER :: scan s (c + 1)
    | '<' ->
        if c < String.length s - 1 && String.get s (c + 1) = '=' then
          LESS_EQ :: scan s (c + 2)
        else LESS :: scan s (c + 1)
    | '\"' ->
        let _ =
          Re.Str.search_forward (Re.Str.regexp "[A-Za-z0-9]+\"") s (c + 1)
        in
        let t = Re.Str.matched_string s in
        let p = String.sub t 0 (String.length t - 1) in
        STR p :: scan s (c + String.length p + 2)
    | _ ->
        let b =
          Re.Str.string_match (Re.Str.regexp "[A-Za-z][A-Za-z0-9_]+") s c
        in
        if b then
          let our_str = Re.Str.matched_string s in
          let found = find our_str keywords in
          (if Option.is_none found then IDENTIFIER our_str else Option.get found)
          :: scan s (c + (our_str |> String.length))
        else
          let n = Re.Str.string_match (Re.Str.regexp "[0-9]+.?[0-9]*") s c in
          (* Add a fucking dot in this regex man *)
          if n then
            let our_str = Re.Str.matched_string s in
            NUMBER (float_of_string our_str)
            :: scan s (c + (our_str |> String.length))
          else
            raise
              (NotFound
                 ("Not a keyword or variable: "
                 ^ String.sub s c (String.length s - 1)))

let rec print_tokens = function
  | [] -> ()
  | h :: t -> (
      match h with
      | STR s ->
          print_string s;
          print_tokens t
      | OR ->
          print_string "<or>";
          print_tokens t
      | AND ->
          print_string "<and>";
          print_tokens t
      | IDENTIFIER i ->
          print_string ("<identifier> " ^ i);
          print_tokens t
      | NUMBER n ->
          print_string ("<number> " ^ string_of_float n);
          print_tokens t
      | EOF ->
          print_newline ();
          print_tokens t
      | _ ->
          print_string "(";
          print_tokens t)
