type tokens = 
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Function
  | Let
  | Eof
  | Illegal;;

type token = {
  kind: tokens;
  literal: string;
}

let keywords =
  ["fn", Function; "let", Let];;

let lookup_identifier (identifier: string) : tokens =
  List.assoc identifier keywords;;

let hello () = print_endline "Hello"