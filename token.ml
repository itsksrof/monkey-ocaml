type tokens = 
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Eof
  | Letter
  | Illegal;;

type token = {
  kind: tokens;
  literal: string;
} 

let hello () = print_endline "Hello"