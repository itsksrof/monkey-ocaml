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
  | Identifier
  | Int
  | Eof
  | Illegal;;

type token = {
  kind: tokens;
  literal: string;
}

let keywords =
  ["fn", Function; "let", Let];;

let lookup_identifier (identifier: string) : tokens =
  try List.assoc identifier keywords with Not_found -> Identifier;;