type token = {
  kind: string;
  literal: string
}

type tokens = 
  | Illegal
  | Eof 
  | Ident 
  | Int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Function
  | Let;; 