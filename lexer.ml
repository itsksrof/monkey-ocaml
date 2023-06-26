type lexer = {
  mutable input: string;
  mutable position: int;
  mutable read_pos: int;
  mutable ch: char;
}

let read_char (l: lexer) =
  let read =
    if l.read_pos >= String.length l.input then
      l.ch <- '\x00'
    else
      l.ch <- String.get l.input l.read_pos
    in 
      l.position <- l.read_pos;
      l.read_pos <- l.read_pos + 1;
      read;; 

let new_lexer (input: string) : lexer =
  let l =
    {input = input; position = 0; read_pos = 0; ch = '\x00'}
  in
    read_char(l);
    l;;

let new_token (kind: Token.tokens) (ch: char) : Token.token =
  {kind = kind; literal = String.make 1 ch};;
  
let is_letter (ch: char) : bool =
  'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';;

let read_identifier (l: lexer) : string =
  let position = l.position in
    while is_letter(l.ch) do
      read_char(l)
    done;
    String.sub l.input position l.position;;

let skip_whitespace (l: lexer) =
  while l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' do
    read_char(l)
  done;;

let next_token (l: lexer) : Token.token =
  skip_whitespace(l);
  let match_token =
    match l.ch with
    | '=' -> new_token Token.Assign l.ch
    | '+' -> new_token Token.Plus l.ch
    | ',' -> new_token Token.Comma l.ch
    | ';' -> new_token Token.Semicolon l.ch
    | '(' -> new_token Token.LeftParen l.ch
    | ')' -> new_token Token.RightParen l.ch
    | '{' -> new_token Token.LeftBrace l.ch
    | '}' -> new_token Token.RightBrace l.ch
    | '\x00' -> new_token Token.Eof l.ch
    | _ -> 
      if is_letter(l.ch) then
        let literal = read_identifier(l) in
          {kind = Token.lookup_identifier(literal); literal = read_identifier(l)} 
      else
        new_token Token.Illegal l.ch
  in 
    read_char(l);
    match_token;; 