type lexer = {
  mutable input: string;
  mutable position: int;
  mutable read_pos: int;
  mutable ch: char;
}

let read_char (l: lexer) =
  if l.read_pos >= String.length l.input then
    l.ch <- char_of_int 0
  else
    l.ch <- String.get l.input l.read_pos;
  l.position <- l.read_pos;
  l.read_pos <- l.read_pos + 1;;

let new_lexer (input: string) : lexer =
  let l =
    {input = input; position = 0; read_pos = 0; ch = char_of_int 0}
  in
    read_char(l);
    l;;

let new_token (kind: Token.tokens) (ch: char) : Token.token =
  {kind = kind; literal = String.make 1 ch};;
  
let is_letter (ch: char) : bool =
  'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';;

let is_digit (ch: char) : bool =
  '0' <= ch && ch <= '9';;

let is_eof (ch: char) : bool =
  char_of_int 0 = ch;;

let read_identifier (l: lexer) : string =
  let position = l.position in
    while is_letter(l.ch) do
      read_char(l)
    done;
    let len = l.position - position in
    String.sub l.input position len;;

let read_number (l: lexer) : string =
  let position = l.position in
    while is_digit(l.ch) do
      read_char(l)
    done;
    let len = l.position - position in
    String.sub l.input position len;;

let skip_whitespace (l: lexer) =
  while l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' do
    read_char(l)
  done;;

let next_token (l: lexer) : Token.token =
  skip_whitespace(l);
  let tok = match l.ch with
  | '=' -> new_token Token.Assign l.ch
  | '+' -> new_token Token.Plus l.ch
  | ',' -> new_token Token.Comma l.ch
  | ';' -> new_token Token.Semicolon l.ch
  | '(' -> new_token Token.LeftParen l.ch
  | ')' -> new_token Token.RightParen l.ch
  | '{' -> new_token Token.LeftBrace l.ch
  | '}' -> new_token Token.RightBrace l.ch
  | _ ->
    if is_letter l.ch then
      let literal = read_identifier l in
      {kind = Token.lookup_identifier literal; literal = literal}
    else if is_digit l.ch then
      {kind = Token.Int; literal = read_number l}
    else if is_eof l.ch then
      new_token Token.Eof l.ch
    else
      new_token Token.Illegal l.ch
    in read_char l; tok;;