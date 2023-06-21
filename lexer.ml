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
    in read;

  l.position <- l.read_pos;
  l.read_pos <- l.read_pos + 1;;

let new_lexer (input: string) : lexer =
  let l =
    {input = input; position = 0; read_pos = 0; ch = '\x00'}
  in
    read_char(l);
    l;;
