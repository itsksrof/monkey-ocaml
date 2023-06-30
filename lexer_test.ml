let test_lexer_assign =
  let input : string = "=" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in 
    if tok.kind != Token.Assign then
      Printf.eprintf "[failed] test_lexer_assign -> wrong token kind\n";
    if String.compare tok.literal "=" != 0 then
      Printf.eprintf "[failed] test_lexer_assign -> wrong token literal\n";;

let test_lexer_plus =
  let input : string = "+" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Plus then
      Printf.eprintf "[failed] test_lexer_plus -> wrong token kind\n";
    if String.compare tok.literal "+" != 0 then
      Printf.eprintf "[failed] test_lexer_plus -> wrong token literal\n";;

let test_lexer_comma =
  let input : string = "," in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Comma then
      Printf.eprintf "[failed] test_lexer_comma -> wrong token kind\n";
    if String.compare tok.literal "," != 0 then
      Printf.eprintf "[failed] test_lexer_comma -> wrong token literal\n";;

let test_lexer_semicolon =
  let input : string = ";" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Semicolon then
      Printf.eprintf "[failed] test_lexer_semicolon -> wrong token kind\n";
    if String.compare tok.literal ";" != 0 then
      Printf.eprintf "[failed] test_lexer_semicolon -> wrong token literal\n";;
      
let test_lexer_leftparen =
  let input : string = "(" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.LeftParen then
      Printf.eprintf "[failed] test_lexer_leftparen -> wrong token kind\n";
    if String.compare tok.literal "(" != 0 then
      Printf.eprintf "[failed] test_lexer_leftparen -> wrong token literal\n";;

let test_lexer_rightparen =
  let input : string = ")" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.RightParen then
      Printf.eprintf "[failed] test_lexer_rightparen -> wrong token kind\n";
    if String.compare tok.literal ")" != 0 then
      Printf.eprintf "[failed] test_lexer_rightparen -> wrong token literal\n";;

let test_lexer_leftbrace =
  let input : string = "{" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.LeftBrace then
      Printf.eprintf "[failed] test_lexer_leftbrace -> wrong token kind\n";
    if String.compare tok.literal "{" != 0 then
      Printf.eprintf "[failed] test_lexer_leftbrace -> wrong token literal\n";;

let test_lexer_rightbrace =
  let input : string = "}" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.RightBrace then
      Printf.eprintf "[failed] test_lexer_rightbrace -> wrong token kind\n";
    if String.compare tok.literal "}" != 0 then
      Printf.eprintf "[failed] test_lexer_rightbrace -> wrong token literal\n";;

let test_lexer_function =
  let input : string = "fn" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Function then
      Printf.eprintf "[failed] test_lexer_function -> wrong token kind\n";
    if String.compare tok.literal "fn" != 0 then
      Printf.eprintf "[failed] test_lexer_function -> wrong token literal\n";;

let test_lexer_let =
  let input : string = "let" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Let then
      Printf.eprintf "[failed] test_lexer_let -> wrong token kind\n";
    if String.compare tok.literal "let" != 0 then
      Printf.eprintf "[failed] test_lexer_let -> wrong token literal\n";;

let test_lexer_int =
  let input : string = "8" in
  let lexer = Lexer.new_lexer input in
  let tok = Lexer.next_token lexer in
    if tok.kind != Token.Int then
      Printf.eprintf "[failed] test_lexer_int -> wrong token kind\n";
    if String.compare tok.literal "8" != 0 then
      Printf.eprintf "[failed] test_lexer_int -> wrong token literal\n";;

let () =
  test_lexer_assign;
  test_lexer_plus;
  test_lexer_comma;
  test_lexer_semicolon;
  test_lexer_leftparen;
  test_lexer_rightparen;
  test_lexer_leftbrace;
  test_lexer_rightbrace;
  test_lexer_function;
  test_lexer_let;
  test_lexer_int