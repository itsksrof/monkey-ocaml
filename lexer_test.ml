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

let () =
  test_lexer_assign;
  test_lexer_plus;
  test_lexer_comma;
  test_lexer_semicolon;
  test_lexer_leftparen;
  test_lexer_rightparen