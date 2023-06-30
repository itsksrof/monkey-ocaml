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
      Printf.eprintf "[failed] test_lexer_plus -> wrong token literal \n";;

let () =
  test_lexer_assign;
  test_lexer_plus