let () = print_endline "Hello, World!"

open Interpreter;;

let input = ref "";;

let memory = init_memory ();;

(* let lexbuf = Lexing.from_string !input;;
let line_list = Parser.line_list Lexer.token lexbuf;;
List.map (fun x -> print_endline (line_to_string x)) line_list;; *)

let done_ = ref true;;

while !done_ do

  print_string "> ";
  let temp_input = (read_line ()) in
  if temp_input = "run" then
    let lexbuf = Lexing.from_string !input in
    let line_list = Parser.line_list Lexer.token lexbuf in
    let code = line_list_to_code line_list in 
    run_lines memory code;
  else if temp_input = "quit" then
    done_:=false
  else if temp_input = "clear" then
    input:=""  
  else if temp_input = "list" then
    print_endline !input
  else
    if !input = "" then
      input:=temp_input
    else
      input:=!input ^ "\n" ^ temp_input
  

done

