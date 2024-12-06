print_endline "Hello, World!";;
print_endline ("Rémy's Tiny Basic interpreter (rtb) version " ^(match Build_info.V1.version () with
   | None -> "N/A"
   | Some v -> Build_info.V1.Version.to_string v));

open Interpreter;;

let input = ref "";;

let memory = init_memory ();;

(* let lexbuf = Lexing.from_string !input;;
let line_list = Parser.line_list Lexer.token lexbuf;;
List.map (fun x -> print_endline (line_to_string x)) line_list;; *)

let done_ = ref true;;

while !done_ do

  print_string "> ";
  let temp_input = 
    try
      read_line ()
    with
      End_of_file -> done_:=false; print_endline ""; "" in
  if temp_input = "run" then
    try 
      let lexbuf = Lexing.from_string !input in
      let line_list = Parser.line_list Lexer.token lexbuf in
      let code = line_list_to_code line_list in 
      run_lines memory code
    with
      Halted -> ()
    | Lexer.Error s -> print_endline s
    | Parser.Error -> print_endline "Syntax error"
    | Failure s -> print_endline s
  else if temp_input = "quit" then
    done_:=false
  else if temp_input = "clear" then
    input:=""  
  else if temp_input = "list" then
    print_string !input
  else
    input:=!input ^ temp_input ^ "\n" 
  

done

