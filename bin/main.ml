open Interpreter;;

let usage_msg = Printf.sprintf "Usage: %s [-c your_script.rtb]" Sys.argv.(0)
let input_file = ref ""
let version_print = ref false
let speclist =
  [
    ("-c", Arg.Set_string input_file, "rtb script to run.");
    ("--version", Arg.Set version_print, "Print version information")
  ];;

Arg.parse speclist ignore usage_msg;;

let print_version () =
  print_endline ("rtb version " ^(match Build_info.V1.version () with
  | None -> "N/A"
  | Some v -> Build_info.V1.Version.to_string v));
  print_endline "Rémy's Tiny Basic interpreter";
  print_endline "Copyright (C) 2024 Rémy B.";
  print_endline "License MIT <https://github.com/KirrimK/rtb/blob/main/LICENSE>";;

if !version_print then
  print_version ()
else
  if !input_file <> "" then

    try
      let file_in = open_in !input_file in
      let file_contents = really_input_string file_in (in_channel_length file_in) in
      let () = close_in file_in in
      let memory = init_memory () in
      let lexbuf = Lexing.from_string file_contents in
      let line_list = Parser.line_list Lexer.token lexbuf in
      let code = line_list_to_code line_list in 
      run_lines memory code
    with
      Halted -> ()
    | Lexer.Error s -> print_endline s
    | Parser.Error -> print_endline "Syntax error"
    | Failure s -> print_endline s

  else
    
    print_version ();

    let input = ref "" in
    let done_ = ref true in

    while !done_ do

      print_string "> ";
      let temp_input = 
        try
          read_line ()
        with
          End_of_file -> done_:=false; print_endline ""; "" in
      if temp_input = "run" then
        try
          let memory = init_memory () in
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

