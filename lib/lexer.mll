{
  open Parser
  exception Error of string

  let pe = fun a b ->
    a := !a + b
  
  let colnum = ref 0
  let rownum = ref 0
}

let number = ['0'-'9'] +
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let other = ['_' ':']

let id = (letter| other) +

rule token = parse
| '(' { pe colnum 1; LPAR }
| ')' { pe colnum 1; RPAR }
| '+' { pe colnum 1; PLUS }
| '-' { pe colnum 1; MINUS }
| '*' { pe colnum 1; MULT }
| '/' { pe colnum 1; DIV }
| '>' { pe colnum 1; GT }
| '<' { pe colnum 1; LT }
| '=' { pe colnum 1; EQ }
| ',' { pe colnum 1; COMMA }
| ">=" { pe colnum 2; GTE }
| "<=" { pe colnum 2; LTE }
| "<>" { pe colnum 2; DIFF }
| "><" { pe colnum 2; FROWN }
| "print" { pe colnum 5; PRINT }
| "if" { pe colnum 2; IF }
| "then" { pe colnum 4; THEN }
| "goto" { pe colnum 4; GOTO }
| "input" { pe colnum 5; INPUT }
| "let" { pe colnum 3; LET }
| "gosub" { pe colnum 5; GOSUB }
| "return" { pe colnum 6; RETURN }
| "clear" { pe colnum 5; CLEAR }
| "list" { pe colnum 4; LIST }
| "run" { pe colnum 3; RUN }
| "end" { pe colnum 3; END }
| id as i { pe colnum (String.length i); VAR i}
| number as n { pe colnum (String.length n); NUMBER (int_of_string n)}
| '\"' ([^'\"']* as str) '\"' { pe colnum ((String.length str)+2); STRING str }
| [' ' '\t'] { pe colnum 1; token lexbuf }
| ['\n'] {pe rownum 1; colnum := 0; NEWLINE }
| eof { EOF }
| _ as c { raise (Error (Printf.sprintf "Line %d, column %d: unexpected character: '%c'\n" !rownum !colnum c)) }
