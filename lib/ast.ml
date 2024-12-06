
type line =
  NumberedStatement of int * statement
| Statement of statement

and statement =
  Print of expression list
| If of expression * relop * expression * statement
| Goto of expression
| Input of string list
| Let of string * expression
| Gosub of expression
| Return
| Clear
| List
| Run
| End

and expression =
  Plus of expression * expression
| Minus of expression * expression
| Mult of expression * expression
| Div of expression * expression
| UnaryMinus of expression
| Var of string
| Value of value

and value = 
| Number of int
| String of string

and relop = 
  Lt
| Lte
| Gt
| Gte
| Eq
| Diff
| Frown

let relop_to_string (relop : relop) : string =
  match relop with
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Eq -> "="
  | Diff -> "<>"
  | Frown -> "><"

let value_to_string ?(debug=false) (value: value) : string =
  match value with
  | Number num -> string_of_int num
  | String str -> if debug then
                    "\"" ^ str ^ "\""
                  else
                    str

let rec expression_to_string (expression : expression) : string =
  match expression with
  | Plus (left, right) -> "(" ^ expression_to_string left ^ " + " ^ expression_to_string right ^ ")"
  | Minus (left, right) -> "(" ^ expression_to_string left ^ " - " ^ expression_to_string right ^ ")"
  | Mult (left, right) -> "(" ^ expression_to_string left ^ " * " ^ expression_to_string right ^ ")"
  | Div (left, right) -> "(" ^ expression_to_string left ^ " / " ^ expression_to_string right ^ ")"
  | UnaryMinus expr -> "-" ^ expression_to_string expr
  | Var name -> "Var: " ^ name
  | Value value -> "Value: " ^ value_to_string ~debug:true value

let expression_to_string_list (exprs : expression list) : string =
  match exprs with
  | [] -> ""
  | [expr] -> expression_to_string expr
  | _ -> "[" ^ String.concat ", " (List.map expression_to_string exprs) ^ "]"

let rec statement_to_string (statement : statement) : string =
  match statement with
  | Print exprs -> "Print: " ^ expression_to_string_list exprs
  | If (ea, rl, eb, stmt) ->
      "If (" ^ expression_to_string ea ^ relop_to_string rl ^ expression_to_string eb ^ ") {" ^
      statement_to_string stmt ^ "}"
  | Goto expr -> "Goto: " ^ expression_to_string expr
  | Input strs ->
      "Input: [" ^
      String.concat ", " strs ^ "]"
  | Let (name, expr) ->
      "Let (" ^ name ^ ") = " ^ expression_to_string expr
  | Gosub expr -> "Gosub: " ^ expression_to_string expr
  | Return -> "Return"
  | Clear -> "Clear"
  | List -> "List"
  | Run -> "Run"
  | End -> "End"

let line_to_string (line : line) : string =
  match line with
  | NumberedStatement (n, statement) -> "Numbered Statement: " ^ string_of_int n ^ " - " ^ statement_to_string statement
  | Statement statement -> "Statement: " ^ statement_to_string statement
