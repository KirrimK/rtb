open Ast;;

type code = line array * ((int * int) list)

type memory = (string * value) list * int list

exception Not_implemented;;
exception Halted;;

let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function s -> Str.bounded_split re s 2

let line_list_to_code (line_list: line list): code =
  let line_array = Array.of_list line_list in
  let line_assoc_pre = Array.mapi (fun i e -> 
      match e with
      | NumberedStatement(n, _) -> Some((n, i))
      | _ -> None
    ) line_array in
  let line_assoc = List.filter_map (fun x -> x) (Array.to_list (line_assoc_pre)) in

  (line_array, line_assoc)

let init_memory (): memory =
  ([], [])

let rec run_line (memory: memory) (code: code) (ip:int) : (memory * int) =
  let (line_array, line_assoc) = code in
  if ip >= (Array.length line_array) then
    raise Halted;
  let statement = match line_array.(ip) with
                    NumberedStatement (_, s) -> s
                  | Statement s -> s in
  run_statement memory line_assoc ip statement

and run_statement (memory: memory) (line_assoc: (int * int) list) (ip: int) (statement: statement): memory * int =
  let (var_assoc, ret_stack) = memory in
  try
    begin match statement with
      Print el ->
        print_endline (String.concat "" (List.map value_to_string (List.map (fun x -> calc_expr memory x) el))); (memory, ip+1)
    | If (ea, rl, eb, stat) ->
        if calc_comp memory ea rl eb then
          run_statement memory line_assoc ip stat
        else
          (memory, ip+1)
    | Goto e -> begin match calc_expr memory e with
                  Number n -> (memory, List.assoc n line_assoc)
                | String _ -> failwith "Cannot jump to non-int line number" end
    | Input var_list -> 
                begin let user_input = read_line () in
                let rec local = fun (vlist: string list) (out: (string * value) list) (input_buffer: string): (string * value) list ->
                  match vlist with
                    [a] -> (a, String input_buffer)::out
                  | hd_vlist::tl_vlist -> begin match split_on_first_space input_buffer with
                                [a] -> (hd_vlist,(String a))::out
                              | [hd_buffer;tl_buffer] -> local tl_vlist ((hd_vlist, (String hd_buffer))::out) tl_buffer
                              | _ -> out end 
                  | _ -> [] in
                let new_vars = local var_list [] user_input in
                ((new_vars @ var_assoc, ret_stack), ip+1)
                end
    | Let (s, e) -> let v = calc_expr memory e in (((s, v)::var_assoc, ret_stack), ip+1)
    | Gosub e -> begin match calc_expr memory e with
                  Number n -> ((var_assoc, (ip+1)::ret_stack), List.assoc n line_assoc)
                | String _ -> failwith "Cannot jump to non-int line number" end
    | Return ->  ((var_assoc, List.tl ret_stack), List.hd ret_stack)
    | Clear -> failwith "Clear command for interactive prompt only"
    | List -> failwith "List command for interactive prompt only"
    | Run -> failwith "Run command for interactive prompt only"
    | End -> raise Halted end
  with
    Failure s -> failwith ("Error on line " ^ (string_of_int ip) ^ " (" ^ (statement_to_string statement) ^ "): " ^ s)

and calc_expr (memory: memory) (e: expression) : value =
  let (var_assoc, _) = memory in
  match e with
  | Value v -> v
  | Var name -> 
      begin try
        List.assoc name var_assoc
      with Not_found -> failwith ("Variable '" ^ name ^ "' not found") end
  | Plus (ea, eb) -> 
      begin match calc_expr memory ea, calc_expr memory eb with
        Number na, Number nb -> Number (na+nb)
      | String sa, String sb -> String (sa^sb)
      | _ -> failwith "Unsupported operation" end
  | Minus (ea, eb) -> 
      begin match calc_expr memory ea, calc_expr memory eb with
        Number na, Number nb -> Number (na-nb)
      | _ -> failwith "Unsupported operation" end
  | Mult (ea, eb) ->
      begin match calc_expr memory ea, calc_expr memory eb with
        Number na, Number nb -> Number (na*nb)
      | _ -> failwith "Unsupported operation" end
  | Div (ea, eb) ->
      begin match calc_expr memory ea, calc_expr memory eb with
        Number na, Number nb -> Number (na/nb)
      | _ -> failwith "Unsupported operation" end
  | UnaryMinus (e) ->
      begin match calc_expr memory e with
        Number n -> Number (-n)
      | String _ -> failwith "Unsupported operation" end

and calc_comp (memory: memory) ea rl eb: bool =
  let va = calc_expr memory ea in
  let vb = calc_expr memory eb in
  match rl with
    Lt -> 
      begin match va, vb with
        Number na, Number nb -> na < nb
      | _, _ -> failwith "Unsupported operation" end
  | Lte -> 
      begin match va, vb with
        Number na, Number nb -> na <= nb
      | _, _ -> failwith "Unsupported operation" end
  | Gt -> 
      begin match va, vb with
        Number na, Number nb -> na > nb
      | _, _ -> failwith "Unsupported operation" end
  | Gte ->
      begin match va, vb with
        Number na, Number nb -> na >= nb
      | _, _ -> failwith "Unsupported operation" end
  | Eq -> va = vb
  | Diff -> va <> vb
  | Frown -> raise Not_implemented

let run_lines (memory: memory) (code: code): unit =
  let rec local memory ip =
    let (new_memory, new_ip) = run_line memory code ip in
    local new_memory new_ip in
  local memory 0 
