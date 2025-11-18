type instr_type =
  | Addi
  | Add
  | Sub
  | Srl
  | Sll
  | Sra
  | Mv

let split_on_spaces = String.split_on_char ' '
let registers = Array.make 32 0

type operand =
  | Register of string
  | Value of int
  | None

type instruction = {
  name : instr_type;
  op1 : operand;
  op2 : operand;
  op3 : operand;
}

let convert_str_to_instr (input : string) : instruction =
  let components = split_on_spaces input in
  match components with
  | [] -> failwith "empty operands"
  | name :: operands_str -> (
      match (String.lowercase_ascii name, operands_str) with
      | ("add" | "and" | "sub" | "srl" | "sll" | "sra"), [ o1; o2; o3 ] ->
          {
            name =
              (match name with
              | "add" -> Add
              | "sub" -> Sub
              | "srl" -> Srl
              | "sll" -> Sll
              | "sra" -> Sra
              | _ -> assert false);
            op1 = Register o1;
            op2 = Register o2;
            op3 = Register o3;
          }
      | "mv", [ o1; o2 ] ->
          { name = Mv; op1 = Register o1; op2 = Register o2; op3 = None }
      | "addi", [ o1; o2; o3 ] ->
          {
            name = Addi;
            op1 = Register o1;
            op2 = Register o2;
            op3 = Value (int_of_string o3);
          }
      | _, _ -> failwith ("Wrong number of operands for instruction: " ^ name))

let make_instructions (instructions_str_list : string list) : instruction list =
  List.map convert_str_to_instr instructions_str_list
