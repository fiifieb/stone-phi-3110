type instr_type =
  | Addi
  | Add
  | Sub
  | Srl
  | Sll
  | Sra
  | Mv

(** [split_on_spaces s] splits the string [s] on ASCII space characters. The
    resulting list contains empty strings for consecutive spaces. Helper
    function used during instruction parsing. *)
let split_on_spaces = String.split_on_char ' '

(** [clean s] trims surrounding whitespace in [s] and replaces commas with
    spaces. This prepares an instruction string for tokenization. *)
let clean s =
  s |> String.trim |> String.map (fun c -> if c = ',' then ' ' else c)

(** [filter_empty lst] removes all empty strings [""] from [lst]. Used to ensure
    instruction components are meaningful tokens. *)
let filter_empty = List.filter (fun s -> s <> "")

(** [registers] is the global register file used by the emulator. It is an array
    of 32 integers, all initially 0. *)
let registers = Array.make 32 0

(** An [operand] represents one operand of a RISC-V instruction:
    - [Register r] represents a register name such as ["x1"].
    - [Value n] represents an immediate integer literal.
    - [None] is used for instructions that do not take a third operand. *)
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
(** An [instruction] record stores the parsed form of a RISC-V instruction.
    - [name] is the instruction variant (e.g. [Add], [Sub], [Mv]).
    - [op1], [op2], [op3] are its operands. *)

(** [convert_str_to_instr s] parses a single assembly instruction string [s]
    (e.g. ["add x1, x2, x3"]) into an [instruction] record.

    Commas are permitted, spacing may vary, and register names are preserved.
    Raises [Failure] if the instruction is empty, has the wrong number of
    operands, or contains an unsupported instruction name. *)
let convert_str_to_instr (input : string) : instruction =
  let cleaned = clean input in
  let components = cleaned |> split_on_spaces |> filter_empty in
  match components with
  | [] -> failwith "empty operands"
  | name :: operands_str -> (
      match (String.lowercase_ascii name, operands_str) with
      | ("add" | "sub" | "srl" | "sll" | "sra"), [ o1; o2; o3 ] ->
          {
            name =
              (match name with
              | "add" -> Add
              | "sub" -> Sub
              | "srl" -> Srl
              | "sll" -> Sll
              | "sra" -> Sra
              | _ -> failwith "unsopported instruction");
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

(** [make_instructions lst] converts a list of instruction strings [lst] into a
    list of parsed [instruction] values by applying [convert_str_to_instr] to
    each element. *)
let make_instructions (instructions_str_list : string list) : instruction list =
  List.map convert_str_to_instr instructions_str_list
