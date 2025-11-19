(** A supported RISC-V instruction variant.
    - [Addi] represents the ADDI immediate addition instruction.
    - [Add] represents register addition.
    - [Sub] represents register subtraction.
    - [Srl] represents logical right shift.
    - [Sll] represents logical left shift.
    - [Sra] represents arithmetic right shift.
    - [Mv] represents register-to-register move. *)
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

(**[parse_register s] takes a register name and converts it to int. Eg. x1 -> 1*)
let parse_register s =
  if String.length s < 2 || s.[0] <> 'x' then failwith ("invalid register: " ^ s)
  else
    let idx_str = String.sub s 1 (String.length s - 1) in
    let idx =
      try int_of_string idx_str
      with Failure _ -> failwith ("invalid register index: " ^ s)
    in
    if idx < 0 || idx > 31 then failwith ("register out of range: " ^ s)
    else idx

(** [registers] is the global register file used by the emulator. It is an array
    of 32 integers, all initially 0. *)
let registers = Array.make 32 0

(** An [operand] represents one operand of a RISC-V instruction:
    - [Register r] means register number [r] (0 ≤ r ≤ 31).
    - [Value n] eans the literal integer immediate [n].
    - [None] is used for instructions that do not take a third operand. *)
type operand =
  | Register of int
  | Value of int
  | None

(** An [alu_op] indicates the ALU operation that should be performed after
    decoding:
    - [ADD_OP] addition
    - [SUB_OP] subtraction
    - [SRL_OP] logical right shift
    - [SLL_OP] logical left shift
    - [SRA_OP] arithmetic right shift
    - [PASS_OP] pass-through (used for [mv], returning the source register). *)
type alu_op =
  | ADD_OP
  | SUB_OP
  | SRL_OP
  | SLL_OP
  | SRA_OP
  | PASS_OP

type instruction = {
  name : instr_type;
  op1 : operand;
  op2 : operand;
  op3 : operand;
}
(** An [instruction] record stores the parsed form of a RISC-V instruction.
    - [name] is the instruction variant (e.g. [Add], [Sub], [Mv]).
    - [op1], [op2], [op3] are its operands. *)

type cpu_state = {
  mutable pc : int;
  regs : int array;
  instrs : instruction array;
}
(** A [cpu_state] bundles all mutable processor state needed by the emulator:
    - [pc] holds the index of the next instruction to execute in [instrs].
    - [regs] is the register file (32 general-purpose integer registers).
    - [instrs] is the program loaded into the emulator as an array of parsed
      instructions. *)

type decoded = {
  dst : int option;
  src1 : int option;
  src2 : int option;
  imm : int option;
  alu : alu_op;
}
(** A [decoded] value represents the micro-operation obtained after decoding a
    single instruction:
    - [dst] is the destination register (if any).
    - [src1] is the first source register (if any).
    - [src2] is the second source register (if any).
    - [imm] is the immediate operand (if the instruction uses one).
    - [alu] specifies which ALU operation to perform. *)

(** [convert_str_to_instr s] parses a single assembly instruction string [s]
    (e.g. ["add x1, x2, x3"]) into an [instruction] record. Commas are
    permitted, spacing may vary, and register names are preserved. Raises
    [Failure] if the instruction is empty, has the wrong number of operands, or
    contains an unsupported instruction name. *)
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
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Register (parse_register o3);
          }
      | "mv", [ o1; o2 ] ->
          {
            name = Mv;
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = None;
          }
      | "addi", [ o1; o2; o3 ] ->
          {
            name = Addi;
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Value (int_of_string o3);
          }
      | _, _ -> failwith ("Wrong number of operands for instruction: " ^ name))

(** [make_instructions lst] converts a list of instruction strings [lst] into a
    list of parsed [instruction] values by applying [convert_str_to_instr] to
    each element. *)
let make_instructions (instructions_str_list : string list) : instruction list =
  List.map convert_str_to_instr instructions_str_list

(** [decode inst] translates the parsed instruction [inst] into a lower-level
    [decoded] micro-operation suitable for execution. It extracts the
    destination register, source registers, and immediate values as appropriate
    for the instruction variant, and selects the ALU operation that must be
    applied. The resulting [decoded] record determines how the CPU will update
    its state during execution. *)
let decode (inst : instruction) : decoded =
  let reg_of_operand = function
    | Register r -> Some r
    | Value _ -> None
    | None -> None
  in
  let imm_of_operand = function
    | Value n -> Some n
    | _ -> None
  in

  match inst.name with
  | Add ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = ADD_OP;
      }
  | Sub ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SUB_OP;
      }
  | Srl ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SRL_OP;
      }
  | Sll ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SLL_OP;
      }
  | Sra ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SRA_OP;
      }
  | Addi ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu = ADD_OP;
      }
  | Mv ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = None;
        alu = PASS_OP;
      }
