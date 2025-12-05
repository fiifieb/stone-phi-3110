(** Public interface for the RISC-V emulator logic. *)

(** A supported RISC-V instruction variant. *)
type instr_type =
  | Addi
  | Add
  | Sub
  | Srl
  | Sll
  | Sra
  | Slli
  | Srli
  | Srai
  | Beq
  | Bne
  | Blt
  | Bge
  | Mv
  | And
  | Or
  | Xor
  | Andi
  | Ori
  | Xori
  | Jal
  | Jalr
  | LW
  | LB
  | LD
  | SW
  | SB
  | SD

(** An [operand] represents one operand of a RISC-V instruction. *)
type operand =
  | Register of int
  | Value of int
  | None

(** An [alu_op] indicates the ALU operation to be performed. *)
type alu_op =
  | ADD_OP
  | SUB_OP
  | SRL_OP
  | SLL_OP
  | SRA_OP
  | PASS_OP
  | AND_OP
  | OR_OP
  | XOR_OP

type memory_op =
  | LOAD8_OP
  | STORE8_OP
  | LOAD32_OP
  | STORE32_OP
  | LOAD64_OP
  | STORE64_OP
  | PASS_OP

type instruction = {
  name : instr_type;
  op1 : operand;
  op2 : operand;
  op3 : operand;
}
(** An [instruction] record stores the parsed form of a RISC-V instruction. *)

type cpu_state = {
  mutable pc : int;
  regs : int array;
  instrs : instruction array;
  instr_strings : string array;
}
(** A [cpu_state] bundles all mutable processor state needed by the emulator. *)

type decoded = {
  dst : int option;
  src1 : int option;
  src2 : int option;
  imm : int option;
  alu : alu_op;
  memory : memory_op;
  branch : string option;
}
(** A [decoded] instruction contains extracted operand indices and the ALU
    operation. *)

val clean : string -> string
(** [clean s] trims surrounding whitespace in [s] and replaces commas with
    spaces. *)

val parse_register : string -> int
(** [parse_register s] takes a register name and converts it to an index. *)

val convert_str_to_instr : string -> instruction
(** [convert_str_to_instr input] parses a single instruction string into an
    [instruction] record. *)

val make_instructions : string list -> instruction list
(** [make_instructions instructions_str_list] converts a list of instruction
    strings into a list of [instruction] records. *)

val decode : instruction -> decoded
(** [decode inst] decodes an [instruction] into a [decoded] form with extracted
    operands and ALU operation. *)

val run : cpu_state -> unit
(** [run cpu] executes all instructions in [cpu.instrs] sequentially in program
    order. *)

val step : cpu_state -> unit
(** [step cpu] executes a single instruction at [cpu.pc] and updates [cpu.pc]
    accordingly. *)

val get_from_mem : int -> int option
(** [get_from_mem addr] retrieves the value stored at memory address [addr].
    Returns [None] if the address has not been written to. *)

val main_mem : (int, int) Hashtbl.t
(** [main_mem] is the hash table representing main memory. *)
