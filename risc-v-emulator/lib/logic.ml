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
  | LW (* Memory Stuff *)
  | LB
  | LD
  | SW
  | SB
  | SD

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
(*Is this ^^^ vestigial code? Like is this needed anymore?*)

(* ------ Main Memory Implementation ------ *)
(* We're gonna simulate main memory with a 
hash table*)

(** [main_mem] is our representation of memory, using a hash table to store ints*)
let main_mem = Hashtbl.create 4

(** [key_create a] creates a key value associated with a number, a*)
let key_create = Hashtbl.hash

(** [add_to_mem a data] creates a key with the value a to store data in the hash
    table*)
let add_to_mem (a : int) (data : int) =
  let key = key_create a in
  Hashtbl.add main_mem key data

(** [get_from_mem] obtains the data tied to the key created by calling
    key_create on a*)
let get_from_mem (a : int) =
  let key = key_create a in
  Hashtbl.find_opt main_mem key

(** [clean_for_mem s] is [clean s] except it replaces ( and ) with whitespaces.
    This is so for load and store commands we can correct parse strings in the
    form of offset(rsn) *)
let clean_for_mem (s : string) =
  s |> String.trim
  |> String.map (fun c -> if c = '(' || c = ')' then ' ' else c)

(** [parse_register_for_mem s] is like [parse_register s] but specialize to work
    on the form offset(rsn), returns a tuple of the offset amount and register
    number*)
let parse_register_for_mem (s : string) =
  if String.length s < 5 then failwith ("invalid offset form: " ^ s)
  else
    let offset_idx_str = clean_for_mem s |> split_on_spaces in
    let offset = int_of_string (List.nth offset_idx_str 0) in
    let idx = List.nth offset_idx_str 1 in
    let id = int_of_string (String.sub idx 1 (String.length idx - 1)) in
    (offset, id)

let mem_id ((a, b) : int * int) : int =
  match (a, b) with
  | _, y -> y

let offset_amount ((a, b) : int * int) : int =
  match (a, b) with
  | x, _ -> x
(* ----- Might be unneeded idk ----- *)

(** [remove_from_mem] removes the data tied to the key created by calling
    key_create on a*)
let remove_from_mem (a : int) =
  let key = key_create a in
  Hashtbl.remove main_mem key

(** [get_remove] gets and then removes the data tied to the key created by
    calling key_create on a*)
let get_remove (a : int) =
  let value = get_from_mem a in
  let () = remove_from_mem a in
  value

(* ---------------------------------------- *)

(** An [operand] represents one operand of a RISC-V instruction:
    - [Register r] means register number [r] (0 ≤ r ≤ 31).
    - [Value n] means the literal integer immediate [n].
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
    - [PASS_OP] pass-through (used for [mv], returning the source register).
    - [LOAD8_OP] loading 8-bits from memory
    - [STORE8_OP] storing 8-bits to memory
    - [LOAD32_OP] loading 32-bits from memory
    - [STORE32_OP] storing 32-bits to memory
    - [LOAD64_OP] loading 64-bits from memory
    - [STORE64_OP] storing 64-bits to memory*)
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
  (* Memory Stuff *)
  | LOAD8_OP
  | STORE8_OP
  | LOAD32_OP
  | STORE32_OP
  | LOAD64_OP
  | STORE64_OP

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
  instr_strings : string array;
}
(** A [cpu_state] bundles all mutable processor state needed by the emulator:
    - [pc] holds the index of the next instruction to execute in [instrs].
    - [regs] is the register file (32 general-purpose integer registers).
    - [instrs] is the program loaded into the emulator as an array of parsed
      instructions.
    - [instr_strings] holds the original string representation of each
      instruction for display purposes. *)

type decoded = {
  dst : int option;
  src1 : int option;
  src2 : int option;
  imm : int option;
  alu : alu_op;
  branch : string option;
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
      let lname = String.lowercase_ascii name in
      match (lname, operands_str) with
      | ("add" | "sub" | "srl" | "sll" | "sra"), [ o1; o2; o3 ] ->
          {
            name =
              (match lname with
              | "add" -> Add
              | "sub" -> Sub
              | "srl" -> Srl
              | "sll" -> Sll
              | "sra" -> Sra
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Register (parse_register o3);
          }
      | ("slli" | "srli" | "srai"), [ o1; o2; o3 ] ->
          {
            name =
              (match lname with
              | "slli" -> Slli
              | "srli" -> Srli
              | "srai" -> Srai
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Value (int_of_string o3);
          }
      | ("beq" | "bne" | "blt" | "bge"), [ o1; o2; o3 ] ->
          {
            name =
              (match lname with
              | "beq" -> Beq
              | "bne" -> Bne
              | "blt" -> Blt
              | "bge" -> Bge
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Value (int_of_string o3);
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
      | ("and" | "or" | "xor"), [ o1; o2; o3 ] ->
          {
            name =
              (match lname with
              | "and" -> And
              | "or" -> Or
              | "xor" -> Xor
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Register (parse_register o3);
          }
      | ("andi" | "ori" | "xori"), [ o1; o2; o3 ] ->
          {
            name =
              (match lname with
              | "andi" -> Andi
              | "ori" -> Ori
              | "xori" -> Xori
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Value (int_of_string o3);
          }
      | "jal", [ o1; o2 ] ->
          {
            name = Jal;
            op1 = Register (parse_register o1);
            op2 = None;
            op3 = Value (int_of_string o2);
          }
      | "jalr", [ o1; o2; o3 ] ->
          {
            name = Jalr;
            op1 = Register (parse_register o1);
            op2 = Register (parse_register o2);
            op3 = Value (int_of_string o3);
          }
      | ("lw" | "lb" | "ld"), [ o1; o2 ] ->
          let offset, reg = parse_register_for_mem o2 in
          {
            name =
              (match lname with
              | "lw" -> LW
              | "lb" -> LB
              | "ld" -> LD
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register reg;
            op3 = Value offset;
          }
      | ("sw" | "sb" | "sd"), [ o1; o2 ] ->
          let offset, reg = parse_register_for_mem o2 in
          {
            name =
              (match lname with
              | "sw" -> SW
              | "sb" -> SB
              | "sd" -> SD
              | _ -> failwith "unsupported instruction");
            op1 = Register (parse_register o1);
            op2 = Register reg;
            op3 = Value offset;
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
        branch = None;
      }
  | Sub ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SUB_OP;
        branch = None;
      }
  | Srl ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SRL_OP;
        branch = None;
      }
  | Sll ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SLL_OP;
        branch = None;
      }
  | Sra ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu = SRA_OP;
        branch = None;
      }
  | Addi ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu = ADD_OP;
        branch = None;
      }
  | And | Or | Xor ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op3;
        imm = None;
        alu =
          (match inst.name with
          | And -> AND_OP
          | Or -> OR_OP
          | Xor -> XOR_OP
          | _ -> PASS_OP);
        branch = None;
      }
  | Andi | Ori | Xori ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu =
          (match inst.name with
          | Andi -> AND_OP
          | Ori -> OR_OP
          | Xori -> XOR_OP
          | _ -> PASS_OP);
        branch = None;
      }
  | Mv ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = None;
        alu = PASS_OP;
        branch = None;
      }
  | Slli | Srli | Srai ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu =
          (match inst.name with
          | Slli -> SLL_OP
          | Srli -> SRL_OP
          | Srai -> SRA_OP
          | _ -> PASS_OP);
        branch = None;
      }
  | Beq | Bne | Blt | Bge ->
      {
        dst = None;
        src1 = reg_of_operand inst.op1;
        src2 = reg_of_operand inst.op2;
        imm = imm_of_operand inst.op3;
        alu = PASS_OP;
        branch =
          Some
            (match inst.name with
            | Beq -> "beq"
            | Bne -> "bne"
            | Blt -> "blt"
            | Bge -> "bge"
            | _ -> "");
      }
  | Jal ->
      {
        dst = reg_of_operand inst.op1;
        src1 = None;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu = PASS_OP;
        branch = Some "jal";
      }
  | Jalr ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu = PASS_OP;
        branch = Some "jalr";
      }
  | LW | LB | LD ->
      {
        dst = reg_of_operand inst.op1;
        src1 = reg_of_operand inst.op2;
        src2 = None;
        imm = imm_of_operand inst.op3;
        alu =
          (match inst.name with
          | LW -> LOAD8_OP
          | LB -> LOAD32_OP
          | LD -> LOAD64_OP
          | _ -> PASS_OP);
        branch = None;
      }
  | SW | SB | SD ->
      {
        dst = None;
        src1 = reg_of_operand inst.op2;
        src2 = reg_of_operand inst.op1;
        imm = imm_of_operand inst.op3;
        alu =
          (match inst.name with
          | SW -> STORE8_OP
          | SB -> STORE32_OP
          | SD -> STORE64_OP
          | _ -> PASS_OP);
        branch = None;
      }

(** [alu_execute op a b] performs the ALU operation [op] on operands [a] and [b]
    and returns the resulting integer. For operations that ignore [b] (e.g.
    [PASS_OP]) the value of [b] is unused. Shift amounts are masked to the low 5
    bits (0-31) to model RV32I behaviour. *)
let alu_execute op a b =
  let shamt = b in
  let a32 = Int32.of_int a in
  let b32 = Int32.of_int b in
  match op with
  | ADD_OP -> Int32.to_int (Int32.add a32 b32)
  | SUB_OP -> Int32.to_int (Int32.sub a32 b32)
  | SLL_OP -> Int32.to_int (Int32.shift_left a32 shamt)
  | SRL_OP -> Int32.to_int (Int32.shift_right_logical a32 shamt)
  | SRA_OP -> Int32.to_int (Int32.shift_right a32 shamt)
  | AND_OP -> Int32.to_int (Int32.logand a32 b32)
  | OR_OP -> Int32.to_int (Int32.logor a32 b32)
  | XOR_OP -> Int32.to_int (Int32.logxor a32 b32)
  | PASS_OP -> Int32.to_int a32
  (* Memory operations - these are handled specially in exec_decoded *)
  | LOAD8_OP | LOAD32_OP | LOAD64_OP ->
      0 (* placeholder, actual load in exec_decoded *)
  | STORE8_OP | STORE32_OP | STORE64_OP ->
      0 (* placeholder, actual store in exec_decoded *)

(** [exec_decoded cpu d] executes a decoded micro-op [d] on [cpu], writing the
    result into the destination register if present. Writes to register 0 are
    ignored (x0 is hard-wired zero). *)
let exec_decoded (cpu : cpu_state) (d : decoded) : unit =
  let regs = cpu.regs in
  (* Handle memory operations specially *)
  match d.alu with
  | LOAD8_OP | LOAD32_OP | LOAD64_OP -> (
      let base =
        match d.src1 with
        | Some i -> regs.(i)
        | None -> 0
      in
      let offset =
        match d.imm with
        | Some n -> n
        | None -> 0
      in
      let addr = base + offset in
      let value =
        match get_from_mem addr with
        | Some v -> v
        | None -> 0 (* Return 0 if memory location not initialized *)
      in
      match d.dst with
      | Some 0 -> ()
      | Some dst -> regs.(dst) <- value
      | None -> ())
  | STORE8_OP | STORE32_OP | STORE64_OP ->
      let base =
        match d.src1 with
        | Some i -> regs.(i)
        | None -> 0
      in
      let offset =
        match d.imm with
        | Some n -> n
        | None -> 0
      in
      let addr = base + offset in
      let data =
        match d.src2 with
        | Some i -> regs.(i)
        | None -> 0
      in
      add_to_mem addr data
  | _ -> (
      (* Regular ALU operations *)
      let src1_val =
        match d.src1 with
        | Some i -> regs.(i)
        | None -> 0
      in
      let src2_val =
        match (d.src2, d.imm) with
        | Some i, _ -> regs.(i)
        | None, Some imm -> imm
        | None, None -> 0
      in
      let result = alu_execute d.alu src1_val src2_val in
      match d.dst with
      | Some 0 -> ()
      | Some dst -> regs.(dst) <- result
      | None -> ())

(** [exec_instruction cpu inst] decodes and executes a single instruction [inst]
    on [cpu]. *)
let exec_instruction (cpu : cpu_state) (inst : instruction) : unit =
  let d = decode inst in
  exec_decoded cpu d

(** [run cpu] executes all instructions in [cpu.instrs] sequentially in program
    order. After completion, [cpu.pc] is set to the number of instructions
    executed. *)
let run (cpu : cpu_state) : unit =
  let len = Array.length cpu.instrs in
  let i = ref 0 in
  while !i < len do
    cpu.pc <- !i;
    let inst = cpu.instrs.(!i) in
    let d = decode inst in
    match d.branch with
    | Some b -> (
        let regs = cpu.regs in
        match b with
        | "beq" | "bne" | "blt" | "bge" ->
            let v1 =
              match d.src1 with
              | Some r -> regs.(r)
              | None -> failwith "branch missing src1"
            in
            let v2 =
              match d.src2 with
              | Some r -> regs.(r)
              | None -> failwith "branch missing src2"
            in
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "branch missing imm"
            in
            let take =
              match b with
              | "beq" -> v1 = v2
              | "bne" -> v1 <> v2
              | "blt" -> v1 < v2
              | "bge" -> v1 >= v2
              | _ -> false
            in
            if take then i := !i + offset else i := !i + 1
        | "jal" ->
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "jal missing imm"
            in
            (match d.dst with
            | Some 0 -> ()
            | Some dst -> regs.(dst) <- !i + 1
            | None -> ());
            i := !i + offset
        | "jalr" ->
            let base =
              match d.src1 with
              | Some r -> regs.(r)
              | None -> failwith "jalr missing src1"
            in
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "jalr missing imm"
            in
            (match d.dst with
            | Some 0 -> ()
            | Some dst -> regs.(dst) <- !i + 1
            | None -> ());
            i := base + offset
        | _ -> i := !i + 1)
    | None ->
        exec_decoded cpu d;
        i := !i + 1
  done;
  cpu.pc <- !i

(** [step cpu] executes a single instruction at [cpu.pc]. Updates [cpu.pc] to
    point to the next instruction (or beyond the end if execution is complete).
    Does nothing if [cpu.pc] is already at or beyond the end of the instruction
    array. *)
let step (cpu : cpu_state) : unit =
  let len = Array.length cpu.instrs in
  if cpu.pc < len then
    let inst = cpu.instrs.(cpu.pc) in
    let d = decode inst in
    match d.branch with
    | Some b -> (
        let regs = cpu.regs in
        match b with
        | "beq" | "bne" | "blt" | "bge" ->
            let v1 =
              match d.src1 with
              | Some r -> regs.(r)
              | None -> failwith "branch missing src1"
            in
            let v2 =
              match d.src2 with
              | Some r -> regs.(r)
              | None -> failwith "branch missing src2"
            in
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "branch missing imm"
            in
            let take =
              match b with
              | "beq" -> v1 = v2
              | "bne" -> v1 <> v2
              | "blt" -> v1 < v2
              | "bge" -> v1 >= v2
              | _ -> false
            in
            if take then cpu.pc <- cpu.pc + offset else cpu.pc <- cpu.pc + 1
        | "jal" ->
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "jal missing imm"
            in
            (match d.dst with
            | Some 0 -> ()
            | Some dst -> regs.(dst) <- cpu.pc + 1
            | None -> ());
            cpu.pc <- cpu.pc + offset
        | "jalr" ->
            let base =
              match d.src1 with
              | Some r -> regs.(r)
              | None -> failwith "jalr missing src1"
            in
            let offset =
              match d.imm with
              | Some n -> n
              | None -> failwith "jalr missing imm"
            in
            (match d.dst with
            | Some 0 -> ()
            | Some dst -> regs.(dst) <- cpu.pc + 1
            | None -> ());
            cpu.pc <- base + offset
        | _ -> cpu.pc <- cpu.pc + 1)
    | None ->
        exec_decoded cpu d;
        cpu.pc <- cpu.pc + 1
