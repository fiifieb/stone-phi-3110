open OUnit2
open Risc_v_emulator.Logic

let tests =
  "test suite"
  >::: [
         ("a trivial test" >:: fun _ -> assert_equal 0 0);
         (*Basic Behavior Tests*)
         ( "Cleaning a string" >:: fun _ ->
           let string_of_inst = "  add,x1,x3,x4 " in
           let clean_string = clean string_of_inst in
           assert_equal clean_string "add x1 x3 x4" );
         ( "Converting a type string to a type Instruction" >:: fun _ ->
           let stringOfInst = "add x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let oughtInst : instruction =
             {
               name = Add;
               op1 = Register 1;
               op2 = Register 2;
               op3 = Register 3;
             }
           in
           assert_equal inst oughtInst );
         ( "Test if we fail gracefully if we are given an empty string to \
            convert"
         >:: fun _ ->
           let stringOfInstList = "" in
           let err = fun () -> convert_str_to_instr stringOfInstList in
           assert_raises (Failure "empty operands") err );
         (* Problem noted, since we first pattern match against num operands AND
            name of op, we will always default to the fail message regarding num
            of operands instead of fail message regarding unsupported
            instruction*)
         ( "Test if we fail gracefully if we are given a instruction that does \
            not exist"
         >:: fun _ ->
           let stringOfInstList = "bum x1, x2, x4" in
           let err = fun () -> convert_str_to_instr stringOfInstList in
           assert_raises (Failure "unsupported instruction") err );
         ( "Test if we fail gracefully if we are given too many operands"
         >:: fun _ ->
           let stringOfInstList = "add, x3, x1, x4, x5" in
           let err = fun () -> convert_str_to_instr stringOfInstList in
           assert_raises
             (Failure "Wrong number of operands for instruction: add") err );
         ( "Converting a type string list into a type Instruction list"
         >:: fun _ ->
           let stringOfInstList =
             [ "addi x1, x1, 10"; "mv x1, x2"; "sub x3, x1, x2" ]
           in
           let insts = make_instructions stringOfInstList in
           let oughInsts : instruction list =
             [
               {
                 name = Addi;
                 op1 = Register 1;
                 op2 = Register 1;
                 op3 = Value 10;
               };
               { name = Mv; op1 = Register 1; op2 = Register 2; op3 = None };
               {
                 name = Sub;
                 op1 = Register 3;
                 op2 = Register 1;
                 op3 = Register 2;
               };
             ]
           in
           assert_equal insts oughInsts );
         ( "Decoding an Instruction list" >:: fun _ ->
           let stringOfInstList =
             [ "addi x1, x1, 10"; "mv x1, x2"; "sub x3, x1, x2" ]
           in
           let insts = make_instructions stringOfInstList in
           let decodes = List.map (fun inst -> decode inst) insts in
           let corr_decodes : decoded list =
             [
               {
                 dst = Some 1;
                 src1 = Some 1;
                 src2 = None;
                 imm = Some 10;
                 alu = ADD_OP;
                 memory = PASS_OP;
                 branch = None;
               };
               {
                 dst = Some 1;
                 src1 = Some 2;
                 src2 = None;
                 imm = None;
                 alu = PASS_OP;
                 memory = PASS_OP;
                 branch = None;
               };
               {
                 dst = Some 3;
                 src1 = Some 1;
                 src2 = Some 2;
                 imm = None;
                 alu = SUB_OP;
                 memory = PASS_OP;
                 branch = None;
               };
             ]
           in
           assert_equal decodes corr_decodes );
         ( "Creating a CPU state" >:: fun _ ->
           let stringOfInst = "add x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 1;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           if Array.length state.instrs = state.pc then
             assert_equal state.regs registers
           else failwith "CPU state creation failure" );
         ( "Test if we can parse a register name to an index for \n\
           \            an array"
         >:: fun _ ->
           let register = "x1" in
           let index = parse_register register in
           assert_equal index 1 );
         ( "Test if we fail gracefully upon finding a non-defined\n\
           \            register name"
         >:: fun _ ->
           let register = "h1" in
           let err = fun () -> parse_register register in
           assert_raises (Failure "invalid register: h1") err );
         ( "Test if we fail gracefully upon finding a short register name"
         >:: fun _ ->
           let register = "1" in
           let err = fun () -> parse_register register in
           assert_raises (Failure "invalid register: 1") err );
         ( "Test if we fail gracefully upon finding a register name without an \
            index"
         >:: fun _ ->
           let register = "xx" in
           let err = fun () -> parse_register register in
           assert_raises (Failure "invalid register index: xx") err );
         ( "Test if we fail gracefully if register index is out of bounds: above"
         >:: fun _ ->
           let register = "x33" in
           let err = fun () -> parse_register register in
           assert_raises (Failure "register out of range: x33") err );
         ( "Test if we fail gracefully if register index is out of bounds: below"
         >:: fun _ ->
           let register = "x-1" in
           let err = fun () -> parse_register register in
           assert_raises (Failure "register out of range: x-1") err );
         (*Test if PC increase by 4 for every basic operation 
          *)
         (*Test that values are stored in the correct registers
          *)
         (*Instructions/Operations Tests*)
         (*Test each instruction individually This also tests exec_instruction,
           exec_decoded, alu_execute *)
         ( "Add Test" >:: fun _ ->
           let stringOfInst = "add x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- 2;
           registers.(3) <- 3;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 5 state.regs.(1) );
         ( "Sub Test" >:: fun _ ->
           let stringOfInst = "sub x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- 3;
           registers.(3) <- 2;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 1 state.regs.(1) );
         ( "Srl Test" >:: fun _ ->
           let stringOfInst = "srl x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- -1;
           registers.(3) <- 32;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 0 state.regs.(1) );
         ( "Sll Test" >:: fun _ ->
           let stringOfInst = "sll x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- 1;
           registers.(3) <- 1;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 2 state.regs.(1) );
         ( "Sra Test" >:: fun _ ->
           let stringOfInst = "sra x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- -1;
           registers.(3) <- 1;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal (-1) state.regs.(1) );
         ( "Srli Test" >:: fun _ ->
           let stringOfInst = "srli x1, x2, 32" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- -1;
           registers.(3) <- 32;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 0 state.regs.(1) );
         ( "Slli Test" >:: fun _ ->
           let stringOfInst = "slli x1, x2, 1" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- 1;
           registers.(3) <- 1;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 2 state.regs.(1) );
         ( "Srai Test" >:: fun _ ->
           let stringOfInst = "srai x1, x2, 1" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- -1;
           registers.(3) <- 1;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal (-1) state.regs.(1) );
         ( "Addi Test" >:: fun _ ->
           let stringOfInst = "addi x1, x1, 1" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 1 state.regs.(1) );
         ( "Mv Test" >:: fun _ ->
           let stringOfInst = "mv x1, x2" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(1) <- 5;
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 0 state.regs.(1) );
         (* test load and store instructions, specifically whether or not there
            form (ld x1, 0(x2)) messes with *)
         ( "Load and Store Test" >:: fun _ ->
           let stringOfInsts =
             [ "addi x1, x1, 10"; "sd x1, 4(x2)"; "ld x3, 4(x2)" ]
           in
           let insts = make_instructions stringOfInsts in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list stringOfInsts;
             }
           in
           let () = run state in
           assert_equal 10 state.regs.(3) );
         ( "Load and Store Test, byte 1: if val >= 256, only lower 8 bits stored"
         >:: fun _ ->
           let stringOfInsts =
             [ "addi x1, x1, 256"; "sb x1, 4(x2)"; "lb x3, 4(x2)" ]
           in
           let insts = make_instructions stringOfInsts in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list stringOfInsts;
             }
           in
           let () = run state in
           assert_equal 0 state.regs.(3) );
         ( "Load and Store Test, byte 2: if val < 256, val stored" >:: fun _ ->
           let stringOfInsts =
             [ "addi x1, x1, 255"; "sb x1, 4(x2)"; "lb x3, 4(x2)" ]
           in
           let insts = make_instructions stringOfInsts in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list stringOfInsts;
             }
           in
           let () = run state in
           assert_equal 255 state.regs.(3) );
         ( "Load and Store Test, word to double word" >:: fun _ ->
           let stringOfInsts =
             [ "addi x1, x1, 0x7fffffff"; "sw x1, 4(x2)"; "ld x3, 4(x2)" ]
           in
           let insts = make_instructions stringOfInsts in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list stringOfInsts;
             }
           in
           let () = run state in
           assert_equal 0x7fffffff state.regs.(3) );
         ( "Load and Store Test, double word to  word" >:: fun _ ->
           let stringOfInsts =
             [ "addi x1, x1, 0xffffffff"; "sd x1, 4(x2)"; "lw x3, 4(x2)" ]
           in
           let insts = make_instructions stringOfInsts in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list stringOfInsts;
             }
           in
           let () = run state in
           assert_equal (-1) state.regs.(3) );
         ( "Step test" >:: fun _ ->
           let inststrings =
             [ "addi x1, x1, 2"; "addi x2, x2, 3"; "sub x3, x2, x1" ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = step state in
           assert_equal (state.regs.(1) = 2 && state.regs.(2) = 0) true );
         ( "And Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "and x5,x1,x2" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 0 );
         ( "Or Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "or x5,x1,x2" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 7 );
         ( "Xor Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "xor x5,x1,x4" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 1 );
         ( "Andi Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "andi x5,x1,4" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 0 );
         ( "Ori Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "ori x5,x1,4" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 7 );
         ( "Xori Test" >:: fun _ ->
           let regist = Array.make 32 0 in
           regist.(1) <- 3;
           regist.(2) <- 4;
           regist.(3) <- 1;
           regist.(4) <- 2;
           let inststring = "xori x5,x1,2" in
           let inst = convert_str_to_instr inststring in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = regist;
               instrs = [| inst |];
               instr_strings = [| inststring |];
             }
           in
           let () = run state in
           assert_equal state.regs.(5) 1 );
         ( "Beq Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 2";
               "addi x2, x2, 3";
               "beq x1, x2, 2";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) = state.regs.(4)) true );
         ( "Bne Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 2";
               "addi x2, x2, 3";
               "bne x1, x2, 2";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) <> state.regs.(4)) true );
         ( "Blt Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 2";
               "addi x2, x2, 3";
               "blt x1, x2, 2";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) <> state.regs.(4)) true );
         ( "Bge Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 2";
               "addi x2, x2, 3";
               "bge x1, x2, 2";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) = state.regs.(4)) true );
         ( "Jal Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 2";
               "addi x2, x2, 3";
               "jal x5, 2";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) <> state.regs.(4)) true );
         (* Funny behavior, since Jalr has the same syntax as loads and saves we
            can't use parse register. Luckily though we can just use parse
            register for mem since it literally uses the exact same form *)
         ( "Jalr Test" >:: fun _ ->
           let inststrings =
             [
               "addi x1, x1, 1";
               "jalr x5, 3(x1)";
               "addi x2, x2, 3";
               "add x3, x1, x2";
               "add x4, x1, x2";
             ]
           in
           let insts = make_instructions inststrings in
           let registers = Array.make 32 0 in
           let (state : cpu_state) =
             {
               pc = 0;
               regs = registers;
               instrs = Array.of_list insts;
               instr_strings = Array.of_list inststrings;
             }
           in
           let () = run state in
           assert_equal (state.regs.(3) <> state.regs.(4)) true );
       ]

let _ = run_test_tt_main tests
