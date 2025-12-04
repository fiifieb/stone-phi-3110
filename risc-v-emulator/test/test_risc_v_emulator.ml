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
                 branch = None;
               };
               {
                 dst = Some 1;
                 src1 = Some 2;
                 src2 = None;
                 imm = None;
                 alu = PASS_OP;
                 branch = None;
               };
               {
                 dst = Some 3;
                 src1 = Some 1;
                 src2 = Some 2;
                 imm = None;
                 alu = SUB_OP;
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
               pc = 1;
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
               pc = 1;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 1 state.regs.(1) );
         (*Something is off with this test, either srl is messed or this test is
           and tbh I cannot tell Okay I am fairly certain srl is messed*)
         ( "Srl Test" >:: fun _ ->
           let stringOfInst = "srl x1, x2, x3" in
           let inst = convert_str_to_instr stringOfInst in
           let registers = Array.make 32 0 in
           registers.(2) <- -1;
           registers.(3) <- 32;
           let (state : cpu_state) =
             {
               pc = 1;
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
               pc = 1;
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
               pc = 1;
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
               pc = 1;
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
               pc = 1;
               regs = registers;
               instrs = [| inst |];
               instr_strings = [| stringOfInst |];
             }
           in
           let () = run state in
           assert_equal 0 state.regs.(1) );
       ]

let _ = run_test_tt_main tests
