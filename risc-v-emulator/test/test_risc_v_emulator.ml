open OUnit2
open Risc_v_emulator.Logic

let tests =
  "test suite"
  >::: [
         ("a trivial test" >:: fun _ -> assert_equal 0 0);
         (*Basic Behavior Tests*)
         ( "Initialization of Register Array" >:: fun _ ->
           assert_equal (Array.make 32 0) registers );
         ( "Converting a type string to a type Instruction" >:: fun _ ->
           let stringOfInst = "add x1 x2 x3" in
           let inst = convert_str_to_instr stringOfInst in
           let oughtInst : instruction =
             {
               name = Add;
               op1 = Register "x1";
               op2 = Register "x2";
               op3 = Register "x3";
             }
           in
           assert_equal inst oughtInst );
         ( "Converting a type string list into a type Instruction list"
         >:: fun _ ->
           let stringOfInstList =
             [ "addi x1, x1, 10"; "mv x1 x2"; "sub x3 x1 x2" ]
           in
           let insts = make_instructions stringOfInstList in
           let oughInsts : instruction list =
             [
               {
                 name = Addi;
                 op1 = Register "x1";
                 op2 = Register "x1";
                 op3 = Value 10;
               };
               {
                 name = Mv;
                 op1 = Register "x1";
                 op2 = Register "x2";
                 op3 = None;
               };
               {
                 name = Add;
                 op1 = Register "x3";
                 op2 = Register "x1";
                 op3 = Register "x2";
               };
             ]
           in
           assert_equal insts oughInsts );
         (*Instructions/Operations Tests*)

         (*I/O Tests*)
       ]

let _ = run_test_tt_main tests
