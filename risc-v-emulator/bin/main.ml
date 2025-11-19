open Risc_v_emulator.Logic

(*Prints the contents of [registers] in the format "x[index] = [value]"*)
let print_registers () =
  print_endline "\nRegister contents:";
  Array.iteri (fun i v -> Printf.printf "x%-2d = %d\n" i v) registers

(*Reads contents from [file] and returns a list of strings*)
let read_lines file =
  try
    let channel = open_in file in
    let rec loop acc =
      match input_line channel with
      | line -> loop (line :: acc)
      | exception End_of_file ->
          close_in channel;
          List.rev acc
    in
    loop []
  with Sys_error msg ->
    print_endline ("Error opening file: " ^ file ^ "\n" ^ msg);
    exit 1

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: dune exec risc-v-emulator -- <file.s>";
    exit 1);

  let filename = Sys.argv.(1) in
  let instructions = read_lines filename in
  List.iteri (fun i s -> Printf.printf "%d: %s\n" (i + 1) s) instructions;
  (*[instructions] is sent to backend and [registers] is updated*)
  print_registers ()
