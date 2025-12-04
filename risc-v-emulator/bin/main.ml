open Risc_v_emulator.Logic

(*Prints the contents of [registers] in the format "x[index] = [value]"*)
let print_registers regs =
  print_endline "\nRegister contents:";
  Array.iteri (fun i v -> Printf.printf "x%-2d = %d\n" i v) regs

(*Parses a line starting with "IRV" and returns a list of initial
  (register,value) pairs*)
let parse_irv line : (int * int) list =
  let line = String.trim line in
  let prefix = "IRV:" in
  if
    String.length line < String.length prefix
    || String.sub line 0 (String.length prefix) <> prefix
  then failwith "IRV line missing IRV prefix";
  let line' =
    String.sub line (String.length prefix)
      (String.length line - String.length prefix)
  in
  let regs =
    line' |> String.trim |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  List.map
    (fun reg ->
      match String.split_on_char '@' reg with
      | [ reg; value ] ->
          let r = int_of_string reg in
          let v = int_of_string value in
          (r, v)
      | _ -> failwith ("Invalid IRV entry: " ^ reg))
    regs

(*Returns a [cpu_state] where [cpu_state.pc] is the number of instructions,
  [cpu_state.regs] = the array of registers including initial values specified
  in [lines], and [cpu_state.instrs] is the array of instructions created from
  [lines]*)
let cpu_init (lines : string list) : cpu_state =
  match lines with
  | [] -> failwith "empty input file"
  | irv :: instructions ->
      let registers = Array.make 32 0 in
      let initial_vals = parse_irv irv in
      List.iter
        (fun (r, v) ->
          if r < 0 || r > 31 then failwith "Register out of range"
          else registers.(r) <- v)
        initial_vals;
      let instruction_list = make_instructions instructions in
      let instruction_array = Array.of_list instruction_list in
      {
        pc = Array.length instruction_array;
        regs = registers;
        instrs = instruction_array;
      }

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
  let usage =
    "Usage: dune exec -- risc-v-emulator -- [--ui terminal|graphical] <file.s>\n\
    \       or: dune exec -- risc-v-emulator -- -t <file.s>\n\
    \       or: dune exec -- risc-v-emulator -- -g <file.s>\n\
    \       or: dune exec -- risc-v-emulator -- <file.s>"
  in

  if Array.length Sys.argv < 2 then (
    prerr_endline usage;
    exit 1);

  let ui_mode, filename =
    match Sys.argv.(1) with
    | "--ui" ->
        if Array.length Sys.argv < 4 then (
          prerr_endline "Not enough arguments for --ui";
          prerr_endline usage;
          exit 1)
        else (Sys.argv.(2), Sys.argv.(3))
    | "-t" ->
        if Array.length Sys.argv < 3 then (
          prerr_endline "Missing filename for -t";
          prerr_endline usage;
          exit 1)
        else ("terminal", Sys.argv.(2))
    | "-g" ->
        if Array.length Sys.argv < 3 then (
          prerr_endline "Missing filename for -g";
          prerr_endline usage;
          exit 1)
        else ("graphical", Sys.argv.(2))
    | fname -> ("terminal", fname)
  in

  let raw_lines = read_lines filename in
  let lines =
    raw_lines |> List.map String.trim
    |> List.filter (fun line ->
        line <> "" && not (String.starts_with ~prefix:"#" line))
  in
  let cpu = cpu_init lines in

  (match ui_mode with
  | "terminal" -> run cpu
  | "graphical" ->
      prerr_endline
        "Graphical UI selected but not yet implemented; falling back to \
         terminal UI.";
      run cpu
  | _ ->
      prerr_endline ("Unknown UI mode: " ^ ui_mode);
      exit 1);

  Printf.printf "Register Contents:\n";
  print_registers cpu.regs
