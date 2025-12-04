open Bogue
open Logic

let cpu_ref : cpu_state option ref = ref (None : cpu_state option)
let prev_pc : int ref = ref (-1)
let mem_offset : int ref = ref 0
let mem_labels : Widget.t array ref = ref [||]
let initial_regs : int array ref = ref [||]

(** Creates an array of labels for the 32 registers with their initial values.
*)
let create_labels () =
  Array.init 32 (fun i ->
      let text = Printf.sprintf "x%02d:%10d         " i 0 in
      Widget.label ~size:12 text)

(** Creates an array of labels for the instructions. *)
let create_instr_labels () =
  match !cpu_ref with
  | None -> [||]
  | Some cpu ->
      Array.mapi
        (fun i instr_str ->
          let prefix = if i = cpu.pc then "-> " else "   " in
          let text = Printf.sprintf "%s%03d: %-40s" prefix i instr_str in
          Widget.label text)
        cpu.instr_strings

(** Creates an array of labels for memory display with initial values of 0. *)
let create_mem_labels () =
  Array.init 32 (fun i ->
      let text = Printf.sprintf "[%06d]: %12d  " 0 0 in
      Widget.label ~size:12 text)

(** Updates memory labels based on current memory contents and offset.*)
let update_mem_labels () =
  let labels = !mem_labels in
  let offset = !mem_offset in
  Array.iteri
    (fun i label ->
      let addr = offset + (i * 4) in
      let value =
        match get_from_mem addr with
        | Some v -> v
        | None -> 0
      in
      let text = Printf.sprintf "[%06d]: %12d  " addr value in
      Widget.set_text label text)
    labels

(** Updates register labels and PC label with the current CPU state. *)
let update_labels labels pc_label =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      Array.iteri
        (fun i label ->
          let text = Printf.sprintf "x%02d:%10d         " i cpu.regs.(i) in
          Widget.set_text label text)
        labels;
      Widget.set_text pc_label (Printf.sprintf "PC: %08d" cpu.pc)

(** Updates instruction labels to indicate current and previous PC*)
let update_instr_colors instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      Array.iteri
        (fun i label ->
          let prefix =
            if i = cpu.pc then "-> " else if i = !prev_pc then " * " else "   "
          in
          let text =
            Printf.sprintf "%s%03d: %-40s" prefix i cpu.instr_strings.(i)
          in
          Widget.set_text label text)
        instr_labels

(** Executes a single CPU step and updates relevant labels. *)
let on_step labels pc_label instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      if cpu.pc < Array.length cpu.instrs then (
        prev_pc := cpu.pc;
        step cpu;
        update_labels labels pc_label;
        update_instr_colors instr_labels;
        update_mem_labels ())

(** Executes entire CPU and updates relevant labels. *)
let on_run labels pc_label instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      prev_pc := cpu.pc;
      run cpu;
      update_labels labels pc_label;
      update_instr_colors instr_labels;
      update_mem_labels ()

(** Resets CPU state, registers, memory, and updates all GUI labels. *)
let on_reset labels pc_label instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      (* Reset PC to 0 *)
      cpu.pc <- 0;
      prev_pc := -1;
      (* Restore initial register values *)
      Array.blit !initial_regs 0 cpu.regs 0 32;
      (* Clear memory *)
      Hashtbl.clear main_mem;
      (* Reset memory offset *)
      mem_offset := 0;
      (* Update displays *)
      update_labels labels pc_label;
      update_instr_colors instr_labels;
      update_mem_labels ()
(** Initializes the GUI and returns a tuple:
    - the main Bogue board,
    - the array of register labels,
    - the PC label widget,
    - the array of instruction labels. *)
let init_gui () =
  let labels = create_labels () in
  let instr_labels = create_instr_labels () in

  (* Initialize memory labels *)
  mem_labels := create_mem_labels ();

  let rows =
    Array.init 4 (fun r ->
        let start_index = r * 8 in
        let row_widgets = Array.sub labels start_index 8 |> Array.to_list in
        Layout.flat_of_w row_widgets)
  in
  let register_layout = Layout.tower (Array.to_list rows) in
  let pc_label = Widget.label ~size:15 "PC: 00000000 " in

  let instr_layout =
    if Array.length instr_labels = 0 then
      Layout.resident (Widget.label "No instructions")
    else
      let instr_widgets =
        instr_labels |> Array.to_list |> List.map (fun w -> Layout.resident w)
      in
      Layout.make_clip ~h:400 (Layout.tower instr_widgets)
  in

  let step_button = Widget.button "Step" in
  let run_button = Widget.button "Run" in
  let reset_button = Widget.button "Reset" in

  Widget.on_click
    ~click:(fun _ -> on_step labels pc_label instr_labels)
    step_button;
  Widget.on_click
    ~click:(fun _ -> on_run labels pc_label instr_labels)
    run_button;
  Widget.on_click
    ~click:(fun _ -> on_reset labels pc_label instr_labels)
    reset_button;
  let buttons_layout =
    Layout.flat_of_w [ reset_button; step_button; run_button ]
  in

  (* Memory view section *)
  let mem_title = Widget.label ~size:15 "Memory View " in
  let mem_offset_label = Widget.label "Memory Offset:  " in
  let mem_offset_input = Widget.text_input ~size:15 ~text:"0" () in
  let mem_update_button = Widget.button "Update" in

  (* Update memory offset when button clicked *)
  Widget.on_click mem_update_button ~click:(fun _ ->
      try
        let text = Widget.get_text mem_offset_input in
        let new_offset = int_of_string (String.trim text) in
        mem_offset := new_offset;
        update_mem_labels ()
      with
      | Failure msg ->
          Printf.printf "Failed to parse offset: %s\n" msg;
          flush stdout
      | e ->
          Printf.printf "Error updating memory: %s\n" (Printexc.to_string e);
          flush stdout);

  let mem_offset_row =
    Layout.flat_of_w [ mem_offset_label; mem_offset_input; mem_update_button ]
  in

  (* Create memory display grid (4 rows x 8 columns = 32 memory locations) *)
  let mem_rows =
    Array.init 4 (fun r ->
        let start_index = r * 8 in
        let row_widgets =
          Array.sub !mem_labels start_index 8 |> Array.to_list
        in
        Layout.flat_of_w row_widgets)
  in
  let mem_grid = Layout.tower (Array.to_list mem_rows) in

  let mem_view_layout =
    Layout.tower [ Layout.resident mem_title; mem_offset_row; mem_grid ]
  in

  let right_panel =
    Layout.tower [ Layout.resident pc_label; register_layout; buttons_layout ]
  in
  let top_panel = Layout.flat [ instr_layout; right_panel ] in
  let main_layout = Layout.tower [ top_panel; mem_view_layout ] in

  let gui = Bogue.make [] [ main_layout ] in
  (gui, labels, pc_label, instr_labels)
