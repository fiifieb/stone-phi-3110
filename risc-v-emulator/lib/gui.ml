open Bogue
open Logic

let cpu_ref : cpu_state option ref = ref (None : cpu_state option)
let prev_pc : int ref = ref (-1)

let create_labels () =
  Array.init 32 (fun i ->
      let text = Printf.sprintf "x%-2d = %-10d" i 0 in
      Widget.label text)

let create_instr_labels () =
  match !cpu_ref with
  | None -> [||]
  | Some cpu ->
      Array.mapi
        (fun i instr_str ->
          let prefix = if i = cpu.pc then "-> " else "   " in
          let text = Printf.sprintf "%s%d: %s" prefix i instr_str in
          Widget.label text)
        cpu.instr_strings

let update_labels labels pc_label =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      Array.iteri
        (fun i label ->
          Widget.set_text label (Printf.sprintf "x%-2d = %-10d" i cpu.regs.(i)))
        labels;
      Widget.set_text pc_label (Printf.sprintf "PC = %d" cpu.pc)

let update_instr_colors instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      Array.iteri
        (fun i label ->
          let prefix =
            if i = cpu.pc then "-> " else if i = !prev_pc then " * " else "   "
          in
          let text = Printf.sprintf "%s%d: %s" prefix i cpu.instr_strings.(i) in
          Widget.set_text label text)
        instr_labels

let on_step labels pc_label instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      if cpu.pc < Array.length cpu.instrs then (
        prev_pc := cpu.pc;
        step cpu;
        update_labels labels pc_label;
        update_instr_colors instr_labels)

let on_run labels pc_label instr_labels =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      prev_pc := cpu.pc;
      run cpu;
      update_labels labels pc_label;
      update_instr_colors instr_labels

let init_gui () =
  let labels = create_labels () in
  let instr_labels = create_instr_labels () in

  let rows =
    Array.init 4 (fun r ->
        let start_index = r * 8 in
        let row_widgets = Array.sub labels start_index 8 |> Array.to_list in
        Layout.flat_of_w row_widgets)
  in
  let register_layout = Layout.tower (Array.to_list rows) in
  let pc_label = Widget.label ~size:15 "PC = 0" in

  let instr_layout =
    if Array.length instr_labels = 0 then
      Layout.resident (Widget.label "No instructions")
    else
      Layout.tower
        (instr_labels |> Array.to_list |> List.map (fun w -> Layout.resident w))
  in

  let step_button = Widget.button "Step" in
  let run_button = Widget.button "Run" in

  Widget.on_click
    ~click:(fun _ -> on_step labels pc_label instr_labels)
    step_button;
  Widget.on_click
    ~click:(fun _ -> on_run labels pc_label instr_labels)
    run_button;
  let buttons_layout = Layout.flat_of_w [ step_button; run_button ] in

  let right_panel =
    Layout.tower [ Layout.resident pc_label; register_layout; buttons_layout ]
  in
  let main_layout = Layout.flat [ instr_layout; right_panel ] in

  let gui = Bogue.make [] [ main_layout ] in
  (gui, labels, pc_label, instr_labels)
