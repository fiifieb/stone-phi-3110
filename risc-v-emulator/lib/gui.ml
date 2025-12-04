open Bogue
open Logic

let cpu_ref : cpu_state option ref = ref (None : cpu_state option)

let create_labels () =
  Array.init 32 (fun i ->
      let text = Printf.sprintf "x%-2d = %-10d" i 0 in
      Widget.label text)

let update_labels labels pc_label =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      Array.iteri
        (fun i label ->
          Widget.set_text label (Printf.sprintf "x%-2d = %-10d" i cpu.regs.(i)))
        labels;
      Widget.set_text pc_label (Printf.sprintf "PC = %d" cpu.pc)

let on_step labels pc_label =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      if cpu.pc < Array.length cpu.instrs then (
        step cpu;
        update_labels labels pc_label)

let on_run labels pc_label =
  match !cpu_ref with
  | None -> ()
  | Some cpu ->
      run cpu;
      update_labels labels pc_label

let init_gui () =
  let labels = create_labels () in
  let rows =
    Array.init 4 (fun r ->
        let start_index = r * 8 in
        let row_widgets = Array.sub labels start_index 8 |> Array.to_list in
        Layout.flat_of_w row_widgets)
  in
  let label_layout = Layout.tower (Array.to_list rows) in
  let pc_label = Widget.label "PC = 0" in

  let step_button = Widget.button "Step" in
  let run_button = Widget.button "Run" in

  Widget.on_click ~click:(fun _ -> on_step labels pc_label) step_button;
  Widget.on_click ~click:(fun _ -> on_run labels pc_label) run_button;
  let buttons_layout = Layout.flat_of_w [ step_button; run_button ] in
  let main_layout =
    Layout.tower [ Layout.resident pc_label; label_layout; buttons_layout ]
  in
  let gui = Bogue.make [] [ main_layout ] in
  (gui, labels, pc_label)
