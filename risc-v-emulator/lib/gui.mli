val cpu_ref : Logic.cpu_state option ref
(** [cpu_ref] is a reference to the current CPU state, or None if no program is
    loaded. *)

val update_labels : Bogue.Widget.t array -> Bogue.Widget.t -> unit
(** [update_labels labels pc_label] updates all register labels and the PC label
    to display the current values from [cpu_ref]. *)

val init_gui : unit -> Bogue.Main.board * Bogue.Widget.t array * Bogue.Widget.t
(** [init_gui ()] initializes the GUI and returns a triple:
    - the main Bogue board,
    - the array of register labels,
    - the PC label widget. *)
