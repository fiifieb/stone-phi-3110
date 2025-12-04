val cpu_ref : Logic.cpu_state option ref
(** [cpu_ref] is a reference to the current CPU state, or None if no program is
    loaded. *)

val init_gui : unit -> Bogue.Main.board * Bogue.Widget.t array * Bogue.Widget.t
(** [init_gui ()] initializes the GUI and returns a triple:
    - the main Bogue board,
    - the array of register labels,
    - the PC label widget. *)
