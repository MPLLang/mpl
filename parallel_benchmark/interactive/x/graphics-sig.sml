(* High-level graphics library *)

(* Stefan Muller *)

signature GRAPHICS =
sig

    exception NoWindowOpen

    type color
    type button = MLX.button
    type event = MLX.event
    type font = MLX.font
    type modifier = MLX.modifier

    val openwindow : string option -> int * int -> unit
    val setforeground : color -> unit
    val flush : unit -> unit
    val closewindow : unit -> unit
    val clear : unit -> unit

    (* for example,
       selectinput d w (Mask.make [Mask.PropertyChange, Mask.Exposure])
       *)
    val selectinput : MLX.Mask.mask -> unit

    (* Blocks until an event is available, and returns it. *)
    val nextevent : unit -> event

    (* Blocks until an event matching the mask is available. *)
    val maskevent : MLX.Mask.mask -> event

    (* Finds the first event matching the mask, if one exists. Returns immediately. *)
    val checkmaskevent : MLX.Mask.mask -> event option

    (* Polling *)
    val mousepos : unit -> int * int
    val buttons : unit -> button list
    val button : unit -> bool
    val modstate : unit -> modifier list
    val readkey : unit -> char
    val keyoption : unit -> char option

    (* drawtext font x y text *)
    val drawtext : font option -> int -> int -> string -> unit

    (* drawpoint x y *)
    val drawpoint : int -> int -> unit

    (* {draw, fill}rectangle x y w h *)
    val fillrectangle : int -> int -> int -> int -> unit
    val drawrectangle : int -> int -> int -> int -> unit

    (* drawline x1 y1 x2 y2 *)
    val drawline : int -> int -> int -> int -> unit
end
