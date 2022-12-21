type color = { r : int; g : int; b : int }
type frame
type t

val init : unit -> t

(* Encode a light sequence into a prepared frame *)
val encode : t -> color list -> frame

(* Synchronously write the frame to the LEDs *)
val output : frame -> unit

(* Cstructs *)
val encoded_size : t -> int -> int
val write_cstruct : t -> Cstruct.t -> color list -> unit
