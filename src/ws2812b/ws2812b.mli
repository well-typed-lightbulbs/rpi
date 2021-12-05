type color = { r : int; g : int; b : int }

type frame

(* Encode a light sequence into a prepared frame *)
val encode : color list -> frame

(* Synchronously write the frame to the LEDs *)
val output : frame -> unit
