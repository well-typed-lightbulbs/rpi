type color = { r : int; g : int; b : int }

type frame

type t

val init : unit -> t Lwt.t

(* Encode a light sequence into a prepared frame *)
val encode : t -> color list -> frame

(* Synchronously write the frame to the LEDs *)
val output : frame -> unit Lwt.t
