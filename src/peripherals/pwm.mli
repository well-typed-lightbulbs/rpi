type mode = Serial | Analog

type pin_nr = Pin12 | Pin13 | Pin18 | Pin19 | Pin40 | Pin41 | Pin45

module Make (Setting : sig
    val mode : mode
    val pins : pin_nr list
    val freq : int
    val range : int
    val is_stereo : bool
end) : sig
    val init : unit -> unit Lwt.t

    val write : int -> unit Lwt.t

    val stop : unit -> unit Lwt.t

    val flush : unit -> unit Lwt.t
end
