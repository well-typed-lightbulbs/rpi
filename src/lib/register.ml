module Make(B: sig val addr : Mem.addr end) = struct
  
  type 'a field = {
    offset: int;
    size: int;
    to_int: 'a -> int;
    of_int: int -> 'a;
  }

  let read {offset; size; of_int; _} =
    of_int ((Mem.get_int B.addr lsr offset) land (((1 lsl size) - 1)))

  let empty = 0

  let set {offset; size; to_int; _} value current = 
    let mask = (1 lsl size) - 1 
    in
    (current land (lnot (mask lsl offset))) lor (to_int value lsl offset)

  let write = Mem.set_int B.addr
  
  
  let bool ~offset = {
    offset;
    size = 1;
    to_int=(fun x -> if x then 1 else 0);
    of_int=(fun x -> x == 1);
  }

end