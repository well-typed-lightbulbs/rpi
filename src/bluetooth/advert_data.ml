type ad =
  (* 0x01 *)
  | Flags of {
      le_limited_discoverable_mode : bool;
      le_general_discoverable_mode : bool;
      br_edr_not_supported : bool;
      simultaneous_le_and_br_edr_to_the_same_device_capable_controller : bool;
      simultaneous_le_and_br_edr_to_the_same_device_capable_host : bool;
    }
  (* 0x03 *)
  | Complete_list_16bit_service_uuid of int list
  (* 0x09 *)
  | Complete_local_name of string
  (* 0x16 *)
  | Service_data_16bit of int * bytes

let sizeof_data = function
  | Flags _ -> 1
  | Complete_list_16bit_service_uuid lst -> 2 * List.length lst
  | Service_data_16bit (_, data) -> 2 + Bytes.length data
  | Complete_local_name data -> String.length data

let set_bit position value = Bool.to_int value lsl position

let write_ad_to frame ad =
  let sz = sizeof_data ad in
  Cstruct.set_uint8 frame 0 (1 + sz);
  match ad with
  | Flags
      {
        le_limited_discoverable_mode;
        le_general_discoverable_mode;
        br_edr_not_supported;
        simultaneous_le_and_br_edr_to_the_same_device_capable_controller;
        simultaneous_le_and_br_edr_to_the_same_device_capable_host;
      } ->
      let value =
        set_bit 0 le_limited_discoverable_mode
        lor set_bit 1 le_general_discoverable_mode
        lor set_bit 2 br_edr_not_supported
        lor set_bit 3
              simultaneous_le_and_br_edr_to_the_same_device_capable_controller
        lor set_bit 4 simultaneous_le_and_br_edr_to_the_same_device_capable_host
      in
      Cstruct.set_uint8 frame 1 0x01;
      Cstruct.set_uint8 frame 2 value
  | Complete_list_16bit_service_uuid list ->
      Cstruct.set_uint8 frame 1 0x03;
      List.iteri (fun i v -> Cstruct.LE.set_uint16 frame (2 + (2 * i)) v) list
  | Complete_local_name name ->
      Cstruct.set_uint8 frame 1 0x09;
      Cstruct.blit_from_string name 0 frame 2 (String.length name)
  | Service_data_16bit (service, data) ->
      Cstruct.set_uint8 frame 1 0x16;
      Cstruct.LE.set_uint16 frame 2 service;
      Cstruct.blit_from_bytes data 0 frame 3 (Bytes.length data)

(* According to 7.8.7  LE Set Advertising Data command in bluetooth core spec *)
let encode v =
  let total_size =
    List.fold_left (fun acc v -> acc + (2 + sizeof_data v)) 0 v
  in
  let frame = Cstruct.create 32 in
  if total_size > 31 then failwith "Advert data too big";
  Cstruct.set_uint8 frame 0 total_size;
  List.fold_left
    (fun position item ->
      let sz = sizeof_data item in
      write_ad_to (Cstruct.sub frame position (sz + 2)) item;
      sz + 2 + position)
    1 v
  |> ignore;
  frame

let decode_frame frame =
  let type' = Cstruct.get_uint8 frame 0 in
  match type' with
  | 1 ->
      let value = Cstruct.get_uint8 frame 1 in
      let le_limited_discoverable_mode = value land (1 lsl 0) <> 0 in
      let le_general_discoverable_mode = value land (1 lsl 1) <> 0 in
      let br_edr_not_supported = value land (1 lsl 2) <> 0 in
      let simultaneous_le_and_br_edr_to_the_same_device_capable_controller =
        value land (1 lsl 3) <> 0
      in
      let simultaneous_le_and_br_edr_to_the_same_device_capable_host =
        value land (1 lsl 4) <> 0
      in
      Some
        (Flags
           {
             le_limited_discoverable_mode;
             le_general_discoverable_mode;
             br_edr_not_supported;
             simultaneous_le_and_br_edr_to_the_same_device_capable_controller;
             simultaneous_le_and_br_edr_to_the_same_device_capable_host;
           })
  | 3 ->
      let n_services = (Cstruct.length frame - 1) / 2 in
      Some
        (Complete_list_16bit_service_uuid
           (List.init n_services (fun n ->
                Cstruct.LE.get_uint16 frame (1 + (2 * n)))))
  | 9 -> Some (Complete_local_name (Cstruct.to_string ~off:1 frame))
  | n ->
      Printf.printf "[WARNING] advert_data.ml: unknown frame type %d\n" n;
      None

(* decode a frame without header *)
let decode v =
  let rec parse aux position =
    if position >= Cstruct.length v then aux
    else
      let size = Cstruct.get_uint8 v position in
      let frame = decode_frame (Cstruct.sub v (position + 1) size) in
      parse (frame :: aux) (position + 1 + size)
  in
  parse [] 0 |> List.filter_map Fun.id
