module Events = struct
  module Advertising_report = struct
    let id = 0x02

    type event_type =
      | Adv_ind
      | Adv_direct_ind
      | Adv_scan_ind
      | Adv_nonconn_ind
      | Scan_response

    let event_type_to_int = function
      | Adv_ind -> 0
      | Adv_direct_ind -> 1
      | Adv_scan_ind -> 2
      | Adv_nonconn_ind -> 3
      | Scan_response -> 4

    let event_type_of_int = function
      | 0 -> Adv_ind
      | 1 -> Adv_direct_ind
      | 2 -> Adv_scan_ind
      | 3 -> Adv_nonconn_ind
      | 4 -> Scan_response
      | _ -> assert false

    type address_type =
      | Public_device_address
      | Random_device_address
      | Public_identity_address
      | Random_identity_address

    let address_type_of_int = function
      | 0 -> Public_device_address
      | 1 -> Random_device_address
      | 2 -> Public_identity_address
      | 3 -> Random_identity_address
      | _ -> assert false

    type t = {
      event_type : event_type;
      address_type : address_type;
      address : string;
      data : Hci.Advert_data.ad list;
      rssi : int;
    }

    let parse buffer =
      let field size fn (offset, list) =
        List.fold_left_map
          (fun offset v ->
            let size = size v in
            let buffer = Cstruct.sub buffer offset size in
            (offset + size, (v, fn buffer)))
          offset list
      in

      let num_reports = Cstruct.get_byte buffer 0 in
      (1, List.init num_reports (fun _ -> ()))
      (* 1byte: event type *)
      |> field
           (fun _ -> 1)
           (fun buf -> Cstruct.get_byte buf 0 |> event_type_of_int)
      (* 1byte: address type *)
      |> field
           (fun _ -> 1)
           (fun buf -> Cstruct.get_byte buf 0 |> address_type_of_int)
      (* 6 bytes: address *)
      |> field (fun _ -> 6) Cstruct.to_string
      (* 1byte: data length *)
      |> field (fun _ -> 1) (fun buf -> Cstruct.get_byte buf 0)
      (* [data length] bytes: data *)
      |> field
           (fun (_, data_length) -> data_length)
           (fun buf -> Hci.Advert_data.decode buf)
      (* 1 byte: rssi *)
      |> fun (offset, list) ->
      List.fold_left_map
        (fun offset v -> (offset + 1, (v, Cstruct.get_byte buffer offset)))
        offset list
      |> snd
      |> List.map
           (fun (((((((), event_type), address_type), address), _), data), rssi)
           -> { event_type; address_type; address; data; rssi })
  end

  module Connection_complete = struct
    let id = 0x01

    type t = {
      status : int;
      connection_handle : int;
      role : int;
      peer_address_type : int;
      peer_address : string;
      connection_interval : int;
      connection_latency : int;
      supervision_timeout : int;
      master_clock_accuracy : int;
    }

    let parse buf =
      let open Cstruct in
      {
        status = get_uint8 buf 0;
        connection_handle = LE.get_uint16 buf 1;
        role = get_uint8 buf 3;
        peer_address_type = get_uint8 buf 4;
        peer_address = Cstruct.to_string ~off:5 ~len:6 buf;
        connection_interval = LE.get_uint16 buf 11;
        connection_latency = LE.get_uint16 buf 13;
        supervision_timeout = LE.get_uint16 buf 15;
        master_clock_accuracy = get_uint8 buf 17;
      }
  end
end