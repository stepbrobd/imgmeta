let is_sof byte =
  match byte with
  | 0xc0
  | 0xc1
  | 0xc2
  | 0xc3
  | 0xc5
  | 0xc6
  | 0xc7
  | 0xc9
  | 0xca
  | 0xcb
  | 0xcd
  | 0xce
  | 0xcf -> true
  | _ -> false
;;

let read_metadata r =
  try
    let soi = Reader.read_at r ~pos:0 ~len:2 in
    if not (Bytes.get_uint8 soi 0 = 0xff && Bytes.get_uint8 soi 1 = 0xd8)
    then Error (Types.Malformed "not a jpeg soi")
    else (
      let cursor = ref 2 in
      let rec walk () =
        let m = Reader.read_at r ~pos:!cursor ~len:2 in
        cursor := !cursor + 2;
        if Bytes.get_uint8 m 0 <> 0xff
        then Error (Types.Malformed "expected marker prefix")
        else (
          let code = Bytes.get_uint8 m 1 in
          if code = 0xff
          then (
            cursor := !cursor - 1;
            walk ())
          else if is_sof code
          then (
            let body = Reader.read_at r ~pos:(!cursor + 2) ~len:5 in
            let depth = Bytes.get_uint8 body 0 in
            let height = Bytes.get_uint16_be body 1 in
            let width = Bytes.get_uint16_be body 3 in
            Ok { Types.format = JPEG; width; height; depth; orientation = 1 })
          else if code = 0xd9 || code = 0xda
          then Error (Types.Malformed "reached eoi or sos before sof")
          else (
            let len_bytes = Reader.read_at r ~pos:!cursor ~len:2 in
            let seg_len = Bytes.get_uint16_be len_bytes 0 in
            cursor := !cursor + seg_len;
            walk ()))
      in
      walk ())
  with
  | Types.Imgmeta_error e -> Error e
;;
