let read_metadata r =
  try
    let header = Reader.read_at r ~pos:0 ~len:25 in
    if not (String.equal (Bytes.sub_string header 0 8) "\x89PNG\r\n\x1a\n")
    then Error (Types.Malformed "not a png signature")
    else if not (String.equal (Bytes.sub_string header 12 4) "IHDR")
    then Error (Types.Malformed "missing ihdr chunk")
    else (
      let width = Int32.to_int (Bytes.get_int32_be header 16) in
      let height = Int32.to_int (Bytes.get_int32_be header 20) in
      let depth = Bytes.get_uint8 header 24 in
      Ok { Types.format = PNG; width; height; depth; orientation = 1 })
  with
  | Types.Imgmeta_error e -> Error e
;;
