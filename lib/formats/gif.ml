let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:13 in
    let sig6 = Bytes.sub_string head 0 6 in
    if not (String.equal sig6 "GIF87a" || String.equal sig6 "GIF89a")
    then Error (Types.Malformed "not a gif signature")
    else (
      let width = Bytes.get_uint16_le head 6 in
      let height = Bytes.get_uint16_le head 8 in
      let packed = Bytes.get_uint8 head 10 in
      let depth = ((packed lsr 4) land 0b111) + 1 in
      Ok { Types.format = GIF; width; height; depth; orientation = 1 })
  with
  | Types.Imgmeta_error e -> Error e
;;
