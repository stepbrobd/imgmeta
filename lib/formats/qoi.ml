let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:14 in
    if not (String.equal (Bytes.sub_string head 0 4) "qoif")
    then Error (Types.Malformed "not a qoi signature")
    else (
      let width = Int32.to_int (Bytes.get_int32_be head 4) in
      let height = Int32.to_int (Bytes.get_int32_be head 8) in
      if width <= 0 || height <= 0
      then Error (Types.Malformed "bad qoi dimensions")
      else
        (* qoi stores eight bits per channel and carries no orientation *)
        Ok { Types.format = QOI; width; height; depth = 8; orientation = 1 })
  with
  | Types.Imgmeta_error e -> Error e
;;
