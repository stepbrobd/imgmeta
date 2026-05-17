let scan_exif_orientation r ~start =
  let size = Reader.size r in
  let limit =
    match size with
    | Some n -> n
    | None -> max_int
  in
  let cursor = ref start in
  let result = ref 1 in
  let rec walk () =
    if !cursor + 8 > limit
    then ()
    else (
      let head = Reader.read_at r ~pos:!cursor ~len:8 in
      let len = Int32.to_int (Bytes.get_int32_be head 0) in
      let ty = Bytes.sub_string head 4 4 in
      let body_off = !cursor + 8 in
      if String.equal ty "IEND"
      then ()
      else if String.equal ty "eXIf"
      then (
        let body = Reader.read_at r ~pos:body_off ~len in
        result := Exif.parse_orientation body)
      else (
        cursor := body_off + len + 4;
        walk ()))
  in
  walk ();
  !result
;;

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
      let ihdr_len = Int32.to_int (Bytes.get_int32_be header 8) in
      let orientation =
        try scan_exif_orientation r ~start:(8 + 8 + ihdr_len + 4) with
        | Types.Imgmeta_error _ -> 1
      in
      let width, height =
        if orientation >= 5 && orientation <= 8 then height, width else width, height
      in
      Ok { Types.format = PNG; width; height; depth; orientation })
  with
  | Types.Imgmeta_error e -> Error e
;;
