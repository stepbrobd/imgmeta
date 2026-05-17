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
      let header = Reader.read_at r ~pos:!cursor ~len:8 in
      let ty = Bytes.sub_string header 0 4 in
      let len = Int32.to_int (Bytes.get_int32_le header 4) in
      let body_off = !cursor + 8 in
      let next = body_off + len + (len land 1) in
      if String.equal ty "EXIF"
      then (
        let body = Reader.read_at r ~pos:body_off ~len in
        result := Exif.parse_orientation body)
      else (
        cursor := next;
        walk ()))
  in
  walk ();
  !result
;;

let finalize r ~next_cursor ~width ~height =
  let orientation =
    try scan_exif_orientation r ~start:next_cursor with
    | Types.Imgmeta_error _ -> 1
  in
  let width, height =
    if orientation >= 5 && orientation <= 8 then height, width else width, height
  in
  Ok { Types.format = WebP; width; height; depth = 8; orientation }
;;

let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:12 in
    if
      (not (String.equal (Bytes.sub_string head 0 4) "RIFF"))
      || not (String.equal (Bytes.sub_string head 8 4) "WEBP")
    then Error (Types.Malformed "not a webp riff container")
    else (
      let cursor = ref 12 in
      let rec walk () =
        let header = Reader.read_at r ~pos:!cursor ~len:8 in
        let ty = Bytes.sub_string header 0 4 in
        let len = Int32.to_int (Bytes.get_int32_le header 4) in
        let body_off = !cursor + 8 in
        let next = body_off + len + (len land 1) in
        match ty with
        | "VP8X" ->
          let body = Reader.read_at r ~pos:body_off ~len:10 in
          let w =
            1
            + Bytes.get_uint8 body 4
            + (Bytes.get_uint8 body 5 lsl 8)
            + (Bytes.get_uint8 body 6 lsl 16)
          in
          let h =
            1
            + Bytes.get_uint8 body 7
            + (Bytes.get_uint8 body 8 lsl 8)
            + (Bytes.get_uint8 body 9 lsl 16)
          in
          finalize r ~next_cursor:next ~width:w ~height:h
        | "VP8L" ->
          let body = Reader.read_at r ~pos:body_off ~len:5 in
          if Bytes.get_uint8 body 0 <> 0x2f
          then Error (Types.Malformed "vp8l signature")
          else (
            let b1 = Bytes.get_uint8 body 1 in
            let b2 = Bytes.get_uint8 body 2 in
            let b3 = Bytes.get_uint8 body 3 in
            let b4 = Bytes.get_uint8 body 4 in
            let w = 1 + (b1 lor ((b2 land 0x3f) lsl 8)) in
            let h =
              1 + ((b2 lsr 6) land 0x3 lor (b3 lsl 2) lor ((b4 land 0x0f) lsl 10))
            in
            finalize r ~next_cursor:next ~width:w ~height:h)
        | "VP8 " ->
          let body = Reader.read_at r ~pos:body_off ~len:10 in
          if
            Bytes.get_uint8 body 3 <> 0x9d
            || Bytes.get_uint8 body 4 <> 0x01
            || Bytes.get_uint8 body 5 <> 0x2a
          then Error (Types.Malformed "vp8 keyframe signature")
          else (
            let w = Bytes.get_uint16_le body 6 land 0x3fff in
            let h = Bytes.get_uint16_le body 8 land 0x3fff in
            finalize r ~next_cursor:next ~width:w ~height:h)
        | _ ->
          cursor := next;
          walk ()
      in
      walk ())
  with
  | Types.Imgmeta_error e -> Error e
;;
