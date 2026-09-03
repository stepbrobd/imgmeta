type endian =
  | LE
  | BE

let tag_width = 0x0100
let tag_height = 0x0101
let tag_bits = 0x0102
let tag_orientation = 0x0112
let type_short = 3

let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:8 in
    let endian =
      match Bytes.get_uint8 head 0, Bytes.get_uint8 head 1 with
      | 0x49, 0x49 -> Some LE
      | 0x4d, 0x4d -> Some BE
      | _ -> None
    in
    match endian with
    | None -> Error (Types.Malformed "not a tiff byte order mark")
    | Some e ->
      let u16 b o = if e = LE then Bytes.get_uint16_le b o else Bytes.get_uint16_be b o in
      let u32 b o =
        Int32.to_int (if e = LE then Bytes.get_int32_le b o else Bytes.get_int32_be b o)
      in
      if u16 head 2 <> 42
      then Error (Types.Malformed "not a tiff magic")
      else (
        let ifd0 = u32 head 4 in
        if ifd0 < 8
        then Error (Types.Malformed "bad ifd0 offset")
        else (
          let count = u16 (Reader.read_at r ~pos:ifd0 ~len:2) 0 in
          let entries = Reader.read_at r ~pos:(ifd0 + 2) ~len:(count * 12) in
          let width = ref 0 in
          let height = ref 0 in
          let depth = ref 8 in
          let orientation = ref 1 in
          let scalar off =
            if u16 entries (off + 2) = type_short
            then u16 entries (off + 8)
            else u32 entries (off + 8)
          in
          (* one value per sample. up to two fit the field, a longer array lives
             elsewhere in the file and the field holds its offset *)
          let first_sample off =
            if u32 entries (off + 4) <= 2
            then u16 entries (off + 8)
            else u16 (Reader.read_at r ~pos:(u32 entries (off + 8)) ~len:2) 0
          in
          for i = 0 to count - 1 do
            let off = i * 12 in
            let tag = u16 entries off in
            if tag = tag_width
            then width := scalar off
            else if tag = tag_height
            then height := scalar off
            else if tag = tag_bits
            then depth := first_sample off
            else if tag = tag_orientation
            then (
              let v = u16 entries (off + 8) in
              if v >= 1 && v <= 8 then orientation := v)
          done;
          if !width <= 0 || !height <= 0
          then Error (Types.Malformed "missing tiff dimensions")
          else (
            let w, h =
              if !orientation >= 5 && !orientation <= 8
              then !height, !width
              else !width, !height
            in
            Ok
              { Types.format = TIFF
              ; width = w
              ; height = h
              ; depth = !depth
              ; orientation = !orientation
              })))
  with
  | Types.Imgmeta_error e -> Error e
;;
