(* the metadata record carries bits per channel. bmp counts bits per pixel,
   which only coincides for the packed truecolour depths *)
let depth_of_bit_count = function
  | 24 | 32 -> 8
  | 16 -> 5
  | n -> n
;;

let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:18 in
    if not (String.equal (Bytes.sub_string head 0 2) "BM")
    then Error (Types.Malformed "not a bmp signature")
    else (
      let dib_size = Int32.to_int (Bytes.get_int32_le head 14) in
      if dib_size = 12
      then (
        let core = Reader.read_at r ~pos:18 ~len:8 in
        let width = Bytes.get_uint16_le core 0 in
        let height = Bytes.get_uint16_le core 2 in
        let bits = Bytes.get_uint16_le core 6 in
        Ok
          { Types.format = BMP
          ; width
          ; height
          ; depth = depth_of_bit_count bits
          ; orientation = 1
          })
      else if dib_size >= 40
      then (
        let info = Reader.read_at r ~pos:18 ~len:14 in
        let width = Int32.to_int (Bytes.get_int32_le info 0) in
        (* a negative height marks a top-down bitmap, not a smaller image *)
        let height = Int32.to_int (Bytes.get_int32_le info 4) in
        let bits = Bytes.get_uint16_le info 10 in
        if width <= 0 || height = 0
        then Error (Types.Malformed "bad bmp dimensions")
        else
          Ok
            { Types.format = BMP
            ; width
            ; height = abs height
            ; depth = depth_of_bit_count bits
            ; orientation = 1
            })
      else Error (Types.Malformed "unsupported bmp dib header"))
  with
  | Types.Imgmeta_error e -> Error e
;;
