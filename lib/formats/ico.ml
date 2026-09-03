let depth_of_bit_count = function
  | 0 -> 8
  | 24 | 32 -> 8
  | 16 -> 5
  | n -> n
;;

(* a directory entry stores each dimension in one byte, where zero means 256 *)
let dimension v = if v = 0 then 256 else v

(* an icon file holds several sizes of the same image. the largest is the one a
   caller asking for dimensions means *)
let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:6 in
    if Bytes.get_uint16_le head 0 <> 0 || Bytes.get_uint16_le head 2 <> 1
    then Error (Types.Malformed "not an ico directory")
    else (
      let count = Bytes.get_uint16_le head 4 in
      if count < 1
      then Error (Types.Malformed "empty ico directory")
      else (
        let entries = Reader.read_at r ~pos:6 ~len:(count * 16) in
        let best = ref None in
        for i = 0 to count - 1 do
          let off = i * 16 in
          let width = dimension (Bytes.get_uint8 entries off) in
          let height = dimension (Bytes.get_uint8 entries (off + 1)) in
          let bits = Bytes.get_uint16_le entries (off + 6) in
          let better =
            match !best with
            | None -> true
            | Some (w, h, b) ->
              width * height > w * h || (width * height = w * h && bits > b)
          in
          if better then best := Some (width, height, bits)
        done;
        match !best with
        | None -> Error (Types.Malformed "empty ico directory")
        | Some (width, height, bits) ->
          Ok
            { Types.format = ICO
            ; width
            ; height
            ; depth = depth_of_bit_count bits
            ; orientation = 1
            }))
  with
  | Types.Imgmeta_error e -> Error e
;;
