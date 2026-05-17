let orientation_tag = 0x0112
let type_short = 3

let parse_orientation b =
  let len = Bytes.length b in
  if len < 10
  then 1
  else (
    let endian =
      match Bytes.get_uint8 b 0, Bytes.get_uint8 b 1 with
      | 0x49, 0x49 -> Some `LE
      | 0x4d, 0x4d -> Some `BE
      | _ -> None
    in
    match endian with
    | None -> 1
    | Some e ->
      let get_u16 = if e = `LE then Bytes.get_uint16_le else Bytes.get_uint16_be in
      let get_u32 b o =
        Int32.to_int (if e = `LE then Bytes.get_int32_le b o else Bytes.get_int32_be b o)
      in
      if get_u16 b 2 <> 0x002a
      then 1
      else (
        let ifd0 = get_u32 b 4 in
        if ifd0 < 8 || ifd0 + 2 > len
        then 1
        else (
          let n = get_u16 b ifd0 in
          let base = ifd0 + 2 in
          if base + (n * 12) > len
          then 1
          else (
            let result = ref 1 in
            for i = 0 to n - 1 do
              let off = base + (i * 12) in
              if get_u16 b off = orientation_tag && get_u16 b (off + 2) = type_short
              then (
                let v = get_u16 b (off + 8) in
                if v >= 1 && v <= 8 then result := v)
            done;
            !result))))
;;
