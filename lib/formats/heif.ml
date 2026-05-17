let read_ispe r (box : Isobmff.box) =
  let body = Reader.read_at r ~pos:(box.body_off + 4) ~len:8 in
  let w = Int32.to_int (Bytes.get_int32_be body 0) in
  let h = Int32.to_int (Bytes.get_int32_be body 4) in
  w, h
;;

let read_pixi r (box : Isobmff.box) =
  let head = Reader.read_at r ~pos:(box.body_off + 4) ~len:1 in
  let n = Bytes.get_uint8 head 0 in
  if n = 0
  then 8
  else (
    let depths = Reader.read_at r ~pos:(box.body_off + 5) ~len:n in
    Bytes.get_uint8 depths 0)
;;

let read_irot r (box : Isobmff.box) =
  if box.body_len < 1
  then 0
  else (
    let body = Reader.read_at r ~pos:box.body_off ~len:1 in
    Bytes.get_uint8 body 0 land 0x3)
;;

let irot_to_exif = function
  | 0 -> 1
  | 1 -> 8
  | 2 -> 3
  | 3 -> 6
  | _ -> 1
;;

let read_exif_item r meta =
  match Isobmff.find_exif_item_id r meta with
  | None -> 1
  | Some id ->
    (match Isobmff.find_item_extent r meta ~item_id:id with
     | None -> 1
     | Some (offset, length) ->
       if length < 4
       then 1
       else (
         let payload = Reader.read_at r ~pos:offset ~len:length in
         let header_offset = Int32.to_int (Bytes.get_int32_be payload 0) in
         let skip = 4 + header_offset in
         if skip >= length
         then 1
         else Exif.parse_orientation (Bytes.sub payload skip (length - skip))))
;;

let extract r ~format =
  try
    match Isobmff.find_top r "meta" with
    | None -> Error (Types.Malformed "missing meta box")
    | Some meta ->
      let ispe = ref None in
      let pixi = ref None in
      let irot = ref None in
      let rec scan (box : Isobmff.box) =
        let walk =
          if box.kind = "meta"
          then Isobmff.walk_children_full r box
          else Isobmff.walk_children r box
        in
        walk (fun child ->
          match child.kind with
          | "ispe" -> if !ispe = None then ispe := Some child
          | "pixi" -> if !pixi = None then pixi := Some child
          | "irot" -> if !irot = None then irot := Some child
          | "iprp" | "ipco" -> scan child
          | _ -> ())
      in
      scan meta;
      (match !ispe with
       | None -> Error (Types.Malformed "missing ispe box")
       | Some i ->
         let w, h = read_ispe r i in
         let depth =
           match !pixi with
           | None -> 8
           | Some p -> read_pixi r p
         in
         let orientation =
           match !irot with
           | Some b -> irot_to_exif (read_irot r b)
           | None ->
             (try read_exif_item r meta with
              | Types.Imgmeta_error _ -> 1)
         in
         let w, h = if orientation >= 5 && orientation <= 8 then h, w else w, h in
         Ok { Types.format; width = w; height = h; depth; orientation })
  with
  | Types.Imgmeta_error e -> Error e
;;

let read_metadata r = extract r ~format:Types.HEIF
