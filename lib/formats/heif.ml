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

let extract r ~format =
  try
    match Isobmff.find_top r "meta" with
    | None -> Error (Types.Malformed "missing meta box")
    | Some meta ->
      let ispe = ref None in
      let pixi = ref None in
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
         Ok { Types.format; width = w; height = h; depth; orientation = 1 })
  with
  | Types.Imgmeta_error e -> Error e
;;

let read_metadata r = extract r ~format:Types.HEIF
