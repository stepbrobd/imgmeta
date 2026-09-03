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

let ipco_children r meta =
  match Isobmff.find_descendant r meta "ipco" with
  | None -> [||]
  | Some ipco ->
    let acc = ref [] in
    Isobmff.walk_children r ipco (fun b -> acc := b :: !acc);
    Array.of_list (List.rev !acc)
;;

(* properties belong to an item. pitm and ipma decide which ispe, pixi and irot
   describe the image the file is about. taking the first of each kind
   returns a thumbnail's dimensions whenever one is listed ahead of the primary
   item. files that associate nothing fall back to every property in order,
   which is what a single item file needs *)
let primary_properties r meta =
  let children = ipco_children r meta in
  let indices =
    match Isobmff.find_primary_item_id r meta with
    | None -> []
    | Some id -> Isobmff.find_property_indices r meta ~item_id:id
  in
  match indices with
  | [] -> Array.to_list children
  | _ ->
    List.filter_map
      (fun i ->
         if i >= 1 && i <= Array.length children then Some children.(i - 1) else None)
      indices
;;

let extract r ~format =
  try
    match Isobmff.find_top r "meta" with
    | None -> Error (Types.Malformed "missing meta box")
    | Some meta ->
      let properties = primary_properties r meta in
      let pick kind =
        List.find_opt (fun (b : Isobmff.box) -> String.equal b.kind kind) properties
      in
      (match pick "ispe" with
       | None -> Error (Types.Malformed "missing ispe box")
       | Some i ->
         let w, h = read_ispe r i in
         let depth =
           match pick "pixi" with
           | None -> 8
           | Some p -> read_pixi r p
         in
         let orientation =
           match pick "irot" with
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
