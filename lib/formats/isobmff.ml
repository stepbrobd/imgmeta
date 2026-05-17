type box =
  { kind : string
  ; pos : int
  ; size : int
  ; body_off : int
  ; body_len : int
  }

let read_box r ~pos ~limit =
  if pos + 8 > limit
  then None
  else (
    let hdr = Reader.read_at r ~pos ~len:8 in
    let size32 = Int32.to_int (Bytes.get_int32_be hdr 0) in
    let kind = Bytes.sub_string hdr 4 4 in
    let header_len, size =
      if size32 = 1
      then (
        let ext = Reader.read_at r ~pos:(pos + 8) ~len:8 in
        let s = Int64.to_int (Bytes.get_int64_be ext 0) in
        16, s)
      else if size32 = 0
      then 8, limit - pos
      else 8, size32
    in
    Some { kind; pos; size; body_off = pos + header_len; body_len = size - header_len })
;;

let walk_top r f =
  let limit =
    match Reader.size r with
    | Some n -> n
    | None -> max_int
  in
  let cursor = ref 0 in
  let rec go () =
    match read_box r ~pos:!cursor ~limit with
    | None -> ()
    | Some b ->
      f b;
      cursor := b.pos + b.size;
      if b.size = 0 then () else go ()
  in
  go ()
;;

let walk_children r parent f =
  let limit = parent.body_off + parent.body_len in
  let cursor = ref parent.body_off in
  let rec go () =
    match read_box r ~pos:!cursor ~limit with
    | None -> ()
    | Some b ->
      f b;
      cursor := b.pos + b.size;
      if b.size = 0 then () else go ()
  in
  go ()
;;

let walk_children_full r parent f =
  let limit = parent.body_off + parent.body_len in
  let cursor = ref (parent.body_off + 4) in
  let rec go () =
    match read_box r ~pos:!cursor ~limit with
    | None -> ()
    | Some b ->
      f b;
      cursor := b.pos + b.size;
      if b.size = 0 then () else go ()
  in
  go ()
;;

let find_top r kind =
  let result = ref None in
  walk_top r (fun b -> if !result = None && b.kind = kind then result := Some b);
  !result
;;

let find_descendant r parent kind =
  let result = ref None in
  let rec dive p =
    let children =
      if p.kind = "meta" then walk_children_full r p else walk_children r p
    in
    children (fun b ->
      if !result = None
      then
        if b.kind = kind
        then result := Some b
        else if b.kind = "iprp" || b.kind = "ipco" || b.kind = "meta"
        then dive b)
  in
  dive parent;
  !result
;;

let read_uint r ~pos ~size =
  if size = 0
  then 0
  else (
    let b = Reader.read_at r ~pos ~len:size in
    let v = ref 0 in
    for i = 0 to size - 1 do
      v := (!v lsl 8) lor Bytes.get_uint8 b i
    done;
    !v)
;;

let find_exif_item_id r meta =
  let result = ref None in
  match find_descendant r meta "iinf" with
  | None -> None
  | Some iinf ->
    let cursor = ref (iinf.body_off + 4) in
    let version_byte = Reader.read_at r ~pos:iinf.body_off ~len:1 in
    let version = Bytes.get_uint8 version_byte 0 in
    let entry_count_size = if version = 0 then 2 else 4 in
    let entry_count = read_uint r ~pos:!cursor ~size:entry_count_size in
    cursor := !cursor + entry_count_size;
    let limit = iinf.body_off + iinf.body_len in
    let n = ref 0 in
    while !result = None && !n < entry_count && !cursor < limit do
      match read_box r ~pos:!cursor ~limit with
      | None -> cursor := limit
      | Some infe ->
        let infe_ver_bytes = Reader.read_at r ~pos:infe.body_off ~len:1 in
        let infe_ver = Bytes.get_uint8 infe_ver_bytes 0 in
        let id_size = if infe_ver >= 3 then 4 else 2 in
        let id = read_uint r ~pos:(infe.body_off + 4) ~size:id_size in
        let type_off = infe.body_off + 4 + id_size + 2 in
        let ty = Bytes.to_string (Reader.read_at r ~pos:type_off ~len:4) in
        if String.equal ty "Exif" then result := Some id;
        cursor := infe.pos + infe.size;
        incr n
    done;
    !result
;;

let find_item_extent r meta ~item_id =
  match find_descendant r meta "iloc" with
  | None -> None
  | Some iloc ->
    let header = Reader.read_at r ~pos:iloc.body_off ~len:6 in
    let version = Bytes.get_uint8 header 0 in
    let packed1 = Bytes.get_uint8 header 4 in
    let offset_size = (packed1 lsr 4) land 0xf in
    let length_size = packed1 land 0xf in
    let packed2 = Bytes.get_uint8 header 5 in
    let base_offset_size = (packed2 lsr 4) land 0xf in
    let index_size = if version >= 1 then packed2 land 0xf else 0 in
    let count_size = if version = 2 then 4 else 2 in
    let cursor = ref (iloc.body_off + 6) in
    let item_count = read_uint r ~pos:!cursor ~size:count_size in
    cursor := !cursor + count_size;
    let id_size = if version = 2 then 4 else 2 in
    let limit = iloc.body_off + iloc.body_len in
    let result = ref None in
    let i = ref 0 in
    while !result = None && !i < item_count && !cursor < limit do
      let id = read_uint r ~pos:!cursor ~size:id_size in
      cursor := !cursor + id_size;
      if version = 1 || version = 2 then cursor := !cursor + 2;
      cursor := !cursor + 2;
      let base_offset = read_uint r ~pos:!cursor ~size:base_offset_size in
      cursor := !cursor + base_offset_size;
      let extent_count = read_uint r ~pos:!cursor ~size:2 in
      cursor := !cursor + 2;
      if id = item_id && extent_count >= 1
      then (
        if (version = 1 || version = 2) && index_size > 0
        then cursor := !cursor + index_size;
        let extent_offset = read_uint r ~pos:!cursor ~size:offset_size in
        cursor := !cursor + offset_size;
        let extent_length = read_uint r ~pos:!cursor ~size:length_size in
        result := Some (base_offset + extent_offset, extent_length))
      else
        for _ = 1 to extent_count do
          if (version = 1 || version = 2) && index_size > 0
          then cursor := !cursor + index_size;
          cursor := !cursor + offset_size + length_size
        done;
      incr i
    done;
    !result
;;
