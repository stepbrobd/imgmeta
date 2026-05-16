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
