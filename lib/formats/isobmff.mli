type box =
  { kind : string
  ; pos : int
  ; size : int
  ; body_off : int
  ; body_len : int
  }

val walk_top : Reader.t -> (box -> unit) -> unit
val walk_children : Reader.t -> box -> (box -> unit) -> unit
val walk_children_full : Reader.t -> box -> (box -> unit) -> unit
val find_top : Reader.t -> string -> box option
val find_descendant : Reader.t -> box -> string -> box option
val find_exif_item_id : Reader.t -> box -> int option
val find_item_extent : Reader.t -> box -> item_id:int -> (int * int) option
