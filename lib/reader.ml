type backend =
  | Bytes_b of
      { data : bytes
      ; mutable pos : int
      }

type t = { mutable b : backend }

let of_bytes data = { b = Bytes_b { data; pos = 0 } }
let of_file _ = Error (Types.Io_error "of_file not yet implemented")
let of_in_channel _ = Error (Types.Io_error "of_in_channel not yet implemented")

let pos t =
  match t.b with
  | Bytes_b { pos; _ } -> pos
;;

let size t =
  match t.b with
  | Bytes_b { data; _ } -> Some (Bytes.length data)
;;

let seek t p =
  match t.b with
  | Bytes_b r -> r.pos <- p
;;

let read t len =
  match t.b with
  | Bytes_b r ->
    let avail = Bytes.length r.data - r.pos in
    if len > avail then raise (Types.Imgmeta_error Truncated);
    let out = Bytes.sub r.data r.pos len in
    r.pos <- r.pos + len;
    out
;;

let read_at t ~pos ~len =
  match t.b with
  | Bytes_b r ->
    if pos + len > Bytes.length r.data then raise (Types.Imgmeta_error Truncated);
    Bytes.sub r.data pos len
;;

let close _ = ()
