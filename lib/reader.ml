type backend =
  | Bytes_b of
      { data : bytes
      ; mutable pos : int
      }
  | Chan_b of
      { ic : In_channel.t
      ; seekable : bool
      ; size : int option
      ; owns : bool
      ; mutable cursor : int
      }

type t = { mutable b : backend }

let of_bytes data = { b = Bytes_b { data; pos = 0 } }

let of_in_channel ic =
  let seekable, size =
    try
      let _ = In_channel.pos ic in
      let len = In_channel.length ic in
      true, Some (Int64.to_int len)
    with
    | _ -> false, None
  in
  Ok { b = Chan_b { ic; seekable; size; owns = false; cursor = 0 } }
;;

let of_file path =
  try
    let ic = In_channel.open_bin path in
    let len = In_channel.length ic |> Int64.to_int in
    Ok { b = Chan_b { ic; seekable = true; size = Some len; owns = true; cursor = 0 } }
  with
  | Sys_error msg -> Error (Types.Io_error msg)
;;

let pos t =
  match t.b with
  | Bytes_b { pos; _ } -> pos
  | Chan_b { cursor; _ } -> cursor
;;

let size t =
  match t.b with
  | Bytes_b { data; _ } -> Some (Bytes.length data)
  | Chan_b { size; _ } -> size
;;

let seek t p =
  match t.b with
  | Bytes_b r -> r.pos <- p
  | Chan_b r ->
    if not r.seekable then raise (Types.Imgmeta_error Truncated);
    In_channel.seek r.ic (Int64.of_int p);
    r.cursor <- p
;;

let read t len =
  match t.b with
  | Bytes_b r ->
    let avail = Bytes.length r.data - r.pos in
    if len > avail then raise (Types.Imgmeta_error Truncated);
    let out = Bytes.sub r.data r.pos len in
    r.pos <- r.pos + len;
    out
  | Chan_b r ->
    let buf = Bytes.create len in
    (match In_channel.really_input r.ic buf 0 len with
     | Some () ->
       r.cursor <- r.cursor + len;
       buf
     | None -> raise (Types.Imgmeta_error Truncated))
;;

let read_at t ~pos ~len =
  match t.b with
  | Bytes_b r ->
    if pos + len > Bytes.length r.data then raise (Types.Imgmeta_error Truncated);
    Bytes.sub r.data pos len
  | Chan_b r ->
    if not r.seekable then raise (Types.Imgmeta_error Truncated);
    In_channel.seek r.ic (Int64.of_int pos);
    let buf = Bytes.create len in
    (match In_channel.really_input r.ic buf 0 len with
     | Some () ->
       r.cursor <- pos + len;
       buf
     | None -> raise (Types.Imgmeta_error Truncated))
;;

let close t =
  match t.b with
  | Bytes_b _ -> ()
  | Chan_b r -> if r.owns then In_channel.close r.ic
;;
