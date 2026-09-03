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
      ; ahead : Buffer.t
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
  Ok
    { b =
        Chan_b { ic; seekable; size; owns = false; ahead = Buffer.create 16; cursor = 0 }
    }
;;

let of_file path =
  try
    let ic = In_channel.open_bin path in
    let len = In_channel.length ic |> Int64.to_int in
    Ok
      { b =
          Chan_b
            { ic
            ; seekable = true
            ; size = Some len
            ; owns = true
            ; ahead = Buffer.create 16
            ; cursor = 0
            }
      }
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

let fail e = raise (Types.Imgmeta_error e)
let malformed msg = fail (Types.Malformed msg)

(* every request is validated before it reaches Bytes.create or Bytes.sub. a
   length taken from the file cannot raise Invalid_argument out of the parsers,
   and cannot ask for an allocation larger than the source *)
let check t ~pos ~len =
  if pos < 0 || len < 0 then malformed "negative offset or length";
  match size t with
  | None -> ()
  | Some n -> if pos > n || len > n - pos then fail Types.Truncated
;;

let ahead_chunk = 65536

(* a non-seekable channel is served from a read-ahead cache. the amount held is
   bounded by what the stream actually yields rather than by a requested len *)
let fill_ahead ic ahead n =
  let rec go () =
    let have = Buffer.length ahead in
    if have >= n
    then true
    else (
      let want = min ahead_chunk (n - have) in
      let buf = Bytes.create want in
      let got = In_channel.input ic buf 0 want in
      if got = 0
      then false
      else (
        Buffer.add_subbytes ahead buf 0 got;
        go ()))
  in
  go ()
;;

let read_at t ~pos ~len =
  check t ~pos ~len;
  match t.b with
  | Bytes_b r -> Bytes.sub r.data pos len
  | Chan_b r ->
    if r.seekable
    then (
      In_channel.seek r.ic (Int64.of_int pos);
      let buf = Bytes.create len in
      match In_channel.really_input r.ic buf 0 len with
      | Some () ->
        r.cursor <- pos + len;
        buf
      | None -> fail Types.Truncated)
    else if not (fill_ahead r.ic r.ahead (pos + len))
    then fail Types.Truncated
    else (
      let out = Bytes.of_string (Buffer.sub r.ahead pos len) in
      r.cursor <- pos + len;
      out)
;;

let read t ~len =
  let p = pos t in
  let out = read_at t ~pos:p ~len in
  (match t.b with
   | Bytes_b r -> r.pos <- p + len
   | Chan_b _ -> ());
  out
;;

let seek t p =
  if p < 0 then malformed "negative seek";
  match t.b with
  | Bytes_b r -> r.pos <- p
  | Chan_b r ->
    if r.seekable then In_channel.seek r.ic (Int64.of_int p);
    r.cursor <- p
;;

let close t =
  match t.b with
  | Bytes_b _ -> ()
  | Chan_b r -> if r.owns then In_channel.close r.ic
;;
