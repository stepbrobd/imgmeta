(* jpeg xl packs its headers into a bit stream read least significant bit first
   within each byte *)
type bits =
  { data : bytes
  ; mutable at : int
  }

let bit_reader data = { data; at = 0 }

let read_bits t n =
  let v = ref 0 in
  for i = 0 to n - 1 do
    let p = t.at + i in
    let byte = p lsr 3 in
    if byte >= Bytes.length t.data then raise (Types.Imgmeta_error Types.Truncated);
    let bit = (Bytes.get_uint8 t.data byte lsr (p land 7)) land 1 in
    v := !v lor (bit lsl i)
  done;
  t.at <- t.at + n;
  !v
;;

let read_bool t = read_bits t 1 = 1

(* a two bit selector picks one of four encodings, each either a constant or a
   fixed width field added to an offset *)
let read_u32 t options =
  match List.nth options (read_bits t 2) with
  | `Const v -> v
  | `Bits (n, offset) -> read_bits t n + offset
;;

let size_options = [ `Bits (9, 1); `Bits (13, 1); `Bits (18, 1); `Bits (30, 1) ]
let integer_depths = [ `Const 8; `Const 16; `Const 32; `Bits (6, 1) ]
let float_depths = [ `Const 32; `Const 16; `Const 24; `Bits (6, 1) ]

(* a non zero ratio derives the width from the height instead of coding it *)
let ratios = [| 0, 1; 1, 1; 12, 10; 4, 3; 3, 2; 16, 9; 5, 4; 2, 1 |]

let read_size_header t =
  let small = read_bool t in
  let height = if small then (read_bits t 5 + 1) * 8 else read_u32 t size_options in
  let ratio = read_bits t 3 in
  let width =
    if ratio = 0
    then if small then (read_bits t 5 + 1) * 8 else read_u32 t size_options
    else (
      let num, den = ratios.(ratio) in
      height * num / den)
  in
  width, height
;;

let read_bit_depth t =
  if read_bool t then read_u32 t float_depths else read_u32 t integer_depths
;;

let preview_div8_options = [ `Const 16; `Const 32; `Bits (5, 1); `Bits (9, 33) ]
let preview_options = [ `Bits (6, 1); `Bits (8, 65); `Bits (10, 321); `Bits (12, 1345) ]

(* shaped like the size header, with its own field widths *)
let skip_preview_header t =
  let options = if read_bool t then preview_div8_options else preview_options in
  ignore (read_u32 t options : int);
  if read_bits t 3 = 0 then ignore (read_u32 t options : int)
;;

let tps_numerator_options = [ `Const 100; `Const 1000; `Bits (10, 1); `Bits (30, 1) ]
let tps_denominator_options = [ `Const 1; `Const 1001; `Bits (8, 1); `Bits (10, 1) ]
let num_loops_options = [ `Const 0; `Bits (3, 0); `Bits (16, 0); `Bits (32, 0) ]

let skip_animation_header t =
  ignore (read_u32 t tps_numerator_options : int);
  ignore (read_u32 t tps_denominator_options : int);
  ignore (read_u32 t num_loops_options : int);
  ignore (read_bool t : bool)
;;

(* the bit depth sits after the optional headers rather than before them, which
   means reaching it requires walking past whichever of them are present *)
let read_image_metadata t =
  if read_bool t
  then 8, 1
  else (
    let orientation =
      if not (read_bool t)
      then 1
      else (
        let orientation = read_bits t 3 + 1 in
        if read_bool t then ignore (read_size_header t : int * int);
        if read_bool t then skip_preview_header t;
        if read_bool t then skip_animation_header t;
        orientation)
    in
    let depth = read_bit_depth t in
    depth, orientation)
;;

let signature = "\xff\x0a"

let strip_signature b =
  if Bytes.length b >= 2 && String.equal (Bytes.sub_string b 0 2) signature
  then Bytes.sub b 2 (Bytes.length b - 2)
  else b
;;

(* the size header and the leading image metadata fields fit well inside this *)
let window = 64

let from_codestream r ~pos =
  let avail =
    match Reader.size r with
    | Some n -> max 0 (min window (n - pos))
    | None -> window
  in
  let t = bit_reader (strip_signature (Reader.read_at r ~pos ~len:avail)) in
  let width, height = read_size_header t in
  let depth, orientation = read_image_metadata t in
  if width <= 0 || height <= 0
  then Error (Types.Malformed "bad jxl dimensions")
  else (
    let w, h =
      if orientation >= 5 && orientation <= 8 then height, width else width, height
    in
    Ok { Types.format = Types.JXL; width = w; height = h; depth; orientation })
;;

let read_metadata r =
  try
    let head = Reader.read_at r ~pos:0 ~len:2 in
    if String.equal (Bytes.to_string head) signature
    then from_codestream r ~pos:0
    else (
      match Isobmff.find_top r "jxlc" with
      | Some box -> from_codestream r ~pos:box.body_off
      | None ->
        (match Isobmff.find_top r "jxlp" with
         (* every jxlp box prefixes its fragment with a four byte index *)
         | Some box -> from_codestream r ~pos:(box.body_off + 4)
         | None -> Error (Types.Malformed "missing jxl codestream")))
  with
  | Types.Imgmeta_error e -> Error e
;;
