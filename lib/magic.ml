let starts_with b prefix =
  let pn = String.length prefix in
  Bytes.length b >= pn && String.equal (Bytes.sub_string b 0 pn) prefix
;;

let four b ~at = if Bytes.length b < at + 4 then "" else Bytes.sub_string b at 4

let heif_brands =
  [ "heic"
  ; "heix"
  ; "heim"
  ; "heis"
  ; "hevc"
  ; "hevx"
  ; "hevm"
  ; "hevs"
  ; "mif1"
  ; "msf1"
  ; "miaf"
  ]
;;

let avif_brands = [ "avif"; "avis"; "avio" ]

(* the major brand alone does not identify the codec: a conforming AVIF may name
   mif1 as its major brand and carry avif only among the compatible brands *)
let isobmff_brands b =
  if Bytes.length b < 12 || not (String.equal (four b ~at:4) "ftyp")
  then []
  else (
    let box_len = Int32.to_int (Bytes.get_int32_be b 0) in
    let avail = Bytes.length b in
    let limit = if box_len <= 0 then avail else min box_len avail in
    let rec compatible at acc =
      if at + 4 > limit then acc else compatible (at + 4) (four b ~at :: acc)
    in
    four b ~at:8 :: compatible 16 [])
;;

let of_bytes b : Types.format option =
  if starts_with b "\x89PNG\r\n\x1a\n"
  then Some PNG
  else if starts_with b "\xff\xd8\xff"
  then Some JPEG
  else if starts_with b "GIF87a" || starts_with b "GIF89a"
  then Some GIF
  else if
    starts_with b "RIFF" && Bytes.length b >= 12 && String.equal (four b ~at:8) "WEBP"
  then Some WebP
  else (
    let brands = isobmff_brands b in
    let any set = List.exists (fun brand -> List.mem brand set) brands in
    if any avif_brands then Some AVIF else if any heif_brands then Some HEIF else None)
;;

let sniff_len = 64

(* brand scanning wants more than the magic number, but a short file must still
   be classified. fall back to the largest prefix the source can supply *)
let rec sniff r n =
  if n <= 0
  then Bytes.create 0
  else (
    try Reader.read_at r ~pos:0 ~len:n with
    | Types.Imgmeta_error _ -> sniff r (n / 2))
;;

let detect r =
  let start =
    match Reader.size r with
    | Some n -> min sniff_len n
    | None -> sniff_len
  in
  match of_bytes (sniff r start) with
  | Some f -> Ok f
  | None -> Error Types.Unknown_format
;;
