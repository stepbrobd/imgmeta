let starts_with b prefix =
  let pn = String.length prefix in
  Bytes.length b >= pn && String.equal (Bytes.sub_string b 0 pn) prefix
;;

let four b ~at = if Bytes.length b < at + 4 then "" else Bytes.sub_string b at 4

let isobmff_brand b =
  if Bytes.length b < 12
  then None
  else if not (String.equal (four b ~at:4) "ftyp")
  then None
  else Some (four b ~at:8)
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
    match isobmff_brand b with
    | None -> None
    | Some brand ->
      (match brand with
       | "avif" | "avis" -> Some AVIF
       | "heic" | "heix" | "mif1" | "heim" | "heis" -> Some HEIF
       | _ -> None))
;;

let detect r =
  let head =
    try Reader.read_at r ~pos:0 ~len:16 with
    | Types.Imgmeta_error _ -> Bytes.create 0
  in
  match of_bytes head with
  | Some f -> Ok f
  | None -> Error Types.Unknown_format
;;
