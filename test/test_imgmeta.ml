open Imgmeta

let test_format_construction () =
  Alcotest.(check int)
    "six format constructors"
    6
    (List.length [ PNG; JPEG; GIF; WebP; HEIF; AVIF ])
;;

let test_record_fields () =
  let m = { format = PNG; width = 10; height = 20; depth = 8; orientation = 1 } in
  Alcotest.(check int) "width" 10 m.width;
  Alcotest.(check int) "height" 20 m.height;
  Alcotest.(check int) "depth" 8 m.depth;
  Alcotest.(check int) "orientation" 1 m.orientation
;;

let test_pp_error_unknown () =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt Unknown_format;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "msg" "unknown_format" (Buffer.contents buf)
;;

let test_exception_carries_error () =
  let raised =
    try raise (Imgmeta_error (Malformed "bad header")) with
    | Imgmeta_error (Malformed s) -> String.equal s "bad header"
    | _ -> false
  in
  Alcotest.(check bool) "raises and matches" true raised
;;

let test_reader_bytes_read () =
  let r = Imgmeta.Reader.of_bytes (Bytes.of_string "abcdef") in
  let chunk = Imgmeta.Reader.read r ~len:3 in
  Alcotest.(check string) "first 3" "abc" (Bytes.to_string chunk);
  Alcotest.(check int) "pos advanced" 3 (Imgmeta.Reader.pos r)
;;

let test_reader_bytes_read_at () =
  let r = Imgmeta.Reader.of_bytes (Bytes.of_string "abcdef") in
  let chunk = Imgmeta.Reader.read_at r ~pos:2 ~len:3 in
  Alcotest.(check string) "from offset 2" "cde" (Bytes.to_string chunk)
;;

let test_reader_bytes_size () =
  let r = Imgmeta.Reader.of_bytes (Bytes.of_string "abcdef") in
  Alcotest.(check (option int)) "size" (Some 6) (Imgmeta.Reader.size r)
;;

let test_reader_bytes_truncated () =
  let r = Imgmeta.Reader.of_bytes (Bytes.of_string "abc") in
  let raised =
    try
      let _ = Imgmeta.Reader.read r ~len:10 in
      false
    with
    | Imgmeta_error Truncated -> true
  in
  Alcotest.(check bool) "raises truncated when over reading" true raised
;;

let with_temp_file content f =
  let path = Filename.temp_file "imgmeta_" ".bin" in
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc content);
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)
;;

let test_reader_file_read () =
  with_temp_file "hello world" (fun path ->
    match Imgmeta.Reader.of_file path with
    | Error _ -> Alcotest.fail "of_file failed"
    | Ok r ->
      let head = Imgmeta.Reader.read r ~len:5 in
      Alcotest.(check string) "first 5" "hello" (Bytes.to_string head);
      let tail = Imgmeta.Reader.read_at r ~pos:6 ~len:5 in
      Alcotest.(check string) "tail" "world" (Bytes.to_string tail);
      Imgmeta.Reader.close r)
;;

let test_reader_in_channel_read () =
  with_temp_file "abcdef" (fun path ->
    In_channel.with_open_bin path (fun ic ->
      match Imgmeta.Reader.of_in_channel ic with
      | Error _ -> Alcotest.fail "of_in_channel failed"
      | Ok r ->
        let chunk = Imgmeta.Reader.read r ~len:3 in
        Alcotest.(check string) "head" "abc" (Bytes.to_string chunk)))
;;

let bytes_of_hex hex =
  let clean = String.concat "" (String.split_on_char ' ' hex) in
  let len = String.length clean / 2 in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    let hi = clean.[i * 2] in
    let lo = clean.[(i * 2) + 1] in
    let v c =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | _ -> failwith "bad hex"
    in
    Bytes.set_uint8 b i ((v hi * 16) + v lo)
  done;
  b
;;

let tiff_with_entries ~endian entries =
  let n = List.length entries in
  let buf = Bytes.create (8 + 2 + (12 * n) + 4) in
  let set_u16 = if endian = `LE then Bytes.set_uint16_le else Bytes.set_uint16_be in
  let set_u32 b o v =
    if endian = `LE
    then Bytes.set_int32_le b o (Int32.of_int v)
    else Bytes.set_int32_be b o (Int32.of_int v)
  in
  Bytes.set_uint8 buf 0 (if endian = `LE then 0x49 else 0x4d);
  Bytes.set_uint8 buf 1 (if endian = `LE then 0x49 else 0x4d);
  set_u16 buf 2 0x002a;
  set_u32 buf 4 8;
  set_u16 buf 8 n;
  List.iteri
    (fun i (tag, ty, count, value_lo16) ->
       let off = 10 + (i * 12) in
       set_u16 buf off tag;
       set_u16 buf (off + 2) ty;
       set_u32 buf (off + 4) count;
       set_u16 buf (off + 8) value_lo16;
       set_u16 buf (off + 10) 0)
    entries;
  set_u32 buf (10 + (12 * n)) 0;
  buf
;;

let test_exif_orientation_le () =
  let tiff = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, 6 ] in
  Alcotest.(check int) "le orientation 6" 6 (Imgmeta.Formats.Exif.parse_orientation tiff)
;;

let test_exif_orientation_be () =
  let tiff = tiff_with_entries ~endian:`BE [ 0x0112, 3, 1, 8 ] in
  Alcotest.(check int) "be orientation 8" 8 (Imgmeta.Formats.Exif.parse_orientation tiff)
;;

let test_exif_missing_orientation () =
  let tiff = tiff_with_entries ~endian:`LE [ 0x0100, 3, 1, 256 ] in
  Alcotest.(check int)
    "missing tag returns 1"
    1
    (Imgmeta.Formats.Exif.parse_orientation tiff)
;;

let test_exif_invalid_magic () =
  let tiff = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, 6 ] in
  Bytes.set_uint8 tiff 2 0xff;
  Alcotest.(check int)
    "bad magic returns 1"
    1
    (Imgmeta.Formats.Exif.parse_orientation tiff)
;;

let test_exif_empty () =
  Alcotest.(check int)
    "empty returns 1"
    1
    (Imgmeta.Formats.Exif.parse_orientation Bytes.empty)
;;

let test_exif_out_of_range () =
  let tiff = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, 9 ] in
  Alcotest.(check int)
    "value > 8 returns 1"
    1
    (Imgmeta.Formats.Exif.parse_orientation tiff)
;;

let check_magic name hex expected =
  let b = bytes_of_hex hex in
  Alcotest.(check (option string))
    name
    (Some (Imgmeta.format_to_string expected))
    (Option.map Imgmeta.format_to_string (Imgmeta.Magic.of_bytes b))
;;

let test_magic_png () =
  check_magic "png" "89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52" PNG
;;

let test_magic_jpeg () =
  check_magic "jpeg" "ff d8 ff e0 00 10 4a 46 49 46 00 01 01 00 00 01" JPEG
;;

let test_magic_gif () =
  check_magic "gif" "47 49 46 38 39 61 0a 00 0a 00 80 00 00 ff ff ff" GIF
;;

let test_magic_webp () =
  check_magic "webp" "52 49 46 46 24 00 00 00 57 45 42 50 56 50 38 20" WebP
;;

let test_magic_heif () =
  check_magic "heif" "00 00 00 18 66 74 79 70 68 65 69 63 00 00 00 00" HEIF
;;

let test_magic_avif () =
  check_magic "avif" "00 00 00 18 66 74 79 70 61 76 69 66 00 00 00 00" AVIF
;;

let test_magic_unknown () =
  let b = bytes_of_hex "00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff" in
  Alcotest.(check (option string))
    "unknown returns none"
    None
    (Option.map Imgmeta.format_to_string (Imgmeta.Magic.of_bytes b))
;;

let png_chunk buf ty body =
  let len = Bytes.length body in
  let len_bytes = Bytes.create 4 in
  Bytes.set_int32_be len_bytes 0 (Int32.of_int len);
  Buffer.add_bytes buf len_bytes;
  Buffer.add_string buf ty;
  Buffer.add_bytes buf body;
  Buffer.add_string buf "\x00\x00\x00\x00"
;;

let png_header ~width ~height ~depth ~color_type =
  let buf = Buffer.create 64 in
  Buffer.add_string buf "\x89PNG\r\n\x1a\n";
  let ihdr = Bytes.create 13 in
  Bytes.set_int32_be ihdr 0 (Int32.of_int width);
  Bytes.set_int32_be ihdr 4 (Int32.of_int height);
  Bytes.set_uint8 ihdr 8 depth;
  Bytes.set_uint8 ihdr 9 color_type;
  Bytes.set_uint8 ihdr 10 0;
  Bytes.set_uint8 ihdr 11 0;
  Bytes.set_uint8 ihdr 12 0;
  png_chunk buf "IHDR" ihdr;
  Buffer.to_bytes buf
;;

let png_with_exif ~width ~height ~depth ~color_type ~orientation =
  let buf = Buffer.create 96 in
  Buffer.add_string buf "\x89PNG\r\n\x1a\n";
  let ihdr = Bytes.create 13 in
  Bytes.set_int32_be ihdr 0 (Int32.of_int width);
  Bytes.set_int32_be ihdr 4 (Int32.of_int height);
  Bytes.set_uint8 ihdr 8 depth;
  Bytes.set_uint8 ihdr 9 color_type;
  Bytes.set_uint8 ihdr 10 0;
  Bytes.set_uint8 ihdr 11 0;
  Bytes.set_uint8 ihdr 12 0;
  png_chunk buf "IHDR" ihdr;
  png_chunk buf "eXIf" (tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, orientation ]);
  Buffer.to_bytes buf
;;

let load_bytes path =
  In_channel.with_open_bin path (fun ic ->
    let len = In_channel.length ic |> Int64.to_int in
    let buf = Bytes.create len in
    match In_channel.really_input ic buf 0 len with
    | Some () -> buf
    | None -> failwith "short read")
;;

let test_png_synthesized () =
  let data = png_header ~width:200 ~height:100 ~depth:8 ~color_type:2 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Png.read_metadata r with
  | Error e -> Alcotest.failf "expected ok, got %a" Imgmeta.pp_error e
  | Ok m ->
    Alcotest.(check int) "width" 200 m.width;
    Alcotest.(check int) "height" 100 m.height;
    Alcotest.(check int) "depth" 8 m.depth
;;

let test_png_synthesized_16bit () =
  let data = png_header ~width:2 ~height:2 ~depth:16 ~color_type:6 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Png.read_metadata r with
  | Ok m -> Alcotest.(check int) "depth 16" 16 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_png_orientation_swap () =
  let data = png_with_exif ~width:200 ~height:100 ~depth:8 ~color_type:2 ~orientation:6 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Png.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "swapped width" 100 m.width;
    Alcotest.(check int) "swapped height" 200 m.height;
    Alcotest.(check int) "orientation" 6 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_png_orientation_no_swap () =
  let data = png_with_exif ~width:200 ~height:100 ~depth:8 ~color_type:2 ~orientation:3 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Png.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "unswapped width" 200 m.width;
    Alcotest.(check int) "unswapped height" 100 m.height;
    Alcotest.(check int) "orientation" 3 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_png_fixture () =
  let data = load_bytes "fixture.png" in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Png.read_metadata r with
  | Error e -> Alcotest.failf "fixture.png failed %a" Imgmeta.pp_error e
  | Ok m ->
    Alcotest.(check int) "width" 320 m.width;
    Alcotest.(check int) "height" 320 m.height;
    Alcotest.(check int) "depth" 8 m.depth
;;

let gif_header ~width ~height ~color_res =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "GIF89a";
  let dim = Bytes.create 4 in
  Bytes.set_uint16_le dim 0 width;
  Bytes.set_uint16_le dim 2 height;
  Buffer.add_bytes buf dim;
  let packed = ((color_res - 1) land 0b111) lsl 4 in
  Buffer.add_char buf (Char.chr packed);
  Buffer.add_char buf '\x00';
  Buffer.add_char buf '\x00';
  Buffer.to_bytes buf
;;

let test_gif_synthesized () =
  let data = gif_header ~width:64 ~height:48 ~color_res:8 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Gif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 64 m.width;
    Alcotest.(check int) "height" 48 m.height;
    Alcotest.(check int) "depth" 8 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let jpeg_header ~width ~height ~precision =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "\xff\xd8";
  Buffer.add_string buf "\xff\xe0\x00\x10JFIF\x00\x01\x01\x00\x00\x01\x00\x01\x00\x00";
  Buffer.add_string buf "\xff\xc0";
  let len_bytes = Bytes.create 2 in
  Bytes.set_uint16_be len_bytes 0 17;
  Buffer.add_bytes buf len_bytes;
  Buffer.add_char buf (Char.chr precision);
  let dims = Bytes.create 4 in
  Bytes.set_uint16_be dims 0 height;
  Bytes.set_uint16_be dims 2 width;
  Buffer.add_bytes buf dims;
  Buffer.add_char buf '\x03';
  for _ = 1 to 9 do
    Buffer.add_char buf '\x00'
  done;
  Buffer.to_bytes buf
;;

let jpeg_with_exif ~width ~height ~precision ~orientation =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\xff\xd8";
  let exif_tiff = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, orientation ] in
  let body = Bytes.cat (Bytes.of_string "Exif\x00\x00") exif_tiff in
  let app1_len = 2 + Bytes.length body in
  Buffer.add_string buf "\xff\xe1";
  let len_bytes = Bytes.create 2 in
  Bytes.set_uint16_be len_bytes 0 app1_len;
  Buffer.add_bytes buf len_bytes;
  Buffer.add_bytes buf body;
  Buffer.add_string buf "\xff\xc0";
  let sof_len = Bytes.create 2 in
  Bytes.set_uint16_be sof_len 0 17;
  Buffer.add_bytes buf sof_len;
  Buffer.add_char buf (Char.chr precision);
  let dims = Bytes.create 4 in
  Bytes.set_uint16_be dims 0 height;
  Bytes.set_uint16_be dims 2 width;
  Buffer.add_bytes buf dims;
  Buffer.add_char buf '\x03';
  for _ = 1 to 9 do
    Buffer.add_char buf '\x00'
  done;
  Buffer.to_bytes buf
;;

let test_jpeg_orientation_swap () =
  let data = jpeg_with_exif ~width:320 ~height:240 ~precision:8 ~orientation:6 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Jpeg.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "swapped width" 240 m.width;
    Alcotest.(check int) "swapped height" 320 m.height;
    Alcotest.(check int) "orientation" 6 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_jpeg_orientation_no_swap () =
  let data = jpeg_with_exif ~width:320 ~height:240 ~precision:8 ~orientation:2 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Jpeg.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "unswapped width" 320 m.width;
    Alcotest.(check int) "orientation" 2 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_jpeg_synthesized () =
  let data = jpeg_header ~width:320 ~height:240 ~precision:8 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Jpeg.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 320 m.width;
    Alcotest.(check int) "height" 240 m.height;
    Alcotest.(check int) "depth" 8 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_jpeg_fixture () =
  let data = load_bytes "fixture.jpeg" in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Jpeg.read_metadata r with
  | Error e -> Alcotest.failf "fixture.jpeg failed %a" Imgmeta.pp_error e
  | Ok m ->
    Alcotest.(check int) "width" 320 m.width;
    Alcotest.(check int) "height" 320 m.height;
    Alcotest.(check int) "depth" 8 m.depth
;;

let riff_header chunks =
  let body = Buffer.create 64 in
  Buffer.add_string body "WEBP";
  List.iter
    (fun (ty, data) ->
       Buffer.add_string body ty;
       let len = Bytes.create 4 in
       Bytes.set_int32_le len 0 (Int32.of_int (Bytes.length data));
       Buffer.add_bytes body len;
       Buffer.add_bytes body data;
       if Bytes.length data mod 2 = 1 then Buffer.add_char body '\x00')
    chunks;
  let out = Buffer.create 96 in
  Buffer.add_string out "RIFF";
  let size = Bytes.create 4 in
  Bytes.set_int32_le size 0 (Int32.of_int (Buffer.length body));
  Buffer.add_bytes out size;
  Buffer.add_buffer out body;
  Buffer.to_bytes out
;;

let webp_vp8x_body ~width ~height ~flags =
  let body = Bytes.create 10 in
  Bytes.fill body 0 10 '\x00';
  Bytes.set_uint8 body 0 flags;
  Bytes.set_uint8 body 4 ((width - 1) land 0xff);
  Bytes.set_uint8 body 5 (((width - 1) lsr 8) land 0xff);
  Bytes.set_uint8 body 6 (((width - 1) lsr 16) land 0xff);
  Bytes.set_uint8 body 7 ((height - 1) land 0xff);
  Bytes.set_uint8 body 8 (((height - 1) lsr 8) land 0xff);
  Bytes.set_uint8 body 9 (((height - 1) lsr 16) land 0xff);
  body
;;

let webp_vp8x ~width ~height =
  riff_header [ "VP8X", webp_vp8x_body ~width ~height ~flags:0 ]
;;

let webp_vp8x_with_exif ~width ~height ~orientation =
  let vp8x = webp_vp8x_body ~width ~height ~flags:0x08 in
  let exif = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, orientation ] in
  riff_header [ "VP8X", vp8x; "EXIF", exif ]
;;

let webp_vp8l ~width ~height =
  let body = Bytes.create 5 in
  Bytes.set_uint8 body 0 0x2f;
  let wm1 = width - 1 in
  let hm1 = height - 1 in
  Bytes.set_uint8 body 1 (wm1 land 0xff);
  Bytes.set_uint8 body 2 ((wm1 lsr 8) land 0x3f lor ((hm1 land 0x03) lsl 6));
  Bytes.set_uint8 body 3 ((hm1 lsr 2) land 0xff);
  Bytes.set_uint8 body 4 ((hm1 lsr 10) land 0x0f);
  riff_header [ "VP8L", body ]
;;

let test_webp_vp8x () =
  let data = webp_vp8x ~width:512 ~height:256 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Webp.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 512 m.width;
    Alcotest.(check int) "height" 256 m.height;
    Alcotest.(check int) "depth" 8 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_webp_orientation_swap () =
  let data = webp_vp8x_with_exif ~width:512 ~height:256 ~orientation:6 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Webp.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "swapped width" 256 m.width;
    Alcotest.(check int) "swapped height" 512 m.height;
    Alcotest.(check int) "orientation" 6 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_webp_orientation_no_swap () =
  let data = webp_vp8x_with_exif ~width:512 ~height:256 ~orientation:2 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Webp.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "unswapped width" 512 m.width;
    Alcotest.(check int) "orientation" 2 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_webp_vp8l () =
  let data = webp_vp8l ~width:16 ~height:16 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Webp.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 16 m.width;
    Alcotest.(check int) "height" 16 m.height
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let isobmff_box ty body =
  let buf = Buffer.create (8 + Bytes.length body) in
  let len = Bytes.create 4 in
  Bytes.set_int32_be len 0 (Int32.of_int (8 + Bytes.length body));
  Buffer.add_bytes buf len;
  Buffer.add_string buf ty;
  Buffer.add_bytes buf body;
  Buffer.to_bytes buf
;;

let test_isobmff_walk_top_level () =
  let a = isobmff_box "ftyp" (Bytes.of_string "heic") in
  let b = isobmff_box "free" (Bytes.of_string "xxx") in
  let data = Bytes.cat a b in
  let r = Imgmeta.Reader.of_bytes data in
  let found = ref [] in
  Imgmeta.Formats.Isobmff.walk_top r (fun box ->
    found := box.Imgmeta.Formats.Isobmff.kind :: !found);
  Alcotest.(check (list string)) "two boxes" [ "free"; "ftyp" ] !found
;;

let test_isobmff_find_top () =
  let a = isobmff_box "ftyp" (Bytes.of_string "heic") in
  let b = isobmff_box "free" (Bytes.of_string "xx") in
  let data = Bytes.cat a b in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Isobmff.find_top r "free" with
  | None -> Alcotest.fail "expected free box"
  | Some box -> Alcotest.(check string) "found free" "free" box.kind
;;

let isobmff_full_box ty body =
  let inner = Bytes.create (4 + Bytes.length body) in
  Bytes.fill inner 0 4 '\x00';
  Bytes.blit body 0 inner 4 (Bytes.length body);
  isobmff_box ty inner
;;

let ispe_body ~width ~height =
  let b = Bytes.create 8 in
  Bytes.set_int32_be b 0 (Int32.of_int width);
  Bytes.set_int32_be b 4 (Int32.of_int height);
  b
;;

let pixi_body ~depth =
  let b = Bytes.create 4 in
  Bytes.set_uint8 b 0 3;
  Bytes.set_uint8 b 1 depth;
  Bytes.set_uint8 b 2 depth;
  Bytes.set_uint8 b 3 depth;
  b
;;

let heif_file ~width ~height ~depth =
  let ftyp = isobmff_box "ftyp" (Bytes.of_string "heic\x00\x00\x00\x00mif1") in
  let ispe = isobmff_full_box "ispe" (ispe_body ~width ~height) in
  let pixi = isobmff_full_box "pixi" (pixi_body ~depth) in
  let ipco = isobmff_box "ipco" (Bytes.cat ispe pixi) in
  let iprp = isobmff_box "iprp" ipco in
  let meta = isobmff_full_box "meta" iprp in
  Bytes.cat ftyp meta
;;

let heif_file_with_irot ~width ~height ~depth ~irot =
  let ftyp = isobmff_box "ftyp" (Bytes.of_string "heic\x00\x00\x00\x00mif1") in
  let ispe = isobmff_full_box "ispe" (ispe_body ~width ~height) in
  let pixi = isobmff_full_box "pixi" (pixi_body ~depth) in
  let irot_box = isobmff_box "irot" (Bytes.make 1 (Char.chr (irot land 0x3))) in
  let ipco = isobmff_box "ipco" (Bytes.cat (Bytes.cat ispe pixi) irot_box) in
  let iprp = isobmff_box "iprp" ipco in
  let meta = isobmff_full_box "meta" iprp in
  Bytes.cat ftyp meta
;;

let test_heif_irot_swap () =
  let data = heif_file_with_irot ~width:1920 ~height:1080 ~depth:8 ~irot:1 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Heif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "swapped width" 1080 m.width;
    Alcotest.(check int) "swapped height" 1920 m.height;
    Alcotest.(check int) "orientation" 8 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_heif_irot_180 () =
  let data = heif_file_with_irot ~width:1920 ~height:1080 ~depth:8 ~irot:2 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Heif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "unswapped width" 1920 m.width;
    Alcotest.(check int) "orientation" 3 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let iinf_body entries =
  let inner = Buffer.create 16 in
  let count_bytes = Bytes.create 2 in
  Bytes.set_uint16_be count_bytes 0 (List.length entries);
  Buffer.add_bytes inner count_bytes;
  List.iter
    (fun (id, ty, name) ->
       let payload = Buffer.create 16 in
       let v_and_flags = Bytes.create 4 in
       Bytes.set_uint8 v_and_flags 0 2;
       Buffer.add_bytes payload v_and_flags;
       let id_bytes = Bytes.create 2 in
       Bytes.set_uint16_be id_bytes 0 id;
       Buffer.add_bytes payload id_bytes;
       Buffer.add_bytes payload (Bytes.create 2);
       Buffer.add_string payload ty;
       Buffer.add_string payload name;
       Buffer.add_char payload '\x00';
       Buffer.add_bytes inner (isobmff_box "infe" (Buffer.to_bytes payload)))
    entries;
  Buffer.to_bytes inner
;;

let iloc_body_v0 ~items =
  let buf = Buffer.create 32 in
  Buffer.add_char buf (Char.chr 0x44);
  Buffer.add_char buf '\x00';
  let count_bytes = Bytes.create 2 in
  Bytes.set_uint16_be count_bytes 0 (List.length items);
  Buffer.add_bytes buf count_bytes;
  List.iter
    (fun (id, offset, length) ->
       let id_bytes = Bytes.create 2 in
       Bytes.set_uint16_be id_bytes 0 id;
       Buffer.add_bytes buf id_bytes;
       Buffer.add_bytes buf (Bytes.create 2);
       let ec_bytes = Bytes.create 2 in
       Bytes.set_uint16_be ec_bytes 0 1;
       Buffer.add_bytes buf ec_bytes;
       let off_bytes = Bytes.create 4 in
       Bytes.set_int32_be off_bytes 0 (Int32.of_int offset);
       Buffer.add_bytes buf off_bytes;
       let len_bytes = Bytes.create 4 in
       Bytes.set_int32_be len_bytes 0 (Int32.of_int length);
       Buffer.add_bytes buf len_bytes)
    items;
  Buffer.to_bytes buf
;;

let heif_file_with_exif_item ~width ~height ~depth ~orientation =
  let ftyp = isobmff_box "ftyp" (Bytes.of_string "heic\x00\x00\x00\x00mif1") in
  let ispe = isobmff_full_box "ispe" (ispe_body ~width ~height) in
  let pixi = isobmff_full_box "pixi" (pixi_body ~depth) in
  let ipco = isobmff_box "ipco" (Bytes.cat ispe pixi) in
  let iprp = isobmff_box "iprp" ipco in
  let iinf = isobmff_full_box "iinf" (iinf_body [ 2, "Exif", "Exif" ]) in
  let tiff = tiff_with_entries ~endian:`LE [ 0x0112, 3, 1, orientation ] in
  let exif_payload = Bytes.cat (Bytes.create 4) tiff in
  let exif_len = Bytes.length exif_payload in
  let placeholder = isobmff_full_box "iloc" (iloc_body_v0 ~items:[ 2, 0, exif_len ]) in
  let meta_without_iloc = Bytes.cat iinf iprp in
  let meta_iloc_offset = Bytes.length ftyp + 8 + 4 + Bytes.length meta_without_iloc in
  let exif_offset = meta_iloc_offset + Bytes.length placeholder in
  let iloc = isobmff_full_box "iloc" (iloc_body_v0 ~items:[ 2, exif_offset, exif_len ]) in
  let meta = isobmff_full_box "meta" (Bytes.cat meta_without_iloc iloc) in
  Bytes.cat (Bytes.cat ftyp meta) exif_payload
;;

let test_heif_exif_item_swap () =
  let data = heif_file_with_exif_item ~width:1920 ~height:1080 ~depth:8 ~orientation:6 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Heif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "swapped width" 1080 m.width;
    Alcotest.(check int) "swapped height" 1920 m.height;
    Alcotest.(check int) "orientation" 6 m.orientation
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_heif_synthesized () =
  let data = heif_file ~width:1920 ~height:1080 ~depth:10 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Heif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 1920 m.width;
    Alcotest.(check int) "height" 1080 m.height;
    Alcotest.(check int) "depth" 10 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_heif_fixture () =
  let data = load_bytes "fixture.heic" in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Heif.read_metadata r with
  | Error e -> Alcotest.failf "fixture.heic failed %a" Imgmeta.pp_error e
  | Ok m ->
    Alcotest.(check int) "width" 320 m.width;
    Alcotest.(check int) "height" 320 m.height;
    Alcotest.(check int) "depth" 8 m.depth
;;

let avif_file ~width ~height ~depth =
  let ftyp = isobmff_box "ftyp" (Bytes.of_string "avif\x00\x00\x00\x00mif1") in
  let ispe = isobmff_full_box "ispe" (ispe_body ~width ~height) in
  let pixi = isobmff_full_box "pixi" (pixi_body ~depth) in
  let ipco = isobmff_box "ipco" (Bytes.cat ispe pixi) in
  let iprp = isobmff_box "iprp" ipco in
  let meta = isobmff_full_box "meta" iprp in
  Bytes.cat ftyp meta
;;

let test_avif_synthesized () =
  let data = avif_file ~width:800 ~height:600 ~depth:10 in
  let r = Imgmeta.Reader.of_bytes data in
  match Imgmeta.Formats.Avif.read_metadata r with
  | Ok m ->
    Alcotest.(check int) "width" 800 m.width;
    Alcotest.(check int) "height" 600 m.height;
    Alcotest.(check int) "depth" 10 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_public_of_bytes () =
  let data = png_header ~width:10 ~height:20 ~depth:8 ~color_type:2 in
  match Imgmeta.of_bytes data with
  | Ok m -> Alcotest.(check int) "width" 10 m.width
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_public_of_bytes_exn_raises () =
  let raised =
    try
      let _ = Imgmeta.of_bytes_exn (Bytes.of_string "garbage") in
      false
    with
    | Imgmeta_error _ -> true
  in
  Alcotest.(check bool) "raises" true raised
;;

let test_public_of_file () =
  match Imgmeta.of_file "fixture.png" with
  | Ok m ->
    Alcotest.(check int) "w" 320 m.width;
    Alcotest.(check int) "h" 320 m.height;
    Alcotest.(check int) "d" 8 m.depth
  | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e
;;

let test_public_of_in_channel () =
  In_channel.with_open_bin "fixture.heic" (fun ic ->
    match Imgmeta.of_in_channel ic with
    | Ok m ->
      Alcotest.(check int) "w" 320 m.width;
      Alcotest.(check int) "h" 320 m.height
    | Error e -> Alcotest.failf "%a" Imgmeta.pp_error e)
;;

let test_public_detect_format () =
  let data = png_header ~width:1 ~height:1 ~depth:8 ~color_type:2 in
  Alcotest.(check (option string))
    "detects png"
    (Some "png")
    (Option.map Imgmeta.format_to_string (Imgmeta.detect_format data))
;;

let equal_meta a b =
  a.Imgmeta.format = b.Imgmeta.format
  && a.width = b.width
  && a.height = b.height
  && a.depth = b.depth
  && a.orientation = b.orientation
;;

let load_three_from_path path =
  let from_file = Imgmeta.of_file_exn path in
  let from_bytes = Imgmeta.of_bytes_exn (load_bytes path) in
  let from_chan = In_channel.with_open_bin path Imgmeta.of_in_channel_exn in
  from_file, from_bytes, from_chan
;;

let check_three_equal path =
  let a, b, c = load_three_from_path path in
  Alcotest.(check bool) "file equals bytes" true (equal_meta a b);
  Alcotest.(check bool) "file equals chan" true (equal_meta a c)
;;

let test_cross_source_png () = check_three_equal "fixture.png"
let test_cross_source_jpeg () = check_three_equal "fixture.jpeg"
let test_cross_source_heic () = check_three_equal "fixture.heic"

let expect_error name data =
  match Imgmeta.of_bytes data with
  | Ok _ -> Alcotest.failf "%s expected error but got ok" name
  | Error _ -> ()
;;

let test_negative_unknown () =
  expect_error "garbage" (Bytes.of_string "this is not an image at all")
;;

let test_negative_truncated_png () =
  let data = png_header ~width:1 ~height:1 ~depth:8 ~color_type:2 in
  let short = Bytes.sub data 0 10 in
  expect_error "png truncated" short
;;

let test_negative_malformed_jpeg () =
  expect_error "jpeg bad marker" (Bytes.of_string "\xff\xd8\xab\xcd")
;;

let test_negative_truncated_webp () =
  expect_error "webp truncated" (Bytes.of_string "RIFF\x00")
;;

let test_negative_truncated_heif () =
  expect_error "heif truncated" (Bytes.of_string "\x00\x00\x00\x18ftyphei")
;;

let () =
  Alcotest.run
    "imgmeta"
    [ ( "types"
      , [ Alcotest.test_case "format constructors" `Quick test_format_construction
        ; Alcotest.test_case "record fields" `Quick test_record_fields
        ; Alcotest.test_case "pp_error unknown_format" `Quick test_pp_error_unknown
        ; Alcotest.test_case "exception carries error" `Quick test_exception_carries_error
        ] )
    ; ( "reader"
      , [ Alcotest.test_case "bytes read" `Quick test_reader_bytes_read
        ; Alcotest.test_case "bytes read_at" `Quick test_reader_bytes_read_at
        ; Alcotest.test_case "bytes size" `Quick test_reader_bytes_size
        ; Alcotest.test_case "bytes truncated" `Quick test_reader_bytes_truncated
        ; Alcotest.test_case "file read" `Quick test_reader_file_read
        ; Alcotest.test_case "in_channel read" `Quick test_reader_in_channel_read
        ] )
    ; ( "magic"
      , [ Alcotest.test_case "png" `Quick test_magic_png
        ; Alcotest.test_case "jpeg" `Quick test_magic_jpeg
        ; Alcotest.test_case "gif" `Quick test_magic_gif
        ; Alcotest.test_case "webp" `Quick test_magic_webp
        ; Alcotest.test_case "heif" `Quick test_magic_heif
        ; Alcotest.test_case "avif" `Quick test_magic_avif
        ; Alcotest.test_case "unknown" `Quick test_magic_unknown
        ] )
    ; ( "png"
      , [ Alcotest.test_case "synthesized 8 bit rgb" `Quick test_png_synthesized
        ; Alcotest.test_case "synthesized 16 bit rgba" `Quick test_png_synthesized_16bit
        ; Alcotest.test_case "fixture 320x320 rgba" `Quick test_png_fixture
        ; Alcotest.test_case "orientation 6 swap" `Quick test_png_orientation_swap
        ; Alcotest.test_case "orientation 3 no swap" `Quick test_png_orientation_no_swap
        ] )
    ; "gif", [ Alcotest.test_case "synthesized 64x48" `Quick test_gif_synthesized ]
    ; ( "jpeg"
      , [ Alcotest.test_case "synthesized 320x240" `Quick test_jpeg_synthesized
        ; Alcotest.test_case "fixture 320x320 with exif" `Quick test_jpeg_fixture
        ; Alcotest.test_case "orientation 6 swap" `Quick test_jpeg_orientation_swap
        ; Alcotest.test_case "orientation 2 no swap" `Quick test_jpeg_orientation_no_swap
        ] )
    ; ( "webp"
      , [ Alcotest.test_case "synthesized vp8x 512x256" `Quick test_webp_vp8x
        ; Alcotest.test_case "synthesized vp8l 16x16" `Quick test_webp_vp8l
        ; Alcotest.test_case "orientation 6 swap" `Quick test_webp_orientation_swap
        ; Alcotest.test_case "orientation 2 no swap" `Quick test_webp_orientation_no_swap
        ] )
    ; ( "isobmff"
      , [ Alcotest.test_case "walk top level" `Quick test_isobmff_walk_top_level
        ; Alcotest.test_case "find top" `Quick test_isobmff_find_top
        ] )
    ; ( "heif"
      , [ Alcotest.test_case "synthesized 1920x1080 10bit" `Quick test_heif_synthesized
        ; Alcotest.test_case "fixture 320x320 8bit" `Quick test_heif_fixture
        ; Alcotest.test_case "irot 1 90 ccw swap" `Quick test_heif_irot_swap
        ; Alcotest.test_case "irot 2 180 no swap" `Quick test_heif_irot_180
        ; Alcotest.test_case "exif item swap" `Quick test_heif_exif_item_swap
        ] )
    ; ( "avif"
      , [ Alcotest.test_case "synthesized 800x600 10bit" `Quick test_avif_synthesized ] )
    ; ( "exif"
      , [ Alcotest.test_case "le orientation 6" `Quick test_exif_orientation_le
        ; Alcotest.test_case "be orientation 8" `Quick test_exif_orientation_be
        ; Alcotest.test_case "missing tag" `Quick test_exif_missing_orientation
        ; Alcotest.test_case "invalid magic" `Quick test_exif_invalid_magic
        ; Alcotest.test_case "empty bytes" `Quick test_exif_empty
        ; Alcotest.test_case "out of range" `Quick test_exif_out_of_range
        ] )
    ; ( "public"
      , [ Alcotest.test_case "of_bytes png" `Quick test_public_of_bytes
        ; Alcotest.test_case "of_bytes_exn raises" `Quick test_public_of_bytes_exn_raises
        ; Alcotest.test_case "of_file fixture png" `Quick test_public_of_file
        ; Alcotest.test_case "of_in_channel fixture heic" `Quick test_public_of_in_channel
        ; Alcotest.test_case "detect_format png" `Quick test_public_detect_format
        ] )
    ; ( "cross_source"
      , [ Alcotest.test_case "png all three equal" `Quick test_cross_source_png
        ; Alcotest.test_case "jpeg all three equal" `Quick test_cross_source_jpeg
        ; Alcotest.test_case "heic all three equal" `Quick test_cross_source_heic
        ] )
    ; ( "negative"
      , [ Alcotest.test_case "unknown garbage" `Quick test_negative_unknown
        ; Alcotest.test_case "png truncated" `Quick test_negative_truncated_png
        ; Alcotest.test_case "jpeg malformed marker" `Quick test_negative_malformed_jpeg
        ; Alcotest.test_case "webp truncated" `Quick test_negative_truncated_webp
        ; Alcotest.test_case "heif truncated" `Quick test_negative_truncated_heif
        ] )
    ]
;;
