open Imgmeta

let test_format_construction () =
  Alcotest.(check int)
    "six format constructors"
    6
    (List.length [ PNG; JPEG; GIF; WebP; HEIF; AVIF ])
;;

let test_record_fields () =
  let m = { format = PNG; width = 10; height = 20; depth = 8 } in
  Alcotest.(check int) "width" 10 m.width;
  Alcotest.(check int) "height" 20 m.height;
  Alcotest.(check int) "depth" 8 m.depth
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

let png_header ~width ~height ~depth ~color_type =
  let buf = Buffer.create 64 in
  Buffer.add_string buf "\x89PNG\r\n\x1a\n";
  let chunk ty body =
    let len = Bytes.length body in
    let len_bytes = Bytes.create 4 in
    Bytes.set_int32_be len_bytes 0 (Int32.of_int len);
    Buffer.add_bytes buf len_bytes;
    Buffer.add_string buf ty;
    Buffer.add_bytes buf body
  in
  let ihdr = Bytes.create 13 in
  Bytes.set_int32_be ihdr 0 (Int32.of_int width);
  Bytes.set_int32_be ihdr 4 (Int32.of_int height);
  Bytes.set_uint8 ihdr 8 depth;
  Bytes.set_uint8 ihdr 9 color_type;
  Bytes.set_uint8 ihdr 10 0;
  Bytes.set_uint8 ihdr 11 0;
  Bytes.set_uint8 ihdr 12 0;
  chunk "IHDR" ihdr;
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
        ] )
    ]
;;
