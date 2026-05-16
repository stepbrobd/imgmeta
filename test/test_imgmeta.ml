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
  let chunk = Imgmeta.Reader.read r 3 in
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
      let _ = Imgmeta.Reader.read r 10 in
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
      let head = Imgmeta.Reader.read r 5 in
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
        let chunk = Imgmeta.Reader.read r 3 in
        Alcotest.(check string) "head" "abc" (Bytes.to_string chunk)))
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
    ]
;;
