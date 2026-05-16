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

let () =
  Alcotest.run
    "imgmeta"
    [ ( "types"
      , [ Alcotest.test_case "format constructors" `Quick test_format_construction
        ; Alcotest.test_case "record fields" `Quick test_record_fields
        ; Alcotest.test_case "pp_error unknown_format" `Quick test_pp_error_unknown
        ; Alcotest.test_case "exception carries error" `Quick test_exception_carries_error
        ] )
    ]
;;
